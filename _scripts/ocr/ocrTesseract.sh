#!/bin/bash
set -euo pipefail

INPUT="/data/proarc/.proarc/users/proarc/import/ocrIn"
OUTPUT="/data/proarc/.proarc/users/proarc/import/ocrOut"

LOGFILE="/data/proarc/scripts/ocrTesseract.log"
LOCKFILE="/data/proarc/scripts/ocrTesseract.lock"

OCR_LANG="ces"
PDF_DPI=300
LIMIT=$((100 * 1024 * 1024))

is_folder_stable() {
    local folder="$1"
    local wait_time="${2:-60}"

    local snapshot1 snapshot2

    snapshot1=$(find "$folder" -type f -printf '%P %s %T@\n' | sort)
    sleep "$wait_time"
    snapshot2=$(find "$folder" -type f -printf '%P %s %T@\n' | sort)

    if [ "$snapshot1" = "$snapshot2" ]; then
        return 0  # stabilní
    else
        return 1  # mění se
    fi
}

##################################
# Převod PDF na jednotlivé TIFF stránky
##################################
convert_pdfs_to_tiff() {
    local folder="$1"
    local pdf filename base safe_base tmp_dir
    local page page_number target
    local -a pdfs pages targets

    mapfile -d '' pdfs < <(
        find "$folder" -maxdepth 1 -type f -iname '*.pdf' -print0 | sort -z -V
    )

    if [ "${#pdfs[@]}" -eq 0 ]; then
        return 0
    fi

    if ! command -v pdftoppm >/dev/null 2>&1; then
        echo "CHYBA: pdftoppm neni nainstalovan, PDF nelze prevest" >> "$LOGFILE"
        return 1
    fi

    for pdf in "${pdfs[@]}"; do
        filename=$(basename "$pdf")
        base="${filename%.*}"
        safe_base=$(printf '%s' "$base" | sed 's/[[:space:]]/_/g')

        if ! tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/ocr-pdf.XXXXXX"); then
            echo "CHYBA: nelze vytvorit docasny adresar pro $filename" >> "$LOGFILE"
            return 1
        fi

        echo "PDF -> TIFF: $filename" >> "$LOGFILE"

        if ! pdftoppm -r "$PDF_DPI" -cropbox -tiff -tiffcompression lzw \
            "$pdf" "$tmp_dir/page" >>"$LOGFILE" 2>&1; then
            echo "CHYBA prevodu PDF: $pdf" >> "$LOGFILE"
            rm -rf -- "$tmp_dir"
            return 1
        fi

        mapfile -d '' pages < <(
            find "$tmp_dir" -maxdepth 1 -type f -iname '*.tif' -print0 | sort -z -V
        )

        if [ "${#pages[@]}" -eq 0 ] || [ "${#pages[@]}" -gt 9999 ]; then
            echo "CHYBA: $filename ma neplatny pocet stran (${#pages[@]})" >> "$LOGFILE"
            rm -rf -- "$tmp_dir"
            return 1
        fi

        targets=()
        page_number=1
        for page in "${pages[@]}"; do
            printf -v target '%s/%s_%04d.tif' "$folder" "$safe_base" "$page_number"
            if [ -e "$target" ]; then
                echo "CHYBA: cilovy soubor jiz existuje ($target)" >> "$LOGFILE"
                rm -rf -- "$tmp_dir"
                return 1
            fi
            targets+=("$target")
            page_number=$((page_number + 1))
        done

        for page_number in "${!pages[@]}"; do
            mv -- "${pages[$page_number]}" "${targets[$page_number]}"
        done

        rm -rf -- "$tmp_dir"
        echo "PDF prevedeno: $filename (${#pages[@]} stran)" >> "$LOGFILE"
    done
}

##################################
# LOCK proti paralelnímu běhu
##################################
if [ -f "$LOCKFILE" ]; then
    echo "Script už běží" >> "$LOGFILE"
    exit 1
fi

trap "rm -f $LOCKFILE" EXIT
touch "$LOCKFILE"

echo "=== START $(date) ===" >> "$LOGFILE"

##################################
# zpracování složek s TIFF nebo PDF soubory
##################################
# Hloubka průchodu není omezená. mindepth 2 pouze vylučuje soubory
# uložené přímo v kořeni INPUT; složka na první úrovni má soubor na úrovni 2.
mapfile -d '' FOLDERS < <(
    find "$INPUT" -mindepth 2 -type f \( -iname '*.tif' -o -iname '*.pdf' \) -printf '%h\0' |
        sort -zu
)

for f in "${FOLDERS[@]}"; do
    echo "$f" >> "$LOGFILE"
done

for FOLDER in "${FOLDERS[@]}"; do

    RELATIVE_PATH="${FOLDER#"$INPUT"/}"
    OUTDIR="$OUTPUT/$RELATIVE_PATH"

    echo "----------------------------------" >> "$LOGFILE"
    echo "Složka: $RELATIVE_PATH" >> "$LOGFILE"

    echo "Kontrola stability: " >> "$LOGFILE"

    if ! is_folder_stable "$FOLDER" 30; then
        echo "SKIP: $RELATIVE_PATH se stále zapisuje" >> "$LOGFILE"
        continue
    fi

    if [ -d "$OUTDIR" ]; then
        echo "SKIP: výstup existuje ($OUTDIR)" >> "$LOGFILE"
        continue
    fi

    ##################################
    # 1. PŘEVOD PDF NA TIFF
    ##################################
    if ! convert_pdfs_to_tiff "$FOLDER"; then
        echo "SKIP: prevod PDF selhal ($RELATIVE_PATH)" >> "$LOGFILE"
        continue
    fi

    ##################################
    # 2. NAČTENÍ TIFF SEZNAMU
    ##################################
    mapfile -d '' FILES < <(find "$FOLDER" -maxdepth 1 -type f -iname '*.tif' -print0 | sort -z -V)

    ##################################
    # 3. FÁZE RESIZE
    ##################################

#	for file in "${FILES[@]}"; do
#        filename=$(basename "$file")
#        name="${filename%.tif}"
#        filesize=$(stat -c%s "$file")
#        if [ "$filesize" -gt "$LIMIT" ]; then
#            echo "INFO: resize $filename" >> "$LOGFILE"
#            tmp_file="$FOLDER/${name}_tmp.tif"
#            if convert "$file" -strip -quiet -resize 50% "$tmp_file"; then
#                mv "$tmp_file" "$file"
#                echo "RESIZE: $filename" >> "$LOGFILE"
#            else
#                echo "CHYBA resize: $file" >> "$LOGFILE"
#                rm -f "$tmp_file"
#            fi
#        fi
#    done


    ##################################
    # OCR pro všechny TIFF
    ##################################
    for file in "${FILES[@]}"; do

        filename=$(basename "$file")
        name="${filename%.tif}"

		output_txt="$FOLDER/$name.txt"

		# kontrola existence výstupu
		if [ -f "$output_txt" ]; then
			echo "SKIP: $filename -> výstup již existuje" >> "$LOGFILE"
			continue
		fi

        echo "OCR: $filename" >> "$LOGFILE"

        if tesseract -l "$OCR_LANG" "$file" "$FOLDER/$name" txt alto >>"$LOGFILE" 2>&1; then
            # odstranění posledního řádku TXT
            sed '$d' "$FOLDER/$name.txt" > "$FOLDER/$name.tmp" && mv "$FOLDER/$name.tmp" "$FOLDER/$name.txt"
        else
            echo "CHYBA OCR: $file" >> "$LOGFILE"
            continue
        fi
    done

    mkdir -p "$OUTDIR"

    ##################################
    # přesun dat
    ##################################
    while IFS= read -r -d '' file; do
        mv -- "$file" "$OUTDIR/"
    done < <(find "$FOLDER" -mindepth 1 -maxdepth 1 -type f -print0)

    ##################################
    # práva jen pro tuto složku
    ##################################
    chmod -R 755 "$OUTDIR"

    ##################################
    # odstranění pouze zpracované prázdné složky;
    # nadřazená struktura ve vstupu zůstává
    ##################################
    rmdir "$FOLDER" 2>/dev/null || true

    echo "HOTOVO: $RELATIVE_PATH" >> "$LOGFILE"

done

echo "=== HOTOVO $(date) ===" >> "$LOGFILE"
