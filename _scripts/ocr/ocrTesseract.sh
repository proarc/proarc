#!/bin/bash
set -euo pipefail

INPUT="/data/.proarc/users/proarc/import/ocrIn"
OUTPUT="/data/.proarc/users/proarc/import/ocrOut"

LOGFILE="/home/proarc/_script/ocrTesseract.log"
LOCKFILE="/home/proarc/_script/ocrTesseract.lock"

OCR_LANG="ces"
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
# zpracování složek s TIFF soubory
##################################
# Hloubka průchodu není omezená. mindepth 2 pouze vylučuje TIFF soubory
# uložené přímo v kořeni INPUT; složka na první úrovni má soubor na úrovni 2.
mapfile -d '' FOLDERS < <(
    find "$INPUT" -mindepth 2 -type f -iname '*.tif' -printf '%h\0' |
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
    # 1. NAČTENÍ TIFF SEZNAMU (STABILNÍ)
    ##################################
    mapfile -d '' FILES < <(find "$FOLDER" -maxdepth 1 -type f -iname '*.tif' -print0 | sort -z -V)

    ##################################
    # 2. FÁZE RESIZE
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
