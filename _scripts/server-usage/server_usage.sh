#!/usr/bin/env bash
# Linux filesystem capacity collector for ProArc. Requires Bash 4.4+, jq,
# GNU coreutils (including df) and util-linux flock.
set -euo pipefail

die() {
    printf 'server-usage: %s\n' "$*" >&2
    exit 1
}

usage() {
    printf '%s\n' \
        'Usage: bash server_usage.sh --config FILE [--output-dir DIR] [--historical]' \
        'Default output directory: ~/.proarc' \
        '--historical also saves/replaces the sample for the current local date.'
}

config=''
output_dir="${HOME:?HOME must be set}/.proarc"
historical=false
while (($#)); do
    case "$1" in
        --config|--output-dir)
            if (($# < 2)); then die "Missing value for $1"; fi
            [[ -n "$2" ]] || die "Missing value for $1"
            if [[ "$1" == --config ]]; then config=$2; else output_dir=$2; fi
            shift 2
            ;;
        --historical) historical=true; shift ;;
        --help|-h) usage; exit 0 ;;
        *) die "Unknown argument: $1" ;;
    esac
done
[[ -n "$config" ]] || { usage >&2; die '--config is required'; }
((BASH_VERSINFO[0] > 4 || (BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] >= 4))) \
    || die 'Bash 4.4 or newer is required'
for tool in jq df flock date mktemp chmod mv rm rmdir mkdir; do
    command -v "$tool" >/dev/null || die "Required command not found: $tool"
done

mkdir -p -- "$output_dir"
# Never remove the lock file: another process might already have it open.
exec 9>>"$output_dir/.server-usage.lock"
flock -n 9 || die 'Another collector is writing to this output directory'

work=$(mktemp -d -- "$output_dir/.server-usage.XXXXXXXX")
# Called indirectly by the EXIT trap.
# shellcheck disable=SC2317
cleanup() {
    rm -f -- "$work"/*
    rmdir -- "$work"
}
trap cleanup EXIT
trap 'exit 1' HUP INT TERM

# Slurp rejects multiple JSON documents. NUL is forbidden because Bash uses it
# as the delimiter when reading fields; other whitespace is preserved.
jq -cs '
    if length != 1 then error("Expected one configuration object") else .[0] end
    | if type != "object" then error("Expected a configuration object") else . end
    | if (.disks | type) != "array" then error("Expected a disks array") else . end
    | if (.disks | length) == 0 then error("At least one disk is required") else . end
    | if all(.disks[];
        type == "object"
        and all(.id, .name, .path;
            type == "string" and test("\\S") and (explode | index(0) == null))
        and (.path | startswith("/")))
      then . else error("Each disk needs non-empty id, name and absolute path strings") end
    | if ([.disks[].id] | length) != ([.disks[].id] | unique | length)
      then error("Duplicate disk id") else . end
    | .disks[] | {id, name, path}
' -- "$config" >"$work/config" || die 'Invalid disk configuration'

# Compute UTC time and local date from the same instant, including at midnight.
epoch=$(date +%s)
measured_at=$(date -u -d "@$epoch" '+%Y-%m-%dT%H:%M:%S+00:00')
sample_date=$(date -d "@$epoch" '+%Y-%m-%d')
failed=0
: >"$work/disks"
while IFS= read -r disk; do
    mapfile -d '' -t fields < <(jq -j '.id, "\u0000", .path, "\u0000"' <<<"$disk")
    id=${fields[0]}
    path=${fields[1]}
    error=''
    if [[ ! -d "$path" ]]; then
        error='Disk path is not an accessible directory'
    elif stats=$(LC_ALL=C df --block-size=1 --output=size,used,avail -- "$path" 2>"$work/df-error"); then
        read -r total used free <<<"${stats##*$'\n'}"
        if [[ ! "$total" =~ ^[0-9]+$ || ! "$used" =~ ^[0-9]+$ || ! "$free" =~ ^-?[0-9]+$ ]]; then
            error='Invalid capacity values returned by df'
        fi
    else
        error=$(<"$work/df-error")
        error=${error:-'Cannot measure filesystem capacity'}
    fi
    if [[ -n "$error" ]]; then
        printf '%s: %s\n' "$id" "$error" >&2
        jq -c --arg error "$error" \
            '. + {totalBytes:null, usedBytes:null, freeBytes:null, error:$error}' \
            <<<"$disk" >>"$work/disks"
        failed=1
    else
        jq -c --argjson total "$total" --argjson used "$used" --argjson free "$free" \
            '. + {totalBytes:$total, usedBytes:$used, freeBytes:$free, error:null}' \
            <<<"$disk" >>"$work/disks"
    fi
done <"$work/config"

jq -s --arg time "$measured_at" --arg date "$sample_date" \
    '{schemaVersion:1, measuredAt:$time, date:$date, disks:.}' \
    "$work/disks" >"$work/actual"

history_path="$output_dir/server-usage-historical.json"
if "$historical"; then
    history_input=$history_path
    if [[ ! -e "$history_path" ]]; then
        printf '%s\n' '{"schemaVersion":1,"samples":[]}' >"$work/history-input"
        history_input="$work/history-input"
    fi
    jq -s --slurpfile snapshot "$work/actual" '
        if length != 1 then error("Expected one historical object") else .[0] end
        | if type != "object" then error("Invalid history") else . end
        | if .schemaVersion != 1 or (.samples | type) != "array"
          then error("Unsupported historical format") else . end
        | if all(.samples[]; type == "object" and (.date | type) == "string")
          then . else error("Invalid historical samples") end
        | .samples = ([.samples[] | select(.date != $snapshot[0].date)]
                       + $snapshot | sort_by(.date))
    ' -- "$history_input" >"$work/history" || die 'Cannot update historical file; original preserved'
fi

publish() {
    local source=$1 target=$2
    if [[ -e "$target" ]]; then
        chmod --reference="$target" -- "$source"
    else
        chmod 0644 -- "$source"
    fi
    # Temp files live on the destination filesystem. -T rejects a directory target.
    mv -fT -- "$source" "$target"
}
if "$historical"; then publish "$work/history" "$history_path"; fi
publish "$work/actual" "$output_dir/server-usage-actual.json"
exit "$failed"
