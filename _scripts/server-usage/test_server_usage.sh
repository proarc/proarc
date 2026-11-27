#!/usr/bin/env bash
# Integration tests using real Linux df, jq, flock and temporary files.
set -euo pipefail
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
script="$script_dir/server_usage.sh"
root=$(mktemp -d)
trap 'rm -rf -- "$root"' EXIT
output="$root/output with spaces/.proarc"
config="$root/disks.json"
actual="$output/server-usage-actual.json"
history="$output/server-usage-historical.json"

run() { bash "$script" --config "$config" --output-dir "$output" "$@"; }
expect_failure() {
    local code=0
    "$@" >"$root/stdout" 2>"$root/stderr" || code=$?
    [[ "$code" == 1 ]] || { printf 'Expected exit 1, got %s\n' "$code" >&2; exit 1; }
}
pass() { printf 'PASS: %s\n' "$1"; }

# Whitespace, quotes, backslashes, Unicode and trailing newlines must survive JSON.
special_path="$root/archiv"$'\n'
mkdir -- "$special_path"
name=$'Archiv Žluťoučký "disk" \\ záloha\t\n'
jq -n --arg path "$special_path" --arg name "$name" --arg root "$root" \
    '{disks:[{id:"archive",name:$name,path:$path},{id:"system",name:"System",path:$root}]}' \
    >"$config"
cp -- "$config" "$root/valid-config"
run
jq -e --arg name "$name" --arg path "$special_path" '
    .schemaVersion == 1 and (.disks | length) == 2
    and .disks[0].name == $name and .disks[0].path == $path
    and all(.disks[]; .totalBytes > 0 and .usedBytes >= 0 and .error == null)
    and (.measuredAt | test("\\+00:00$"))
' "$actual" >/dev/null
[[ ! -e "$history" ]]
read -r total _ _ < <(df --block-size=1 --output=size,used,avail -- "$root" | tail -n 1)
# Capacity should match df; used/free may change while the test writes files.
jq -e --argjson total "$total" '.disks[0].totalBytes == $total' "$actual" >/dev/null
pass 'multiple disks, real filesystem capacity and JSON escaping'

run --historical
run --historical
jq -e '(.samples | length) == 1 and .schemaVersion == 1' "$history" >/dev/null
cp -- "$history" "$root/original-history"
run
cmp -- "$history" "$root/original-history"
# Seed another day, then verify both days remain after today's update.
jq '.samples += [(.samples[0] | .date = "2000-01-01")]' "$history" >"$root/seed"
mv -- "$root/seed" "$history"
run --historical
jq -e '(.samples | length) == 2 and .samples[0].date == "2000-01-01"' "$history" >/dev/null
pass 'daily replacement, earlier dates retained, actual leaves history unchanged'

cp -- "$actual" "$root/original-actual"
for invalid in \
    '{"disks":[]}' \
    '{"disks":[{"id":"x","name":"X","path":"relative"}]}' \
    '{"disks":[{"id":"x","name":"X","path":"/"},{"id":"x","name":"Y","path":"/"}]}' \
    '{"disks":[{"id":"x","name":null,"path":"/"}]}' \
    '{"disks":[{"id":"x","name":"X","path":"/tmp/\u0000"}]}' \
    '{"disks":[{"id":"x","name":"X","path":"/"}]} {}'; do
    printf '%s\n' "$invalid" >"$config"
    expect_failure run
    cmp -- "$actual" "$root/original-actual"
done
cp -- "$root/valid-config" "$config"
pass 'invalid configuration rejected without replacing actual'

cp -- "$history" "$root/valid-history"
for invalid in '{broken' '{"schemaVersion":2,"samples":[]}' '{"schemaVersion":1,"samples":[null]}'; do
    printf '%s\n' "$invalid" >"$history"
    cp -- "$history" "$root/broken-history"
    expect_failure run --historical
    cmp -- "$history" "$root/broken-history"
    cmp -- "$actual" "$root/original-actual"
done
cp -- "$root/valid-history" "$history"
pass 'corrupt history preserved and actual not replaced'

jq --arg path "$root/missing" '.disks += [{id:"missing",name:"Missing",path:$path}]' \
    "$root/valid-config" >"$config"
expect_failure run --historical
jq -e '.disks[0].totalBytes > 0 and .disks[2].totalBytes == null
       and .disks[2].usedBytes == null and .disks[2].freeBytes == null
       and (.disks[2].error | length) > 0' "$actual" >/dev/null
cp -- "$root/valid-config" "$config"
pass 'missing disk records an error while successful disks are saved'

cp -- "$actual" "$root/locked-actual"
expect_failure flock "$output/.server-usage.lock" bash "$script" --config "$config" --output-dir "$output"
cmp -- "$actual" "$root/locked-actual"
run
pass 'concurrent writer rejected, lock released after exit'

chmod 0640 -- "$actual"
run
[[ $(stat -c '%a' -- "$actual") == 640 ]]
pass 'existing read permissions preserved'

# Simulate failure at the final rename; old file must survive and temps be removed.
cp -- "$actual" "$root/before-failure"
mkdir -- "$root/bin"
printf '#!/bin/bash\nexit 1\n' >"$root/bin/mv"
chmod +x -- "$root/bin/mv"
expect_failure env PATH="$root/bin:$PATH" bash "$script" --config "$config" --output-dir "$output"
cmp -- "$actual" "$root/before-failure"
for entry in "$output"/.server-usage.????????; do [[ ! -d "$entry" ]]; done
pass 'failed publication preserves original and cleans temporary files'

printf 'All 8 integration tests passed.\n'
