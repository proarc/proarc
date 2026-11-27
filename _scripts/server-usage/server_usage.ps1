#Requires -Version 5.1
<#
.SYNOPSIS
Collect named Windows filesystem capacities for ProArc.
.EXAMPLE
 powershell.exe -NoProfile -File server_usage.ps1 -Config disks.windows.json -OutputDir C:\ProArc\.proarc -Historical
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Config,
    [string] $OutputDir = (Join-Path $HOME '.proarc'),
    [switch] $Historical
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Read-JsonObject([string] $Path) {
    $text = [IO.File]::ReadAllText($Path, [Text.Encoding]::UTF8)
    if (-not $text.TrimStart().StartsWith('{')) {
        throw "Expected a JSON object: $Path"
    }
    # PowerShell 7.5+ otherwise converts historical timestamps into DateTime.
    $options = @{}
    if ((Get-Command ConvertFrom-Json).Parameters.ContainsKey('DateKind')) {
        $options.DateKind = 'String'
    }
    $value = ConvertFrom-Json -InputObject $text @options
    if ($value -isnot [pscustomobject]) { throw "Expected a JSON object: $Path" }
    return $value
}

function Write-AtomicJson([string] $Path, [string] $Json) {
    $temporary = Join-Path ([IO.Path]::GetDirectoryName($Path)) ('.server-usage.' + [guid]::NewGuid() + '.tmp')
    try {
        $bytes = (New-Object Text.UTF8Encoding($false)).GetBytes($Json + "`n")
        $stream = [IO.File]::Open($temporary, [IO.FileMode]::CreateNew, [IO.FileAccess]::Write, [IO.FileShare]::None)
        try {
            $stream.Write($bytes, 0, $bytes.Length)
            $stream.Flush($true)
        } finally {
            $stream.Dispose()
        }
        if ([IO.File]::Exists($Path)) {
            # Replace atomically and preserve the destination's Windows ACL.
            [IO.File]::Replace($temporary, $Path, [System.Management.Automation.Language.NullString]::Value)
        } else {
            [IO.File]::Move($temporary, $Path)
        }
    } finally {
        if ([IO.File]::Exists($temporary)) { [IO.File]::Delete($temporary) }
    }
}

$lockStream = $null
$exitCode = 0
try {
    if ($env:OS -ne 'Windows_NT') { throw 'Use server_usage.sh on Linux; this script requires Windows.' }
    $configPath = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($Config)
    $settings = Read-JsonObject $configPath
    if ($settings.disks -isnot [array] -or $settings.disks.Count -eq 0) {
        throw 'Configuration must contain a non-empty disks array'
    }
    $ids = New-Object 'System.Collections.Generic.HashSet[string]' ([StringComparer]::Ordinal)
    foreach ($disk in $settings.disks) {
        if ($disk -isnot [pscustomobject]) { throw 'Each disk must be an object' }
        foreach ($key in @('id', 'name', 'path')) {
            $property = $disk.PSObject.Properties[$key]
            if ($null -eq $property -or $property.Value -isnot [string] -or
                [string]::IsNullOrWhiteSpace($property.Value) -or $property.Value.Contains([string][char]0)) {
                throw "Each disk needs a non-empty string without NUL: $key"
            }
        }
        if (-not $ids.Add($disk.id)) { throw "Duplicate disk id: $($disk.id)" }
        if ($disk.path -notmatch '^(?:[A-Za-z]:[\\/]|\\\\[^\\/]+[\\/][^\\/]+(?:[\\/]|$))') {
            throw "Disk path must be an absolute drive or UNC path: $($disk.path)"
        }
    }

    if (-not ('ProArc.ServerUsage.NativeDisk' -as [type])) {
        Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
namespace ProArc.ServerUsage {
    public static class NativeDisk {
        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool GetDiskFreeSpaceEx(
            string directory, out ulong available, out ulong total, out ulong free);
    }
}
'@
    }

    $outputPath = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($OutputDir)
    [void][IO.Directory]::CreateDirectory($outputPath)
    # Keep the lock file: FileShare.None rejects another Windows collector.
    $lockPath = Join-Path $outputPath '.server-usage.lock'
    $lockStream = [IO.File]::Open($lockPath, [IO.FileMode]::OpenOrCreate, [IO.FileAccess]::ReadWrite, [IO.FileShare]::None)
    $now = [DateTimeOffset]::Now
    $measurements = @(
        foreach ($disk in $settings.disks) {
            $entry = [ordered]@{
                id = $disk.id; name = $disk.name; path = $disk.path
                totalBytes = $null; usedBytes = $null; freeBytes = $null; error = $null
            }
            try {
                if (-not [IO.Directory]::Exists($disk.path)) {
                    throw 'Disk path is not an accessible directory'
                }
                [uint64]$availableBytes = 0
                [uint64]$totalBytes = 0
                [uint64]$freeBytes = 0
                # A trailing separator also allows UNC share roots.
                $probePath = $disk.path.TrimEnd([char[]]'\/') + '\'
                if (-not [ProArc.ServerUsage.NativeDisk]::GetDiskFreeSpaceEx(
                        $probePath, [ref]$availableBytes, [ref]$totalBytes, [ref]$freeBytes)) {
                    throw (New-Object ComponentModel.Win32Exception ([Runtime.InteropServices.Marshal]::GetLastWin32Error()))
                }
                $entry.totalBytes = $totalBytes
                $entry.usedBytes = [decimal]$totalBytes - [decimal]$freeBytes
                $entry.freeBytes = $availableBytes
            } catch {
                $entry.error = $_.Exception.Message
                [Console]::Error.WriteLine(('{0}: {1}' -f $disk.id, $entry.error))
                $exitCode = 1
            }
            [pscustomobject]$entry
        }
    )
    $snapshot = [pscustomobject][ordered]@{
        schemaVersion = 1
        measuredAt = $now.ToUniversalTime().ToString("yyyy-MM-dd'T'HH:mm:sszzz", [Globalization.CultureInfo]::InvariantCulture)
        date = $now.ToString('yyyy-MM-dd', [Globalization.CultureInfo]::InvariantCulture)
        disks = $measurements
    }
    $actualJson = ConvertTo-Json -InputObject $snapshot -Depth 100
    $historyPath = Join-Path $outputPath 'server-usage-historical.json'
    if ($Historical) {
        $samples = @()
        if ([IO.File]::Exists($historyPath)) {
            $history = Read-JsonObject $historyPath
            if ($history.schemaVersion -ne 1 -or $history.samples -isnot [array]) {
                throw 'Unsupported historical file format; original preserved'
            }
            foreach ($sample in $history.samples) {
                if ($sample -isnot [pscustomobject] -or $sample.date -isnot [string]) {
                    throw 'Invalid historical sample; original preserved'
                }
                if ($sample.date -cne $snapshot.date) { $samples += $sample }
            }
        }
        $samples += $snapshot
        $updated = [ordered]@{ schemaVersion = 1; samples = @($samples | Sort-Object -Property date) }
        $historyJson = ConvertTo-Json -InputObject $updated -Depth 100
        Write-AtomicJson $historyPath $historyJson
    }
    Write-AtomicJson (Join-Path $outputPath 'server-usage-actual.json') $actualJson
} catch {
    [Console]::Error.WriteLine(('server-usage: ' + $_.Exception.Message))
    $exitCode = 1
} finally {
    if ($null -ne $lockStream) { $lockStream.Dispose() }
}
exit $exitCode
