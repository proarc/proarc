#Requires -Version 5.1
# Integration tests on Windows; no Pester or other packages are required.
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$scriptPath = Join-Path $PSScriptRoot 'server_usage.ps1'
$engine = Join-Path $PSHOME 'powershell.exe'
if (-not [IO.File]::Exists($engine)) { $engine = Join-Path $PSHOME 'pwsh.exe' }
$testRoot = Join-Path $PSScriptRoot ('.test-server-usage-' + [guid]::NewGuid())
[void][IO.Directory]::CreateDirectory($testRoot)
$config = Join-Path $testRoot 'disks.json'
$output = Join-Path $testRoot 'output with spaces\.proarc'
$actual = Join-Path $output 'server-usage-actual.json'
$historyPath = Join-Path $output 'server-usage-historical.json'
$utf8 = New-Object Text.UTF8Encoding($false)

function Assert-True([bool] $Condition, [string] $Message) {
    if (-not $Condition) { throw $Message }
}
function Write-Config($Disks) {
    $json = ConvertTo-Json -InputObject @{ disks = @($Disks) } -Depth 10
    [IO.File]::WriteAllText($config, $json, $utf8)
}
function Read-Json([string] $Path) {
    $options = @{}
    if ((Get-Command ConvertFrom-Json).Parameters.ContainsKey('DateKind')) { $options.DateKind = 'String' }
    ConvertFrom-Json -InputObject ([IO.File]::ReadAllText($Path, $utf8)) @options
}
function Invoke-Collector([switch] $Historical, [int] $Expected = 0) {
    $arguments = @('-NoProfile', '-NonInteractive', '-ExecutionPolicy', 'Bypass',
        '-File', ('"{0}"' -f $scriptPath), '-Config', ('"{0}"' -f $config),
        '-OutputDir', ('"{0}"' -f $output))
    if ($Historical) { $arguments += '-Historical' }
    $process = Start-Process -FilePath $engine -ArgumentList $arguments -WindowStyle Hidden -Wait -PassThru `
        -RedirectStandardOutput (Join-Path $testRoot 'stdout') -RedirectStandardError (Join-Path $testRoot 'stderr')
    $errorText = [IO.File]::ReadAllText((Join-Path $testRoot 'stderr'))
    Assert-True ($process.ExitCode -eq $Expected) "Expected exit $Expected, got $($process.ExitCode): $errorText"
}

try {
    $name = 'Archiv ' + [char]0x17d + ' "disk" \ backup' + "`t`n"
    $disks = @(
        @{ id = 'archive'; name = $name; path = $testRoot },
        @{ id = 'system'; name = 'System'; path = [IO.Path]::GetPathRoot($testRoot) }
    )
    Write-Config $disks
    Invoke-Collector
    $snapshot = Read-Json $actual
    Assert-True ($snapshot.schemaVersion -eq 1 -and $snapshot.disks.Count -eq 2) 'Expected two disks'
    Assert-True ($snapshot.disks[0].name -ceq $name) 'JSON escaping or Unicode lost'
    Assert-True ($snapshot.disks[0].totalBytes -gt 0 -and $null -eq $snapshot.disks[0].error) 'Capacity unavailable'
    $drive = New-Object IO.DriveInfo ([IO.Path]::GetPathRoot($testRoot))
    Assert-True ($snapshot.disks[0].totalBytes -eq $drive.TotalSize) 'Capacity differs from .NET DriveInfo'
    Assert-True ($snapshot.measuredAt -match '\+00:00$') 'Expected UTC timestamp'
    Assert-True (-not [IO.File]::Exists($historyPath)) 'Unexpected historical file'
    Write-Host 'PASS: multiple disks, actual capacity, JSON escaping and Unicode'

    Invoke-Collector -Historical
    Invoke-Collector -Historical
    $history = Read-Json $historyPath
    Assert-True ($history.samples.Count -eq 1) 'Same day was duplicated'
    $original = [IO.File]::ReadAllText($historyPath)
    Invoke-Collector
    Assert-True ([IO.File]::ReadAllText($historyPath) -ceq $original) 'Actual run changed history'
    $old = Read-Json $actual
    $old.date = '2000-01-01'
    $history.samples = @($old) + @($history.samples)
    [IO.File]::WriteAllText($historyPath, (ConvertTo-Json -InputObject $history -Depth 100), $utf8)
    Invoke-Collector -Historical
    $history = Read-Json $historyPath
    Assert-True ($history.samples.Count -eq 2 -and $history.samples[0].date -eq '2000-01-01') 'Earlier day lost'
    Write-Host 'PASS: daily replacement and earlier history retained'

    $original = [IO.File]::ReadAllText($actual)
    $invalidConfigs = @(
        '{"disks":[]}',
        '{"disks":[{"id":"x","name":"X","path":"relative"}]}',
        '{"disks":[{"id":"x","name":"X","path":"C:\\"},{"id":"x","name":"Y","path":"C:\\"}]}',
        '{"disks":[{"id":"x","name":null,"path":"C:\\"}]}',
        '{"disks":[{"id":"x","name":"X","path":"C:\\tmp\\\u0000"}]}',
        '{"disks":[{"id":"x","name":"X","path":"C:\\"}]} {}'
    )
    foreach ($invalid in $invalidConfigs) {
        [IO.File]::WriteAllText($config, $invalid, $utf8)
        Invoke-Collector -Expected 1
        Assert-True ([IO.File]::ReadAllText($actual) -ceq $original) 'Invalid config replaced actual'
    }
    Write-Config $disks
    Write-Host 'PASS: invalid configuration rejected without replacing actual'

    $validHistory = [IO.File]::ReadAllText($historyPath)
    foreach ($invalid in @('{broken', '{"schemaVersion":2,"samples":[]}', '{"schemaVersion":1,"samples":[null]}')) {
        [IO.File]::WriteAllText($historyPath, $invalid, $utf8)
        Invoke-Collector -Historical -Expected 1
        Assert-True ([IO.File]::ReadAllText($historyPath) -ceq $invalid) 'Corrupt history was replaced'
        Assert-True ([IO.File]::ReadAllText($actual) -ceq $original) 'Corrupt history caused actual to change'
    }
    [IO.File]::WriteAllText($historyPath, $validHistory, $utf8)
    Write-Host 'PASS: corrupt history preserved'

    Write-Config ($disks + @{ id = 'missing'; name = 'Missing'; path = (Join-Path $testRoot 'missing') })
    Invoke-Collector -Historical -Expected 1
    $snapshot = Read-Json $actual
    Assert-True ($snapshot.disks[0].totalBytes -gt 0) 'Successful disk was lost'
    $missing = $snapshot.disks[2]
    Assert-True ($null -eq $missing.totalBytes -and $null -eq $missing.usedBytes -and
        $null -eq $missing.freeBytes -and $missing.error.Length -gt 0) 'Missing disk should have error and null sizes'
    Write-Config $disks
    Write-Host 'PASS: missing disk records an error, successful disks retained'

    $original = [IO.File]::ReadAllText($actual)
    $lock = [IO.File]::Open((Join-Path $output '.server-usage.lock'), 'OpenOrCreate', 'ReadWrite', 'None')
    try { Invoke-Collector -Expected 1 } finally { $lock.Dispose() }
    Assert-True ([IO.File]::ReadAllText($actual) -ceq $original) 'Locked output was replaced'
    Invoke-Collector
    Write-Host 'PASS: concurrent writer rejected and lock released'

    $acl = (Get-Acl -LiteralPath $actual).Sddl
    $bytes = [IO.File]::ReadAllBytes($actual)
    Assert-True (-not ($bytes[0] -eq 239 -and $bytes[1] -eq 187 -and $bytes[2] -eq 191)) 'Expected UTF-8 without BOM'
    Invoke-Collector
    Assert-True ((Get-Acl -LiteralPath $actual).Sddl -ceq $acl) 'Destination ACL changed'
    Write-Host 'PASS: UTF-8 without BOM and destination ACL retained'

    # Deny replacing the destination while allowing it to be read.
    $original = [IO.File]::ReadAllText($actual)
    $reader = [IO.File]::Open($actual, 'Open', 'Read', 'Read')
    try { Invoke-Collector -Expected 1 } finally { $reader.Dispose() }
    Assert-True ([IO.File]::ReadAllText($actual) -ceq $original) 'Failed publication replaced actual'
    $leftovers = @(Get-ChildItem -LiteralPath $output -Filter '.server-usage.*.tmp' -Force)
    Assert-True ($leftovers.Count -eq 0) 'Temporary file was not cleaned'
    Invoke-Collector
    Write-Host 'PASS: failed publication preserves original and cleans temporary files'
    Write-Host 'All 8 integration tests passed.'
} finally {
    # Verify the absolute deletion target remains inside this script directory.
    $resolved = [IO.Path]::GetFullPath($testRoot)
    $parent = [IO.Path]::GetFullPath($PSScriptRoot).TrimEnd('\') + '\'
    if (-not $resolved.StartsWith($parent, [StringComparison]::OrdinalIgnoreCase) -or
        [IO.Path]::GetFileName($resolved) -notlike '.test-server-usage-*') {
        throw "Refusing to delete unexpected test path: $resolved"
    }
    Remove-Item -LiteralPath $resolved -Recurse -Force
}
