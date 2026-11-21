$ErrorActionPreference = "Stop"

$projectRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
$workerPath = Join-Path $PSScriptRoot "refresh_hotspots.R"
$rscript = (Get-Command Rscript -ErrorAction Stop).Source
$taskName = "KarhutlaProbstat-Hotspot-Daily-Refresh"

$action = New-ScheduledTaskAction `
    -Execute $rscript `
    -Argument ('"' + $workerPath + '"') `
    -WorkingDirectory $projectRoot
$trigger = New-ScheduledTaskTrigger -Daily -At "6:00AM"
$settings = New-ScheduledTaskSettingsSet `
    -StartWhenAvailable `
    -MultipleInstances IgnoreNew `
    -ExecutionTimeLimit (New-TimeSpan -Hours 2)
$principal = New-ScheduledTaskPrincipal `
    -UserId ([Security.Principal.WindowsIdentity]::GetCurrent().Name) `
    -LogonType Interactive `
    -RunLevel Limited

Register-ScheduledTask `
    -TaskName $taskName `
    -Action $action `
    -Trigger $trigger `
    -Settings $settings `
    -Principal $principal `
    -Description "Memperbarui snapshot hotspot NASA FIRMS untuk dashboard karhutla." `
    -Force | Out-Null

Write-Output "Jadwal harian hotspot terpasang pada pukul 06:00 untuk pengguna yang sedang login."
