$rscript = $null

$rVersions = Get-ChildItem 'C:\Program Files\R' -Directory -Filter 'R-*' -ErrorAction SilentlyContinue |
  Sort-Object Name -Descending

foreach ($version in $rVersions) {
  foreach ($candidate in @(
      (Join-Path $version.FullName 'bin\Rscript.exe'),
      (Join-Path $version.FullName 'bin\x64\Rscript.exe')
    )) {
    if (Test-Path $candidate) {
      $rscript = $candidate
      break
    }
  }

  if ($rscript) {
    break
  }
}

if (-not $rscript) {
  throw 'Rscript.exe was not found under C:\Program Files\R'
}

& $rscript (Join-Path $PSScriptRoot 'build_analysis.R')
exit $LASTEXITCODE
