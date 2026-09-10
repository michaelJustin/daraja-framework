<#
  Doxygen FILTER_PATTERNS script for *.pas files.

  Runs pas2dox (which turns the Pascal source into the C++-ish form Doxygen
  parses), but first rewrites Delphi's "strict private" / "strict protected"
  to plain "private" / "protected".

  pas2dox does not translate the "strict" visibility specifiers: it emits the
  members under the class's leading "public:" section, so every strict-private
  backing field (FStarted, FContext, CS, ...) ends up documented as a public
  attribute. Downgrading them to the non-strict specifiers lets pas2dox map
  them to "private:" / "protected:", which EXTRACT_PRIVATE = NO then hides.
#>
param([Parameter(Mandatory = $true)][string]$Path)

$ErrorActionPreference = 'Stop'
$pas2dox = Join-Path $PSScriptRoot 'pas2dox-0.50rc1.exe'

$source = Get-Content -Raw -LiteralPath $Path
$source = $source -replace '(?im)^([ \t]*)strict[ \t]+private\b',   '$1private'
$source = $source -replace '(?im)^([ \t]*)strict[ \t]+protected\b', '$1protected'

$tmp = Join-Path ([System.IO.Path]::GetTempPath()) ("djdox_" + [System.Guid]::NewGuid().ToString("N") + ".pas")
try {
    [System.IO.File]::WriteAllText($tmp, $source, (New-Object System.Text.UTF8Encoding($false)))
    [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding($false)
    & $pas2dox -s $tmp
}
finally {
    Remove-Item -LiteralPath $tmp -ErrorAction SilentlyContinue
}
