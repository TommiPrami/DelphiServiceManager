<#
.SYNOPSIS
    Force source files to UTF-8 with BOM and CRLF line endings, recursively.

.DESCRIPTION
    Walks Root and every subdirectory, skipping any path that contains
    "thirdparty" or "3rdparty" (case-insensitive) and the .git folder.
    For each file whose extension is in -Extensions it normalises line endings
    to CRLF and re-saves the file as UTF-8 with a byte-order mark.

    Files that are already UTF-8 + BOM + CRLF are left untouched, so the script
    is idempotent and does not create needless Git churn or mtime changes.

    Content is assumed to be UTF-8 or ASCII. An existing BOM is honoured and
    replaced; a UTF-8 file without a BOM gets one.

.PARAMETER Root
    Folder to process. Defaults to the current directory.

.PARAMETER Extensions
    Extensions to touch (with or without a leading dot).
    Defaults to .pas, .inc, .dpr, .dpk, .dproj, .iss.

.PARAMETER ExcludePattern
    Regex matched against each file's full path; matches are skipped.
    Defaults to third-party folders.

.EXAMPLE
    .\Normalize-SourceEncoding.ps1

.EXAMPLE
    .\Normalize-SourceEncoding.ps1 -Root C:\git\MyProject -WhatIf

.EXAMPLE
    .\Normalize-SourceEncoding.ps1 -Extensions pas,inc,dpr,dpk,iss,isl -Verbose
#>
[CmdletBinding(SupportsShouldProcess)]
param(
    [string]   $Root           = '.',
    [string[]] $Extensions     = @('.pas', '.inc', '.dpr', '.dpk', '.dproj', '.iss'),
    [string]   $ExcludePattern = '(?i)(thirdparty|3rdparty)'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# Normalise the extension list to lower-case, each with a single leading dot.
$wantedExtensions = $Extensions | ForEach-Object { '.' + $_.TrimStart('.').ToLowerInvariant() }

$utf8WithBom    = [System.Text.UTF8Encoding]::new($true)   # emits a BOM
$utf8WithoutBom = [System.Text.UTF8Encoding]::new($false)  # for decoding

function Test-ByteArraysEqual([byte[]] $Left, [byte[]] $Right) {
    if ($Left.Length -ne $Right.Length) {
        return $false
    }

    for ($index = 0; $index -lt $Left.Length; $index++) {
        if ($Left[$index] -ne $Right[$index]) {
            return $false
        }
    }

    return $true
}

$convertedCount = 0
$compliantCount = 0
$skippedCount   = 0

Get-ChildItem -LiteralPath $Root -Recurse -File |
    Where-Object { $wantedExtensions -contains $_.Extension.ToLowerInvariant() } |
    ForEach-Object {
        $fullPath = $_.FullName

        if (($fullPath -match $ExcludePattern) -or ($fullPath -match '(?i)[\\/]\.git[\\/]')) {
            $skippedCount++
            Write-Verbose "Skipped (excluded): $fullPath"
            return
        }

        $originalBytes = [System.IO.File]::ReadAllBytes($fullPath)

        # ReadAllText strips a leading BOM if present and decodes as UTF-8.
        $text = [System.IO.File]::ReadAllText($fullPath, $utf8WithoutBom)

        # Normalise any mix of CRLF / lone CR / lone LF to CRLF.
        $text = $text -replace "`r`n", "`n"
        $text = $text -replace "`r", "`n"
        $text = $text -replace "`n", "`r`n"

        $newBytes = $utf8WithBom.GetPreamble() + $utf8WithBom.GetBytes($text)

        if (Test-ByteArraysEqual $originalBytes $newBytes) {
            $compliantCount++
            Write-Verbose "Already UTF-8 BOM + CRLF: $fullPath"
            return
        }

        if ($PSCmdlet.ShouldProcess($fullPath, 'Convert to UTF-8 (BOM) + CRLF')) {
            [System.IO.File]::WriteAllBytes($fullPath, $newBytes)
            $convertedCount++
            Write-Host "Converted: $fullPath"
        }
    }

Write-Host ''
Write-Host ("Converted: {0}   Already compliant: {1}   Skipped (third-party/.git): {2}" -f `
    $convertedCount, $compliantCount, $skippedCount)
