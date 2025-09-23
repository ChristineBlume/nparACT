## Test environments
* Local R installation: R 4.4.2 (Windows 11, x86_64)
* macOS builder: r-devel-macosx-arm64 (macOS 13.3.1, Apple M1)
* win-builder: r-devel
* R-hub: Linux, macOS (Intel/ARM), Windows, Ubuntu (clang, gcc12, release, next)

## R-hub check results
No ERRORs, WARNINGs, or NOTEs.

## R CMD check results
0 ERRORs, 0 WARNINGs, 2 NOTEs:

* **checking for future file timestamps ... NOTE**  
  This appears to be spurious and is commonly reported by other package authors.

* **Possibly mis-spelled words in DESCRIPTION ... NOTE**  
  The flagged words (e.g. *Actigraphy, interdaily, ultradian, zeitgebers, et al.*) are domain-specific terms, author names, or standard Latin abbreviations. They are spelled correctly.

## Downstream dependencies
Reverse dependency checks were run; no issues found.

## Package archival and fixes
This submission is a resubmission of the nparACT package, which was previously archived on CRAN (2025-06-17) because earlier issues were not addressed in time.  
