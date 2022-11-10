First submission to CRAN

## Test environments

- Local Ubuntu 20.04 installation (R 4.2.2)
- GitHub Actions:
  - Windows (oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- r-hub:
  - Windows Server 2022, R-devel, 64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran


## R CMD check results

0 errors | 0 warnings | 0 notes

## Corrections after CRAN manual inspection

1) Issue: 
"As it stands, the the package estimates several..."
Please omit one "the".

- Fixed.

2) Issue:
"Add references in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>"

- Fixed

3) Issue:
"Unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}"

- Fixed.

4) Issue:
"Preferably, an ‘Authors@R’ would be used with ‘ctb’ roles for the
authors of such code"

- Fixed. The 'ctb' roles were added in the DESCRIPTION file.

5) Issue:
" In your LICENSE-file you claim "Ipea" to be the copyrightholder
but dont mention any copyrightholders in your Authors@R-field"

- Fixed.