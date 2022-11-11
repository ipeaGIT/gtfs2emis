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
"Please always write package names, software names and API (application
programming interface) names in single quotes in title and description.
e.g: --> 'gtfs2emis'."

- Fixed.

2) Issue:
"It still seems like you have too many spaces in your description field.
Please remove them."

- Fixed