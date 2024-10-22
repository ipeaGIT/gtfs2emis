## Test environments

- Local Ubuntu 20.04 installation (R 4.4.1)
- GitHub Actions:
  - Windows (release, oldrel, devel)
  - MacOS (release, oldrel)
- r-hub:
  - Windows Server 2022, R-devel, 64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran


## R CMD check results

0 errors | 0 warnings | 0 notes

## Adjustments

- Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with `\donttest{}.`
Fixed.

- Please wrap examples that need packages in ‘Suggests’ in
if(requireNamespace("pkgname")){} instead.
Fixed.

- Found the following (possibly) invalid URLs: https://cetesb.sp.gov.br/veicular/
Fixed.
