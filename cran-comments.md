## enmpa 0.2.6

This is an update of enmpa 0.2.5. Changes in this version:

* Removed the dependencies on 'snow', 'doSNOW' and 'foreach'. Parallel
  processing in `calibration_glm()` now uses 'parallel' and 'pbapply'.
* Bug fixes in `calibration_glm()` (see NEWS.md).

## Test environments
* macOS 27.2 (aarch64), R 4.5.2 (local)
* win-builder, R-devel
* macOS-latest, R release (GitHub Actions)
* Windows-latest, R release (GitHub Actions)
* Ubuntu-latest, R release (GitHub Actions)
* Ubuntu-latest, R devel (GitHub Actions)
* Ubuntu-latest, R oldrel-1 (GitHub Actions)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
There is one reverse dependency on CRAN: kuenm2. It uses `aux_var_comb()`,
`predict_glm()` and `plot_importance()`, which are unchanged in this release.
