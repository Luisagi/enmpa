## This is a resubmission v0.1.5 01/12/23

### Comments by Benjamin Altmann on 30/11/23

The following issues have been corrected or commented here inline:

```
Found the following URLs which should use \doi (with the DOI name only):
    File 'niche_signal.Rd':
      https://doi.org/10.17161/bi.v17i.15985
    File 'plot_niche_signal.Rd':
      https://doi.org/10.17161/bi.v17i.15985
    File 'proc_enm.Rd':
      http://dx.doi.org/10.1016/j.ecolmodel.2007.11.008
```

ANSWER: Now the references have the correct format.

```
Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir(). -> R/short_helpers.R 
```

ANSWER: It is fixed. Now no default paths are defined in writing functions.

## Test environments
* Debian GNU/Linux 12, R 4.2.2 (local)
* MacOS 12.7.1, R release (GitHub Actions)
* Windows 10.0.20348, R release (GitHub Actions)
* Ubuntu 22.04.3 LTS, R release (GitHub Actions)
* Ubuntu 22.04.3 LTS, R devel (GitHub Actions)
* Ubuntu 22.04.3 LTS, R oldrel-1 (GitHub Actions)


## R CMD check results
There were no ERRORs:

There were no WARNINGs:

There were no NOTEs:


## Downstream dependencies
There are currently no downstream dependencies for this package. 


<br>
<hr>


## V 0.1.5 first submission

This is a new release.

## Test environments
* Debian GNU/Linux 12, R 4.2.2 (local)
* MacOS 12.7.1, R release (GitHub Actions)
* Windows 10.0.20348, R release (GitHub Actions)
* Ubuntu 22.04.3 LTS, R release (GitHub Actions)
* Ubuntu 22.04.3 LTS, R devel (GitHub Actions)
* Ubuntu 22.04.3 LTS, R oldrel-1 (GitHub Actions)


## R CMD check results
There were no ERRORs:

There were no WARNINGs:

There were no NOTEs:


## Downstream dependencies
There are currently no downstream dependencies for this package. 


