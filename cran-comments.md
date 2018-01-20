
Submitting 'seplyr' to fix the CRAN packages using undeclared packages in vignettes issue
reported by Professor Brian Ripley in his 2018-01-02 email to package maintainers. I
suspect the issue was under-declaring our dependence on 'DBI' and 'RSQLite' (now strengthened).
Please note this package has had 7 updates in the last 6 months, but I think fixing 
the reported CRAN issue is a high priority.


## Test environments

  * Windows
  * using R Under development (unstable) (2017-12-30 r73992)
  * using platform: x86_64-w64-mingw32 (64-bit)

  * OSX
  * using R version 3.4.3 (2017-11-30)
  * using platform: x86_64-apple-darwin15.6.0 (64-bit)
 

## R CMD check results

R CMD check --as-cran seplyr_0.5.2.tar.gz 

  * this is package ‘seplyr’ version ‘0.5.2’
  * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     Maintainer: ‘John Mount <jmount@win-vector.com>’


Status: OK (no other notes, errors, or warnings)

## Reverse dependencies

     devtools::revdep_check()
     Checking 2 packages: replyr, WVPlots
     Checked replyr : 0 errors | 0 warnings | 0 notes
     Checked WVPlots: 0 errors | 1 warning  | 0 notes

WVPlots warning is spurious:  "Warning: 'unpivotValuesToRows' is deprecated."
'unpivotValuesToRows' is not deprecated on the CRAN version of 'cdata' and
this will be fixed in 'WVPlots' before the CRAN version of 'cdata' deprecates 
methods.


