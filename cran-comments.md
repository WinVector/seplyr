
Note to CRAN: In addition to new features and fixes 
(covered in NEWS.md), this is a maintenance release to
work around errors triggered by the 1.2.0 CRAN release of
'rlang' on 2018-02-20.  The 'rlang' team released this
version of 'rlang' knowing that it triggered a vignette
failure in the 'seplyr' package, (probably without contacting
the maintainers of the impacted package, and certainly without
waiting for or confirming with the maintainers of the impacted
package: https://github.com/r-lib/rlang/issues/411 ).  
This 'seplyr' update removes the interacting code from 'seplyr'
and clears the issue.


## Test environments

  * Windows (checked 0.5.2 version of seplyr)
  * using R Under development (unstable) (2018-01-19 r74138)
  * using platform: x86_64-w64-mingw32 (64-bit)

  * OSX
  * using R version 3.4.3 (2017-11-30)
  * using platform: x86_64-apple-darwin15.6.0 (64-bit)

 

## R CMD check results

R CMD check --as-cran seplyr_0.5.4.tar.gz 

  * checking extension type ... Package
  * this is package ‘seplyr’ version ‘0.5.4’
  * package encoding: UTF-8
  * checking CRAN incoming feasibility ... NOTE

    Maintainer: ‘John Mount <jmount@win-vector.com>’

    Number of updates in past 6 months: 7

    Unknown, possibly mis-spelled, fields in DESCRIPTION:
      ‘Remotes’


Status: 1 NOTE

## Reverse dependencies

     devtools::revdep_check()
     Checking 2 packages: replyr, WVPlots
     Checked replyr : 0 errors | 0 warnings | 0 notes
     Checked WVPlots: 0 errors | 0 warning  | 0 notes


