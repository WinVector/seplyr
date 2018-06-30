
Fix error caused by calling ifFALSE() for versions of R prior to 3.5.0.

## Test environments

  * Windows
  * using R Under development (unstable) (2018-06-28 r74941)
  * using platform: x86_64-w64-mingw32 (64-bit)

  * OSX
  * using R version 3.5.0 (2018-04-23)
  * using platform: x86_64-apple-darwin15.6.0 (64-bit)

## R CMD check results

  R CMD check --as-cran seplyr_0.5.7.tar.gz 

  * using option ‘--as-cran’
  * checking for file ‘seplyr/DESCRIPTION’ ... OK
  * checking extension type ... Package
  * this is package ‘seplyr’ version ‘0.5.7’
  * package encoding: UTF-8
  * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
  Maintainer: ‘John Mount <jmount@win-vector.com>’
  Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep()
    character(0)



