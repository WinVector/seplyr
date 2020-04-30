

## Test environments

### Windows

    rhub::check_for_cran()
    E  checking package dependencies
        Packages required but not available: 'dplyr', 'rlang', 'tidyr'
   
    devtools::check_win_devel()
    * using R Under development (unstable) (2020-04-30 r78335)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'seplyr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'seplyr' version '0.8.6'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

### OSX

    R CMD check --as-cran seplyr_0.8.6.tar.gz 
    * using R version 4.0.0 (2020-04-24)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘0.8.6’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep()
    character(0)
