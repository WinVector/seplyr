
Push a change requested by the rlang package team ( https://github.com/WinVector/seplyr/issues/3 ).

## Test environments

### Windows

    rhub::check_for_cran()
    507#> * using R Under development (unstable) (2018-12-26 r75909)
    508#> * using platform: x86_64-w64-mingw32 (64-bit)
    509#> * using session charset: ISO8859-1
    510#> * using option '--as-cran'
    511#> * checking extension type ... Package
    512#> * this is package 'seplyr' version '0.8.3'
    513#> * checking for file 'seplyr/DESCRIPTION' ... OK
    514#> * package encoding: UTF-8
    515#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    516#> Maintainer: 'John Mount '
    570#> Status: OK
 
### OSX

    R CMD check --as-cran seplyr_0.8.3.tar.gz
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘0.8.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep()
    character(0)



