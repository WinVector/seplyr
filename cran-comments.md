

## Test environments

### Windows

    rhub::check_for_cran()
     620#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     621#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     622#> setting R_REMOTES_STANDALONE to true
     623#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     624#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     625#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     626#> * using log directory 'C:/Users/USERIUNWijvucs/seplyr.Rcheck'
     627#> * using R Under development (unstable) (2020-01-07 r77637)
     628#> * using platform: x86_64-w64-mingw32 (64-bit)
     629#> * using session charset: ISO8859-1
     630#> * using option '--as-cran'
     631#> * checking for file 'seplyr/DESCRIPTION' ... OK
     632#> * checking extension type ... Package
     633#> * this is package 'seplyr' version '0.8.5'
     634#> * package encoding: UTF-8
     635#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     636#> Maintainer: 'John Mount '
     692#> Status: OK

### OSX

    R CMD check --as-cran seplyr_0.8.5.tar.gz 
    * using R version 3.6.2 (2019-12-12)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘0.8.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep()
    character(0)
