

## Test environments

### OSX

    R CMD check --as-cran seplyr_1.0.4.tar.gz
    * using log directory ‘/Users/johnmount/Documents/seplyr.Rcheck’
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘1.0.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


### Windows

    devtools::check_win_devel()

    rhub::check_for_cran()
     516#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     517#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     518#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
     519#> setting R_REMOTES_STANDALONE to true
     520#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     521#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     522#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     523#> Error : Bioconductor does not yet build and check packages for R version 4.2; see
     524#> https://bioconductor.org/install
     525#> * using log directory 'C:/Users/USERTgJojhtdqR/seplyr.Rcheck'
     526#> * using R Under development (unstable) (2021-08-17 r80776)
     527#> * using platform: x86_64-w64-mingw32 (64-bit)
     528#> * using session charset: ISO8859-1
     529#> * using option '--as-cran'
     530#> * checking for file 'seplyr/DESCRIPTION' ... OK
     531#> * checking extension type ... Package
     532#> * this is package 'seplyr' version '1.0.4'
     533#> * package encoding: UTF-8
     534#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     535#> Maintainer: 'John Mount '
     591#> Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep(pkg = 'seplyr')
    character(0)
