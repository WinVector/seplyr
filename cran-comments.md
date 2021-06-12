

## Test environments

### OSX

    R CMD check --as-cran seplyr_1.0.3.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘1.0.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2021-06-11 r80486)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'seplyr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'seplyr' version '1.0.3'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK


## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep(pkg = 'seplyr')
    character(0)
