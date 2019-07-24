

## Test environments

### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2019-07-05 r76784)
    * using platform: x86_64-w64-mingw32 (64-bit)
    Status: OK

### OSX

    R CMD check --as-cran seplyr_0.8.4.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘0.8.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep()
    character(0)


