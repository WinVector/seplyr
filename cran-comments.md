

## Test environments

### OSX

    R CMD check --as-cran seplyr_0.8.8.tar.gz
    * using R version 4.0.0 (2020-04-24)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘0.8.8’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::check_win_devel()

    rhub::check_for_cran()
    531#> ** byte-compile and prepare package for lazy loading
    532#> Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :
    533#> Calls: ... asNamespace -> loadNamespace -> namespaceImport -> loadNamespace
    534#> Execution halted
    535#> namespace 'vctrs' 0.2.4 is already loaded, but >= 0.3.0 is required
    536#> ERROR: lazy loading failed for package 'seplyr'
    537#> * removing 'C:/Users/USERRsaNFVAcOG/R/seplyr'
    dplyr won't install, vctrs version not declared in seplyr. Check environment or dplyr issue.

### Linux

    rhub::check_for_cran()
    1812#> About to run xvfb-run R CMD check --as-cran seplyr_0.8.8.tar.gz
    1813#> * using log directory ‘/home/docker/seplyr.Rcheck’
    1814#> * using R version 3.6.1 (2019-07-05)
    1815#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1816#> * using session charset: UTF-8
    1817#> * using option ‘--as-cran’
    1818#> * checking for file ‘seplyr/DESCRIPTION’ ... OK
    1819#> * checking extension type ... Package
    1820#> * this is package ‘seplyr’ version ‘0.8.8’
    1821#> * package encoding: UTF-8
    1822#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1823#> Maintainer: ‘John Mount ’
    1878#> Status: OK

    rhub::check_for_cran()
    1853#> About to run xvfb-run R CMD check --as-cran seplyr_0.8.8.tar.gz
    1854#> * using log directory ‘/home/docker/seplyr.Rcheck’
    1855#> * using R Under development (unstable) (2020-05-16 r78476)
    1856#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1857#> * using session charset: UTF-8
    1858#> * using option ‘--as-cran’
    1859#> * checking for file ‘seplyr/DESCRIPTION’ ... OK
    1860#> * checking extension type ... Package
    1861#> * this is package ‘seplyr’ version ‘0.8.8’
    1862#> * package encoding: UTF-8
    1863#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1864#> Maintainer: ‘John Mount ’
    1921#> Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep()
    character(0)
