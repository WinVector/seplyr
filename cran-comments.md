

## Test environments

### Windows

   
    devtools::check_win_devel()
    * using log directory 'd:/RCompile/CRANguest/R-devel/seplyr.Rcheck'
    * using R Under development (unstable) (2020-05-11 r78411)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'seplyr/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'seplyr' version '0.8.7'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

    rhub::check_for_cran()
    731#> there is no package called 'utf8'
    Property of test environment, not of the package.

### Linux

    rhub::check_for_cran()
    1887#> About to run xvfb-run R CMD check --as-cran seplyr_0.8.7.tar.gz
    1888#> * using log directory ‘/home/docker/seplyr.Rcheck’
    1889#> * using R version 3.6.1 (2019-07-05)
    1890#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1891#> * using session charset: UTF-8
    1892#> * using option ‘--as-cran’
    1893#> * checking for file ‘seplyr/DESCRIPTION’ ... OK
    1894#> * checking extension type ... Package
    1895#> * this is package ‘seplyr’ version ‘0.8.7’
    1896#> * package encoding: UTF-8
    1897#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1898#> Maintainer: ‘John Mount ’
    1953#> Status: OK
    
    1904#> About to run xvfb-run R CMD check --as-cran seplyr_0.8.7.tar.gz
    1905#> * using log directory ‘/home/docker/seplyr.Rcheck’
    1906#> * using R Under development (unstable) (2020-05-10 r78398)
    1981#> Error in bquote(on.exit({ : 'where' must be an environment
    1995#> Status: 1 ERROR
    Believe this is an incompatibility between this development version of
    R and tidyr (saw same message on CRAN intermittently).

### OSX

    R CMD check --as-cran seplyr_0.8.7.tar.gz
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘0.8.7’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep()
    character(0)
