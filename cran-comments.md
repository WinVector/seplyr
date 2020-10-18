

## Test environments

### OSX

    R CMD check --as-cran seplyr_1.0.1.tar.gz 
    * using log directory ‘/Users/johnmount/Documents/work/seplyr.Rcheck’
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘seplyr/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘seplyr’ version ‘1.0.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Linux

    rhub::check_for_cran()
    1479#> About to run xvfb-run R CMD check --as-cran seplyr_1.0.1.tar.gz
    1480#> * using log directory ‘/home/docker/seplyr.Rcheck’
    1481#> * using R version 3.6.1 (2019-07-05)
    1482#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1483#> * using session charset: UTF-8
    1484#> * using option ‘--as-cran’
    1485#> * checking for file ‘seplyr/DESCRIPTION’ ... OK
    1486#> * checking extension type ... Package
    1487#> * this is package ‘seplyr’ version ‘1.0.1’
    1488#> * package encoding: UTF-8
    1489#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1490#> Maintainer: ‘John Mount ’
    1545#> Status: OK


    rhub::check_for_cran()
    1510#> About to run xvfb-run R CMD check --as-cran seplyr_1.0.1.tar.gz
    1511#> * using log directory ‘/home/docker/seplyr.Rcheck’
    1512#> * using R Under development (unstable) (2020-10-17 r79346)
    1513#> * using platform: x86_64-pc-linux-gnu (64-bit)
    1514#> * using session charset: UTF-8
    1515#> * using option ‘--as-cran’
    1516#> * checking for file ‘seplyr/DESCRIPTION’ ... OK
    1517#> * checking extension type ... Package
    1518#> * this is package ‘seplyr’ version ‘1.0.1’
    1519#> * package encoding: UTF-8
    1520#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1521#> Maintainer: ‘John Mount ’
    1578#> Status: OK

## Reverse dependencies

No declared reverse dependencies.

    devtools::revdep(pkg = 'seplyr')
    character(0)
