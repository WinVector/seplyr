
<!-- README.md is generated from README.Rmd. Please edit that file -->
The [`R`](https://www.r-project.org) package [`seplyr`](https://github.com/WinVector/seplyr) supplies *s*tandard *e*valuation interfaces for some common [`dplyr`](https://CRAN.R-project.org/package=dplyr) verbs. `seplyr` stands for "standard evaluation dplyr" ("standard" meaning we prefer explicit user provided strings over passive [non-standard evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html) capture of un-evaluated user code) or "string edition dplyr."

![](https://github.com/WinVector/seplyr/raw/master/tools/safety.png)

To get started we suggest visiting the [`seplyr` site](https://winvector.github.io/seplyr/), and checking out [some examples](https://winvector.github.io/seplyr/articles/seplyr.html).

One quick example:

``` r
# Assume this is set elsewhere,
# supplied by a user, function argument, or control file.
orderTerms <- c('cyl', 'desc(gear)')

# load packages
library("seplyr")
 #  Loading required package: wrapr

# where we are actually working (perhaps in a re-usable
# script or function)
datasets::mtcars %.>% 
  arrange_se(., orderTerms) %.>% 
  head(.)
 #     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
 #  1 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
 #  2 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
 #  3 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
 #  4 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
 #  5 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
 #  6 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
```

The concept is: in writing re-usable code or scripts you pretend you do not know the actual column names you will be asked to work with (that these will be supplied as values later at analysis time). This forces you to write scripts that can be used even if data changes, and are re-usable on new data you did not know about when writing the script.

To install this package please either install from [CRAN](https://CRAN.R-project.org/package=seplyr) with:

``` r
   install.packages('seplyr')
```

or from [`GitHub`](https://github.com/WinVector/seplyr):

``` r
   devtools::install_github('WinVector/seplyr')
```

Please see [`help("%.>%", package="wrapr")`](https://winvector.github.io/wrapr/reference/grapes-.-greater-than-grapes.html) for details on "dot pipe."

Note
----

Note: `seplyr` is meant only for "tame names", that is: variables and column names that are also valid *simple* (without quotes) `R` variables names.
