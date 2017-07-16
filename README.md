<!-- README.md is generated from README.Rmd. Please edit that file -->
This document describes the [`R`](https://www.r-project.org) package [`seplyr`](https://github.com/WinVector/seplyr) which supplies *s*tandard *e*valuation interfaces for some common [`dplyr`](https://CRAN.R-project.org/package=dplyr) verbs.

The idea is this package lets you program over `dplyr` 0.7.\* without needing a Ph.D. in computer science.

To install this packing in `R` please either install from [CRAN](https://CRAN.R-project.org/package=seplyr) with:

``` r
   install.packages('seplyr')
```

or from [GitHub](https://github.com/WinVector/seplyr):

``` r
   devtools::install_github('WinVector/seplyr')
```

In `dplyr` if you know the names of columns when you are writing code you can write code such as the following.

``` r
suppressPackageStartupMessages(library("dplyr"))
packageVersion("dplyr")
 #  [1] '0.7.1.9000'

datasets::mtcars %>% 
  group_by(cyl, gear) %>% 
  head()
 #  # A tibble: 6 x 11
 #  # Groups:   cyl, gear [4]
 #      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
 #    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 #  1  21.0     6   160   110  3.90 2.620 16.46     0     1     4     4
 #  2  21.0     6   160   110  3.90 2.875 17.02     0     1     4     4
 #  3  22.8     4   108    93  3.85 2.320 18.61     1     1     4     1
 #  4  21.4     6   258   110  3.08 3.215 19.44     1     0     3     1
 #  5  18.7     8   360   175  3.15 3.440 17.02     0     0     3     2
 #  6  18.1     6   225   105  2.76 3.460 20.22     1     0     3     1
```

In `dplyr` `0.7.*` if the names of the columns are coming from a variable set elsewhere you would to need to use a tool (such as `rlang`/`tidyeval`) to substitute those names in as show below.

``` r
groupingVars <- c('cyl', 'gear') # assume this is set elsewhere

datasets::mtcars %>% 
  group_by(!!!rlang::syms(groupingVars)) %>% 
  head()
 #  # A tibble: 6 x 11
 #  # Groups:   cyl, gear [4]
 #      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
 #    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 #  1  21.0     6   160   110  3.90 2.620 16.46     0     1     4     4
 #  2  21.0     6   160   110  3.90 2.875 17.02     0     1     4     4
 #  3  22.8     4   108    93  3.85 2.320 18.61     1     1     4     1
 #  4  21.4     6   258   110  3.08 3.215 19.44     1     0     3     1
 #  5  18.7     8   360   175  3.15 3.440 17.02     0     0     3     2
 #  6  18.1     6   225   105  2.76 3.460 20.22     1     0     3     1
```

If you don't want to try and digest entire theory of quasi-quoting (the `rlang::syms()`) and splicing (the `!!!`) then you can use `seplyr` which conveniently and legibly wraps the operations as follows:

``` r
# devtools::install_github('WinVector/seplyr')
library("seplyr")

datasets::mtcars %>% 
  group_by_se(groupingVars) %>% 
  head()
 #  # A tibble: 6 x 11
 #  # Groups:   cyl, gear [4]
 #      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
 #    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 #  1  21.0     6   160   110  3.90 2.620 16.46     0     1     4     4
 #  2  21.0     6   160   110  3.90 2.875 17.02     0     1     4     4
 #  3  22.8     4   108    93  3.85 2.320 18.61     1     1     4     1
 #  4  21.4     6   258   110  3.08 3.215 19.44     1     0     3     1
 #  5  18.7     8   360   175  3.15 3.440 17.02     0     0     3     2
 #  6  18.1     6   225   105  2.76 3.460 20.22     1     0     3     1
```

And that is it.

------------------------------------------------------------------------

`seplyr::group_by_se()` performs the wrapping for you without you having to work through the details of `rlang`. If you are interested in the details `seplyr` itself is a good tutorial. For example you can examine `seplyr`'s implementation to see the necessary notations (using a command such as `print(group_by_se)`). And, of course, we try to supply some usable help entries, such as: `help(group_by_se)`.

The current set of SE adapters includes (all commands of the form `NAME_se()` being adapters for a `dplyr::NAME()`):

-   `arrange_se()`
-   `distinct_se()`
-   `group_by_se()`
-   `group_indices_se()`
-   `rename_se()`
-   `select_se()`
-   `tally_se()`
-   `add_tally_se()`
-   `count_se()`
-   `add_count_se()`

Only two of the above are completely redundant (`seplyr::group_by_se()` essentially working as `dplyr::group_by_at()` and `seplyr::select_se()` essentially working as `dplyr::select_at()`; others either having different semantics or no matching `dplyr::*_at()` method). Roughly all `seplyr` is trying to do is give a uniform first-class standard interface to all of the primary deprecated underscore suffixed verbs (such as `dplyr::arrange_`).

In addition to the series of adapters we also supply a number of useful new verbs including:

-   `group_summarize()` Binds grouping, arrangement, and summarization together for clear documentation of intent.
-   `add_group_summaries()` Adds per-group summaries to data.
-   `add_group_indices()` Adds a column of per-group ids to data.
-   `add_group_sub_indices()` Adds a column of in-group rank ids to data.
-   `add_rank_indices()` Adds rank indices to data.

`seplyr` is designed to be a thin package that passes all work to `dplyr`. If you want a package that works around `dplyr` implementation differences on different data sources I suggest trying our own [`replyr`](https://CRAN.R-project.org/package=replyr) package.
