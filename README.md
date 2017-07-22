<!-- README.md is generated from README.Rmd. Please edit that file -->
This document describes the [`R`](https://www.r-project.org) package [`seplyr`](https://github.com/WinVector/seplyr) which supplies *s*tandard *e*valuation interfaces for some common [`dplyr`](https://CRAN.R-project.org/package=dplyr) verbs. `seplyr` stands for "standard evaluation dplyr" ("standard" meaning we prefer explicit user provided strings over passive [non-standard evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html) capture of un-evaluated user code) or "string edition dplyr."

The idea is this package lets you program over `dplyr` 0.7.\* without needing a Ph.D. in computer science.

To install this packing in `R` please either install from [CRAN](https://CRAN.R-project.org/package=seplyr) with:

``` r
   install.packages('seplyr')
```

or from [`GitHub`](https://github.com/WinVector/seplyr):

``` r
   devtools::install_github('WinVector/seplyr')
```

In `dplyr` if you know the names of columns when you are writing code you can write code such as the following.

``` r
suppressPackageStartupMessages(library("dplyr"))
packageVersion("dplyr")
 #  [1] '0.7.2'

datasets::mtcars %>% 
  arrange(cyl, desc(gear)) %>% 
  head()
 #     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
 #  1 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
 #  2 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
 #  3 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
 #  4 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
 #  5 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
 #  6 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
```

In `dplyr` `0.7.*` if the names of the columns are coming from a variable set elsewhere you would to need to use a tool to substitute those names in. One such tool is `rlang`/`tidyeval` (though we strongly prefer [`seplyr`](https://github.com/WinVector/seplyr/blob/master/README.md) and [`wrapr::let()`](https://github.com/WinVector/wrapr/blob/master/README.md))). `rlang`/`tidyeval` works as follows (for comparison only, this is *not* our suggested workflow).

``` r
# Assume this is set elsewhere,
# supplied by a user, function argument, or control file.
orderTerms <- c('cyl', 'desc(gear)')

# Now convert into splice-able types, the idea is the user
# supplies variable names that we later convert to "quosures"
# for use in `dplyr` 0.7.* generic code.
# This code is near the pipe under the rule:
# "If you are close enough to form a quosure, 
#  you are close enough to re-code the analysis"
orderQs <- lapply(orderTerms,
                  function(si) { rlang::parse_expr(si) })
# pipe
datasets::mtcars %>% 
  arrange(!!!orderQs) %>% 
  head()
 #     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
 #  1 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
 #  2 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
 #  3 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
 #  4 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
 #  5 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
 #  6 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
```

If you don't want to try and digest entire theory of quasi-quoting and splicing (the `!!!` operator) then you can use `seplyr` which conveniently and legibly wraps the operations as follows:

``` r
# devtools::install_github('WinVector/seplyr')
library("seplyr")

datasets::mtcars %>% 
  arrange_se(orderTerms) %>% 
  head()
 #     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
 #  1 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
 #  2 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
 #  3 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
 #  4 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
 #  5 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
 #  6 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
```

The idea is: the above code looks very much like simple `dplyr` code used running an analysis, and yet is very easy to [parameterize](http://www.win-vector.com/blog/2016/12/parametric-variable-names-and-dplyr/) and re-use in a script or package.

------------------------------------------------------------------------

`seplyr::arrange_se()` performs the wrapping for you without you having to work through the details of `rlang`. If you are interested in the details `seplyr` itself is a good tutorial. For example you can examine `seplyr`'s implementation to see the necessary notations (using a command such as `print(arrange_se)`). And, of course, we try to supply some usable help entries, such as: `help(arrange_se)`. Some more discussion of the ideas can be found [here](http://www.win-vector.com/blog/2017/07/dplyr-0-7-made-simpler/).

The current set of SE adapters includes (all commands of the form `NAME_se()` being adapters for a `dplyr::NAME()` method):

-   `add_count_se()`
-   `add_tally_se()`
-   `arrange_se()`
-   `count_se()`
-   `distinct_se()`
-   `filter_se()`
-   `group_by_se()`
-   `group_indices_se()`
-   `mutate_se()`
-   `rename_se()`
-   `select_se()`
-   `summarize_se()`
-   `tally_se()`
-   `transmute_se()`

Only two of the above are completely redundant. `seplyr::group_by_se()` essentially works as `dplyr::group_by_at()` and `seplyr::select_se()` essentially works as `dplyr::select_at()`. The others either have different semantics or currently (as of `dplyr` `0.7.1`) no matching `dplyr::*_at()` method. Roughly all `seplyr` is trying to do is give a uniform first-class standard interface to all of the primary deprecated underscore suffixed verbs (such as `dplyr::arrange_`).

We also have a few methods that work around a few of the minor inconvenience of working with variable names as strings:

-   `deselect()`
-   `rename_mp()`

For now we are not emphasizing `seplyr::mutate_se()` and `seplyr::summarize_se()` as we think in some cases the best ways to work with these verbs may be a combination of their `dplyr::*_at()` forms plus a `seplyr::rename_se()`. For example:

``` r
datasets::iris %>%
  group_by_at("Species") %>%
  summarize_at(c("Sepal.Length", "Sepal.Width"), funs(mean)) %>%
  rename_se(c("Mean.Sepal.Length" := "Sepal.Length", 
              "Mean.Sepal.Width" := "Sepal.Width"))
 #  # A tibble: 3 x 3
 #       Species Mean.Sepal.Length Mean.Sepal.Width
 #        <fctr>             <dbl>            <dbl>
 #  1     setosa             5.006            3.428
 #  2 versicolor             5.936            2.770
 #  3  virginica             6.588            2.974
```

In the above the `rename_se()` is simply changing the column names to what we want (because we did not specify this in the `summarize_at()` step). The `:=` operator is just a convenience function for building up maps, we could also have used "`=`" or passed in a named vector built up elsewhere.

However, we can also perform the above directly with `seplyr::summarize_se()` as follows.

``` r
datasets::iris %>%
  group_by_se("Species") %>%
  summarize_se(c("Mean.Sepal.Length" := "mean(Sepal.Length)", 
                 "Mean.Sepal.Width" := "mean(Sepal.Width)"))
 #  # A tibble: 3 x 3
 #       Species Mean.Sepal.Length Mean.Sepal.Width
 #        <fctr>             <dbl>            <dbl>
 #  1     setosa             5.006            3.428
 #  2 versicolor             5.936            2.770
 #  3  virginica             6.588            2.974
```

In addition to the series of adapters we also supply a number of useful new verbs including:

-   `group_summarize()` Binds grouping, arrangement, and summarization together for clear documentation of intent.
-   `add_group_summaries()` Adds per-group summaries to data.
-   `add_group_indices()` Adds a column of per-group ids to data.
-   `add_group_sub_indices()` Adds a column of in-group rank ids to data.
-   `add_rank_indices()` Adds rank indices to data.

`seplyr` is designed to be a thin package that passes all work to `dplyr`. If you want a package that works around `dplyr` implementation differences on different data sources I suggest trying our own [`replyr`](https://CRAN.R-project.org/package=replyr) package. Another alternative is using [`wrapr::let()`](https://github.com/WinVector/wrapr/blob/master/README.md).

Some inspiration comes from [Sebastian Kranz's `s_dplyr`](https://gist.github.com/skranz/9681509).
