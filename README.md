<!-- README.md is generated from README.Rmd. Please edit that file -->
This document describes the [`R`](https://www.r-project.org) package [`seplyr`](https://github.com/WinVector/seplyr) which supplies *s*tandard *e*valuation interfaces for some common [`dplyr`](https://CRAN.R-project.org/package=dplyr) verbs.

The idea is this package lets you program over `dplyr` 0.7.\* without needing a Ph.D. in computer science.

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

If instead the names of the columns are coming from a variable set elsewhere you need to use a tool to substitute those names in as show below.

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

If you don't want to try and digest entire theory of quasi-quoting (the `rlang::syms()`) and splicing (the `!!!`) then you can use `seplyr` which conveniently wraps the operations as follows:

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

And that is it. `seplyr::group_by_se()` performs the wrapping for you without you having to work through the details of `rlang`. If you are interested in the details `seplyr` itself is a good tutorial. For example you can examine `seplyr`'s implementation to see the necessary notations:

``` r
print(group_by_se)
 #  function(.data, groupingVars, add = FALSE) {
 #    # convert char vector into spliceable vector
 #    groupingSyms <- rlang::syms(groupingVars)
 #    group_by(.data = .data, !!!groupingSyms, add = add)
 #  }
 #  <bytecode: 0x7fdb7cd77148>
 #  <environment: namespace:seplyr>
```

And of course we try to supply some usable help entries, example `help(group_by_se)`:

    group_by standard interface.

    Description

     Group a data frame by the groupingVars. Author: John Mount, Win-Vector LLC.

    Usage

     group_by_se(.data, groupingVars, add = FALSE)
     
    Arguments

     .data  data.frame
     groupingVars   character vector of column names to group by.
     add    logical, passed to group_by
     
    Value

     .data grouped by columns named in groupingVars

    Examples

     group_by_se(datasets::mtcars, c("cyl", "gear")) %>%
       head()
     # roughly equivalent to:
     # do.call(group_by_, c(list(datasets::mtcars), c('cyl', 'gear')))

In addition to a series of adapters we also supply a number of useful new verbs including:

-   `add_group_indices()` Adds a column of per-group ids to data.
-   `add_group_summaries()` Adds per-group summaries to data.
-   `group_summarize()` Binds grouping, arrangement, and summarization together for clear documentation of intent.

`seplyr` is designed to be a thin package that passes all work to `dplyr`. If you want a package that works around `dplyr` implementation differences on different data sources I suggest trying our own [`replyr`](https://CRAN.R-project.org/package=replyr) package.

------------------------------------------------------------------------

One (*very* advanced) caveat is given below. If you are working with normal in-memory data the following should not matter.

The caveat is: `seplyr` is an `R` package that declares a dependency on `dplyr`.

I *think* this means that `seplyr` is always calling the `dplyr` version of any non-`S3` verbs even if another package (such as `sparklyr` or `dbplyr`) has attempted to override them. I believe run-time `S3` dispatch will correctly re-dispatch to class labeled derived methods. If all relevant package have correct override hygiene there should not be any problems.

If you have any problems working with helper packages (such as `dbplyr` or `sparklyr`) you can try apply `base::source()` to all of the `*.R` files in [`seplyr/R`](https://github.com/WinVector/seplyr/tree/master/R) instead of loading the package.
