bquote functions
================

``` r
suppressPackageStartupMessages(library("dplyr"))
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

``` r
# define our parameters
group_nm <- as.name("am")
num_nm <- as.name("hp")
den_nm <- as.name("cyl")
derived_nm <- as.name(paste0(num_nm, "_per_", den_nm))
mean_nm <- as.name(paste0("mean_", derived_nm))
count_nm <- as.name("group_count")

# apply a parameterized pipeline using rlang::!!
mtcars %>%
  group_by(!!group_nm) %>%
  mutate(!!derived_nm := !!num_nm/!!den_nm) %>%
  summarize(
    !!mean_nm := mean(!!derived_nm),
    !!count_nm := n()
  ) %>%
  ungroup() %>%
  arrange(!!group_nm)
```

    ## # A tibble: 2 x 3
    ##      am mean_hp_per_cyl group_count
    ##   <dbl>           <dbl>       <int>
    ## 1     0            22.7          19
    ## 2     1            23.4          13

``` r
# wrap some dplyr methods
nms <- c("add_count", "add_tally", "all_equal", "anti_join",
         "arrange", "as.tbl", "bind_cols", "bind_rows", "collect",
         "compute", "copy_to", "count", "distinct", "filter",
         "full_join", "group_by", "group_indices", "inner_join",
         "is.tbl", "left_join", "mutate", "rename", "right_join",
         "select", "semi_join", "summarise", "summarize", "tally",
         "tbl", "transmute", "ungroup")
env <- environment()
for(fname in nms) {
  assign(fname, 
         wrapr::bquote_function(getFromNamespace(fname, ns = "dplyr")),
         envir = env)
}



# apply a parameterized pipeline using bquote
mtcars %>%
  group_by(.(group_nm)) %>%
  mutate(.(derived_nm) := .(num_nm)/.(den_nm)) %>%
  summarize(
    .(mean_nm) := mean(.(derived_nm)),
    .(count_nm) := n()
  ) %>%
  ungroup() %>%
  arrange(.(group_nm))
```

    ## # A tibble: 2 x 3
    ##      am mean_hp_per_cyl group_count
    ##   <dbl>           <dbl>       <int>
    ## 1     0            22.7          19
    ## 2     1            23.4          13
