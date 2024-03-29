

# seplyr 1.0.4 2021-09-01

 * Documentation fixes (catch up with rlang fixes).

# seplyr 1.0.3 2021-06-12

 * Remove LazyData decl

# seplyr 1.0.1 2020-10-18

 * Switch to tinytest.
 * Deal with add deprecation in dplyr.

# seplyr 1.0.0 2020-08-12

 * http: to https: where feasible.

# seplyr 0.8.8 2020-05-23

 * Fix datasets dependency again
 
# seplyr 0.8.7 2020-05-13

 * Fix datasets dependency
 
# seplyr 0.8.6 2020-04-30

 * Patches for dplyr 1.0.0
 * Improve tests
 * Badges

# seplyr 0.8.5 2020-01-16

 * Add grouped extend.

# seplyr 0.8.4 2019-07-24

 * Adjust license.

# seplyr 0.8.3 2019-01-02

 * work around https://github.com/tidyverse/dplyr/issues/4062 .
 * more select_nse() work.
 * fix non-grouped xes of group_summarize().

# seplyr 0.8.2 2018-09-25

 * force parent.frame().
 * stricter argument type checking.
 * warnings on mutate(), summarize(), and transmute().
 * fix some argument names.
 * start on select_nse().

# seplyr 0.8.1 2018-09-18

 * Switch from parse_quosure() to parse_quo() ( https://github.com/WinVector/seplyr/issues/3 ).
 * Some documentation fixes.

# seplyr 0.8.0 2018-09-10

 * more expression partition work
 * special-case empty grouping and arranging.
 * check more about arguments in arrange_se().
 * add .by_group argument to arrange_se().

# seplyr 0.5.9 2018-07-20

 * dplyr::one_of() option.
 * Documentation fixes.

# seplyr 0.5.8 2018-06-30

 * Fix NSE interface to gather()/spread().

# seplyr 0.5.7 2018-06-30

 * Remove use of isFALSE().
 * Add gather_se() and spread_se().
 
# seplyr 0.5.6 2018-06-23

 * Adjust dependencies.
 * complete_se() from Richard Layton graphdr.

# seplyr 0.5.5 2018-03-12

 * Move to wrapr 1.3.0.
 * Lower dependencies.
 * Remove factor_mutate vignette.
 
# seplyr 0.5.4 2018-02-21

 * Switch to qae() (need wrapr 1.2.0 or newer version).
 * Undeprecate mutate_nse() et.al..
 * Remove other deperecated functions.
 * Avoid possible rlang/dplyr issue in rename vignette https://github.com/r-lib/rlang/issues/411 .
 * Move off rlang::funs().
 
# seplyr 0.5.3 2018-01-21

 * Fix misspelling of package import.
 
# seplyr 0.5.2 2018-01-20

 * Fix missed case in statement partitioner.
 * Fix deprecations and visibility of stringAlgebra.
 * Start to relax quoting conditions.
 * Factor mutate helper.

# seplyr 0.5.1 2018-01-02

 * mutate_se now uses partition_mutate_se.
 * partition on re-assignment (if_else_device does this, but includes a re-use so was okay).
 * Documentation improvements.
 * More argument checking.
 * Move to wrapr 1.0.2 (pick up qae() and qe()).
 * Deprecate _nse forms.

# seplyr 0.5.0 2017-11-24

 * mutate statement planner.
 * per-row block if-else simulator

# seplyr 0.1.6 2017-11-17

 * work on lists.
 * re-export more dplyr functions.
 * mutate_nse in stages.
 * rename_nse in stages.
 * rename_se direct pass-through option.

# seplyr 0.1.5 2017-08-28

 * add wrapr dependency (for named map builder, and lambda abstraction).
 * finish string algebra and add more *_nse() methods.
 
# seplyr 0.1.4 2017-08-21

 * Documentation fixes.
 * add mutate_nse().
 * add lambda operator.

# seplyr 0.1.3 2017-07-29

 * Implicit paste() in some cases.
 * Replace rlang::parse_expr() with rlang::parse_quosure() to get explicit environment control.

# seplyr 0.1.2 2017-07-27

 * Add novelName().
 * Move dplyr to imports.
 
# seplyr 0.1.1 2017-07-22

 * Fix some documentation.
 * add add_in_group_indices().
 * add many more adapter fns.
 * add := operator.
 * more vignettes.


# seplyr 0.1.0 2017-07-14

 * First CRAN submission.
