
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
