# mousetrap 2.0.0 (under development)
* General changes in existing functions
  * Renamed `show_progress` argument to `verbose` (+ set default to `FALSE`).
  * Introduced `dimensions` argument to explicitly specify the names of the columns in the trajectory array that contain the mouse positions. In most cases, the default is `c("xpos","ypos")` as the x- and y-positions should be used. Note that in some functions (as specified in the documentation) the order of the labels matters, the first value will be taken as the label of the x-positions, the second as the label of the y-positions.
  * Introduced `timestamps` argument to explicitly specify the dimension in the trajectory array containing the timestamps.
  * Perform merging of trial and trajectory data (e.g., in `mt_subset` and `mt_reshape`) based on the `rownames`. Therefore, the `mt_id` column in data.frames is not needed anymore - but is kept for the convenience of the user. The column is called `"mt_id"` in import and measures functions. `mt_reshape` allows the user to specify the label of the `mt_id` column explicitly.
  * As a consequence of the previous changes: Removal of the internal variables `mt_variable_labels` and `mt_id`.
  * Removed "calculate" from all mt_calculate functions: `mt_calculate_measures`, `mt_calculate_derivatives`, `mt_calculate_deviations` are now called `mt_measures`, `mt_derivatives`, `mt_deviations`.
  * Introduction of internal utils function `create_results` (that simplifies including the newly created trajectories or measures in the existing mousetrap data object).
* Changes in specific functions
  * Allow specifying several variables for the trial identifier in `mt_id_label` (in all mt_import functions). A a new ID variable will be created by combining the values of each variable.
  * Enable `mt_import_long` and `mt_import_wide` to import any number of additional variables using `add_labels` (closes #4).
  * Timestamps are no longer used for ordering in `mt_import_long` if `mt_seq_label` is not provided. Instead, data will be imported in the order in which they were stored in `raw_data`.
  * Improved speed of `mt_import_long`.
  * Introduction of `dimensions` argument in `mt_space_normalize` and `mt_align_start` and the corresponding arguments `start` and `end` (deprecate xpos/ypos_start/end). This also fixes the internal bug that in `mt_align_start` `xpos_start` was passed on as `ypos_start` to `mt_space_normalize` (instead of `ypos_start`).
  * Allow `mt_derivatives` to work with an arbitrary number of dimensions and rename `dimension` argument to `dimensions`.
  * Vectorized function `point_to_line` for time speed up in `mt_deviations` (closes #2, thanks to @sbrockhaus).
  * Allow for flexible dimension labels also in `mt_measures` and rename all measures columns relating only to x- or y-positions depending on the values in dimensions (e.g., `x_max` becomes `xpos_max`, `x_flips` becomes `xpos_flips`). Change column label `xy_dist` to `total_dist`.
  * Simplify AUC calculation in `mt_measures` using the actual x- and y-positions . New AUC values correlate to 1.00 with old values in mt_example, but in some cases extremely small differences are possible (maximum difference of 2.328e-10 in mt_example).
* New functions
  * `mt_add_variables`: add new variables to trajectory array.

# mousetrap 1.2.0
* New function `mt_align_start` adjusts trajectories so that they have the same start position (wrapper for `mt_space_normalize`)
* New function `mt_calculate_deviations` calculates the idealized trajectory and the perpendicular deviations of the actual trajectory from it for each position in the trajectory array
* For many functions, a trajectory array can now be provided directly via the `data` argument. In this case, the `use` argument will be ignored and only the resulting trajectory array will be returned
* `mt_plot_riverbed` now preserves factor levels for facets
* `mt_calculate_derivatives` now allows for custom dimension names using the `prefix` argument
* `mt_plot_per_trajectory` now receives the file name as the first argument

# mousetrap 1.1.0
* `mt_import_mousetrap` now offers possibility to combine several variables in mouse-tracking raw data
* `mt_plot_riverbed` now allows for faceting
* `mt_check_resolution` now offers possibility to check (relative) frequencies of desired timestamp differences
* Improved preprocessing of mouse-tracking raw data for `mt_import_mousetrap`
* Change of default scale for fill in `mt_plot_riverbed`
* Custom x and y axis labels for `mt_plot_riverbed` can no longer be set via `x_label` and `y_label` (but can be added using the `ggplot2` functions `xlab()` and `ylab()`, see Examples)
* Minor fix in package documentation (#1) (thanks to @sbrockhaus) 
* Minor fix in documentation of x/y reversals (thanks to Barnabás Szászi)

# mousetrap 1.0.0
* First release
