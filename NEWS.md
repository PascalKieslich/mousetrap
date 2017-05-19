# mousetrap 3.1.0

## Announcements
* A first paper on the mousetrap software packages has been accepted for publication. It presents the mousetrap plugin for creating mouse-tracking experiments in OpenSesame and also includes a short demonstration of basic analyses using the mousetrap R package.
* Reference: Kieslich, P. J., & Henninger, F. (in press). Mousetrap: An integrated, open-source mouse-tracking package. _Behavior Research Methods_. doi:10.3758/s13428-017-0900-z

## Changes in specific functions
* `mt_derivatives`: now always reports acceleration as difference in absolute velocity (the argument `acc_on_abs_vel` has been removed). Besides, the argument `absolute` has been introduced that indicates if absolute values for distances and velocities should be reported (by default, this is not the case). All of this is only relevant if a single dimension is specified in `dimensions`. 
* `mt_sample_entropy`: the default values reported have changed (cf. bug fix below). `mt_sample_entropy` now only uses a custom function for computing sample entropy (which is faster and produces virtually identical results as `pracma::sample_entropy` if the same parameters are used). Therefore, the `method` argument has been removed. Besides, the `lag` argument has been renamed to `m`.
* `mt_distmat`, `mt_cluster`, `mt_cluster_k`, and `mt_map`: now provide the option to remove trajectory points containing missing values (by setting the `na_rm` argument to `TRUE`). Removal is done column-wise. That is, if any trajectory has a missing value at, e.g., the 10th recorded position, the 10th position is removed for all trajectories.
* `mt_distmat`, `mt_cluster`, `mt_cluster_k`, and `mt_map`: now allow for specifying the relative importance of each trajectory dimension via the `weights` argument. Technically, each variable is rescaled so that the standard deviation matches the corresponding value in `weights`. By default, `weights` is a vector of 1s implying equal importance of each dimension (i.e., all dimensions are standardized to a standard deviation of 1). This changes the default behavior of the functions compared to the previous release where the original variables were used without standardization. To use the original variables, set `weights = NULL`.
* `mt_map`: uses `mousetrap::mt_prototypes` as default for `prototypes` if no prototypes are provided.
* `mt_average`: now internally replaces NaNs with NAs. NaNs only occur if a specific dimension contains only NAs for an interval (which in practice only happens for acc values if the trial stops at the interval boundary).
* `mt_standardize`: now by default standardizes mouse-tracking measures across all trials if no `within` variable is specified.

## Bugs fixed
* `mt_sample_entropy`: Bug fixed for `method="pracma"` (the default method): The window size argument (which used to be specified using the `lag` argument - now this has been renamed to `m`) was incorrectly passed on to the `tau` argument of `pracma::sample_entropy`. It should have beend passed on to the `edim` argument. After fixing this, both methods in `mt_sample_entropy` provided virtually identical results (which is why the `method` argument has been dropped, see above).

## New functions
* `bezier`: create Bezier-curves using the Bernstein approximation.
* `mt_scale_trajectories`: standardize variables in a mouse trajectory array.
* `mt_heatmap_raw`: create high-resolution heatmap of trajectory data.
* `mt_heatmap`: plot trajectory heatmap using base plots.
* `mt_heatmap_ggplot`: plot trajectory heatmap using ggplot. 
* `mt_diffmap`: create a difference-heatmap of two trajectory heatmap images.
* `mt_animate`: create a gif trajectory animation.
* Please note that although these functions have been tested extensively, they still have beta status.

## New dataset
* `KH2017_raw`: Raw mouse-tracking dataset from Kieslich & Henninger (in press).
* `KH2017`: Mouse-tracking dataset from Kieslich & Henninger (in press).

# mousetrap 3.0.0

## Announcements
* We are delighted that Dirk Wulff and Jonas Haslbeck have joined the `mousetrap` team! They are contributing a number of new functions, particularly for clustering and visualization.
* The documentation of the `mousetrap` package can now also be found online at http://pascalkieslich.github.io/mousetrap/

## General changes to existing functions
* Introduction of class `mousetrap` for mousetrap data objects (such as `mt_example`). This facilitates, among other things, checking of the data class.
* Change of dimension order for all trajectory arrays: The old order was 1) trials, 2) variables, 3) samples. The new order is 1) trials, 2) samples, 3) variables. All functions and example data have been modified accordingly.
* Many reshape, aggregation, export, and plotting functions now accept a trajectory array as direct input to `data`.
* In case an analysis function adds measures to an existing data.frame, existing columns of the same name are now replaced and a warning is displayed (instead of merging the data.frames and adding generic suffixes).
* Arguments that were already deprecated and replaced (e.g., `show_progress` was replaced with `verbose`) have been removed. 

## Changes in specific functions
* `mt_align_start`: function is now vectorized and allows for optionally aligning to mean start position across
trials; default for `save_as` argument is set to `use`.
* `mt_space_normalize`: function is deprecated and replaced with `mt_align_start_end`. It offers similar functionality but is vectorized and allows for optionally aligning to mean start/end position across trials.
* `mt_resample`: now provides option to perform partial constant interpolation. Thanks to @sbrockhaus for the suggestion (cf. #7, #9).
* `mt_derivatives`: now provides option to additionally return timestamp differences.
* `mt_measures`: now optionally determines the number and duration of hovers (cf. #9), improved documentation and report of time measures (cf. #6).
* `mt_plot`: introduced  `facet_row` and `facet_col` arguments for faceting.
* `mt_plot_add_rect`: internal change to avoid warning message (due to changes in `ggplot2`).
* `mt_plot_riverbed`: explicitly remove zero frequencies instead of relying on the alpha parameter.

## New functions
* `read_mt`: read MouseTracker raw data (.mt files).
* `mt_align`: general purpose function for aligning and rescaling trajectories. For specific operations, you can rely on the specialized functions `mt_align_start` and `mt_align_start_end`.
* `mt_spatialize`: re-represent each trajectory spatially so that adjacent points become equidistant to each other.
* `mt_add_trajectory`: add a new trajectory to a trajectory array.
* `mt_bind`: join two trajectory arrays.
* `mt_count`: count the number of observations for each trajectory.
* `mt_angles`: calculate movement angles for trajectories.
* `mt_distmat`: compute the distance matrix for each pair of trajectories.
* `mt_cluster`: perform trajectory clustering with a specified number of clusters.
* `mt_cluster_k`: estimate the optimal number of clusters using various methods.
* `mt_map`: map trajectories onto a predefined set of prototype trajectories (a core set is provided in `mt_prototypes`).

## Bugs fixed
* `mt_measures`: make checks for timestamps > 0 and < 0 independent. Thanks to Regina Köhler for pointing this out.
* `mt_plot_per_trajectory`: fix bug that all trajectories were plotted on each page (introduced through previous change in `mt_reshape`). Thanks to Bence Palfi for discovering this.
* `create_results` (internal function): Explicitly select mt_id column (instead of assuming that it is the first column - which is, e.g., often not the case in `data[["data"]]`);  ensure for case `overwrite=FALSE` that function also works when multiple columns are merged and when all columns except mt_id are dropped beforehand.

## Removed functions
* `read_mousetracker`: removed as it is recommended to directly import the MouseTracker raw data using the new function `read_mt`.
* `mt_movement_angle`: removed as it is replaced with new and more general function `mt_angles`.
* `mt_calculate_derivatives`, `mt_calculate_deviations`, `mt_calculate_measures`: removed as they were previously deprecated and replaced with `mt_derivatives`, `mt_deviations`, `mt_measures`.


# mousetrap 2.0.0

## General changes to all existing functions
* Renamed `show_progress` argument to `verbose` (+ set default to `FALSE`).
* Introduced `dimensions` argument to explicitly specify the names of the columns in the trajectory array that contain the mouse positions. In most cases, the default is `c("xpos","ypos")` as the x- and y-positions should be used. Note that in some functions (as specified in the documentation) the order of the labels matters, the first value will be taken as the label of the x-positions, the second as the label of the y-positions.
* Introduced `timestamps` argument to explicitly specify the dimension in the trajectory array containing the timestamps.
* Perform merging of trial and trajectory data (e.g., in `mt_subset` and `mt_reshape`) based on the `rownames`. Therefore, the `mt_id` column in data.frames is not needed anymore - but is kept for the convenience of the user. The column is called `"mt_id"` in import and measures functions. `mt_reshape` allows the user to specify the label of the `mt_id` column explicitly.
* As a consequence of the previous changes: Removal of the internal variables `mt_variable_labels` and `mt_id`.
* Removed `calculate` from all mt_calculate functions: `mt_calculate_measures`, `mt_calculate_derivatives`, `mt_calculate_deviations` are now called `mt_measures`, `mt_derivatives`, `mt_deviations`.
* Introduction of internal utils function `create_results` (that simplifies including the newly created trajectories or measures in the existing mousetrap data object).
* Performance improvement of reshaping, aggregation, and visualization functions through internal changes in `mt_reshape`: `base::merge` and `reshape2` functions are replaced with functions from the `tidyr` and `dplyr` packages (and custom functions). Package dependencies were adjusted accordingly. As the `dplyr` functions may introduce additional classes for the reshaped data (such as `grouped_df` and `tbl_df`), a new argument (`convert_df`) is introduced that converts the reshaped data to pure data.frames by default (thereby dropping additional classes).

## Changes in specific functions
* `mt_import_mousetrap`, `mt_import_long`, `mt_import_wide`: Allow specifying several variables for the trial identifier in `mt_id_label`. A new ID variable will be created by combining the values of each variable.
* `mt_import_mousetrap`: Make import more robust against variables with empty logs (warning message is returned in the end) (closes #5, thanks to @sbrockhaus).
* `mt_import_long`, `mt_import_wide`: Import any number of additional variables using `add_labels` (closes #4).
* `mt_import_long`: Timestamps are no longer used for ordering if `mt_seq_label` is not provided. Instead, data will be imported in the order in which they were stored in `raw_data`.
* `mt_import_long`: Improved speed by relying on functions from the `tidyr` and `dplyr` packages.
* `mt_space_normalize`, `mt_align_start`: Introduction of `dimensions` argument and the corresponding arguments `start` and `end` (deprecate `xpos_start`/`xpos_end`/`ypos_start`/`ypos_end`). This also fixes the internal bug that in `mt_align_start` `xpos_start` was passed on as `ypos_start` to `mt_space_normalize` (instead of `ypos_start`).
* `mt_derivatives`: Rename `dimension` argument to `dimensions`, enable function  to work with an arbitrary number of dimensions.
* `mt_deviations`: Vectorized function `point_to_line` for time speed up (closes #2, thanks to @sbrockhaus).
* `mt_measures`: Allow for flexible dimension labels and rename all measures columns relating only to x- or y-positions depending on the values in dimensions (e.g., `x_max` becomes `xpos_max`, `x_flips` becomes `xpos_flips`). Change column label `xy_dist` to `total_dist`.
* `mt_measures`: Simplify AUC calculation using the actual x- and y-positions . New AUC values correlate to 1.00 with old values in `mt_example`, but in some cases extremely small differences are possible (maximum difference of 2.328e-10 in mt_example).
* `mt_reshape`, `mt_aggregate`, `mt_aggregate_per_subject`: Replace `aggregation_function` argument with `.funs`, which is passed on to the aggregation function(`summarize_at`). `.funs` also allows for specifying several aggregation functions.
* `mt_plot_per_trajectory`: New arguments `xlim` and `ylim` for specifying the axes limits explicitly and `axes_exact` for plotting exact axes.
* `mt_plot_per_trajectory`: New arguments `rect`, `color`, and `fill` for plotting rectangles (usually representing the response buttons).
* `mt_plot` (and related functions): New argument `points` allows for plotting points.

## New functions
* `mt_add_variables`: add new variables to trajectory array.
* `mt_export_long`: export mouse-tracking data in long format (wrapper for `mt_reshape`).
* `mt_export_wide`: export mouse-tracking data in wide format (wrapper for `mt_reshape`).

## Bugs fixed
* `mt_align_start`: Fixed bug that `xpos_start` (instead of `ypos_start`) was passed on as `ypos_start` to `mt_space_normalize`.
* `mt_average`: Fixed bug that if intervals were specified explicitly using `intervals`, the wrong interval size was used when averaging (the default size of 100 was used).

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
* Minor fix in documentation of x/y reversals (thanks to Barnabas Szaszi)

# mousetrap 1.0.0
* First release
