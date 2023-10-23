# mousetrap 3.2.2

## Announcements
* A updated tutorial to movement tracking of psychological processes with the `mousetrap` R package has been published as a preprint. Please cite it as follows when using `mousetrap` in your research: Wulff, D. U.\*, Kieslich, P. J.\*, Henninger, F., Haslbeck, J. M. B., & Schulte-Mecklenbeck, M. (2023). _Movement tracking of psychological processes: A tutorial using mousetrap._ PsyArXiv. https://doi.org/10.31234/osf.io/v685r

## Bugs fixed
* `mt_import_long`: Preserve original trial order when importing trajectories. This also fixes the issue that trajectories are imported incorrectly when `mt_id_label` contains mixed case (closes #17, thanks to @LiKao)

## Internal changes
* Implemented a suggestion by CRAN team regarding the setup of the package overview help file (accommodating changes in roxygen2)


# mousetrap 3.2.1

## Announcements
* After more than 5 years, `mousetrap` finally has a logo (thanks to Dirk Wulff)

## General changes to existing functions
* Introduction of lifecycle badges for experimental and deprecated functions using the `lifecycle` package
* `mt_space_normalize` function that was already deprecated and replaced with `mt_align_start_end` has been removed

## New functions
* `mt_exclude_finish`: Remove a potential phase without mouse movement at the end of the trial

## Changes in specific functions
* `mt_average`: Removed `dimension` argument which was already deprecated and replaced with `av_dimension`

## Bugs fixed
* `mt_time_normalize` and `mt_resample`: Setting the `dimensions` argument to `"all"` now works (up to now, the functions simply returned an error when setting `dimensions = "all"`)
* `mt_angles`: Now also works for the edge case that the trajectory array only contains a single trajectory (closes #15)
* `mt_heatmap`: Now passes on the `verbose` argument to the internally used `mt_heatmap_raw` function to prevent messages from `mt_heatmap_raw` if `verbose = FALSE`

## Internal changes
* Added internal `extract_dimensions` function for extracting dimensions from trajectory array that also handles edge case of trajectory array with single trajectory


# mousetrap 3.2.0

## General changes to existing functions
* The `mt_spatialize` function has been replaced with `mt_length_normalize` (to achieve consistent naming analogous to `mt_time_normalize`). The new function offers the same functionality with the exception that the created trajectories are now by default called `ln_trajectories` (instead of `sp_trajectories`), which stands for length-normalized trajectories and should ensure consistency in naming with time-normalized trajectories.
* Because of the renaming of `sp_trajectories` to `ln_trajectories`, all functions that by default were using the `sp_trajectories` are now by default using `ln_trajectories` (e.g., `mt_cluster` and `mt_map`). Additionally, to ease the transition many mousetrap functions now internally flexibly exchange `ln_trajectories` and `sp_trajectories` in the `use` argument (if the specified trajectories are missing and the other trajectories are found) and throw a warning if they replace one with the other.

## Changes in specific functions
* `mt_spatialize`: function is deprecated and replaced with `mt_length_normalize`. The reason for this is to achieve consistent naming analogous to `mt_time_normalize`. The `mt_length_normalize` function offers the same functionality as `mt_spatialize` with the exception that the created trajectories are now by default called `ln_trajectories` (instead of `sp_trajectories`), which stands for length-normalized trajectories and should ensure consistency in naming with time-normalized trajectories.
* `mt_import_mousetrap`: Now can also import trajectory coordinates when they are stored in scientific notation in the raw data (e.g., `2.98e-8`).
* `mt_plot` and `mt_plot_aggregate`: Replaced `only_ggplot` with `return_type` argument. The new `return_type` argument allows for further customization of the plots: `If return_type == "plot"` (the default), a new ggplot is created and the trajectories are plotted using `geom_path` (corresponds to the old default behavior, where `only_ggplot` was set to `FALSE`). If `return_type == "mapping"`, only the ggplot object containing the mapping but without any geoms is returned (corresponds to the old behavior where `only_ggplot` was set to `TRUE`). If `return_type == "geoms"`, only the geoms are returned, which allows adding the plotted trajectories to an existing ggplot (new functionality).

## Bugs fixed
* `mt_exclude_initiation`: Bug fixed so that the correct initial position is used when removing the initial period without movement in a trial. Previously, `mt_exclude_initiation` was only behaving as intended when the initial position was (0,0), which probably was the case in many settings (in particular, if the trajectories were aligned using `mt_align_start` with default start values). If the initial position in a trial corresponded to a different value, the initial period without movement in most cases was simply not removed. This has now been fixed (closes #14).


# mousetrap 3.1.5

## Changes in specific functions
* `mt_measures`: Introduced `initiation_threshold` argument to allow specifying a minimum distance from the start point of the trajectory that needs to be exceeded for calculating the initiation time.
* `mt_import_mousetrap` and `mt_check_resolution`: Introduced `digits` argument. If specified, timestamps will be rounded which can potentially be useful if timestamps are recorded with submillisecond precision.
* `mt_import_mousetrap`: Introduced `unordered` argument that allows for different options to handle unordered, that is, non-monotonically increasing timestamps within a trial.
* `mt_plot` and `mt_plot_aggregate`: Introduced `wrap_var` and `wrap_ncol` arguments that add wrapping to trajectory plots (i.e., splitting a plot across multiple columns and rows based on the levels of a single variable or a combination of variables).
* `mt_plot_riverbed`: Introduced `na.rm` argument which, if set to `TRUE`, allows the creation of riverbed plots from trajectories with unequal length (whether this is in practice desirable or not is an open question).

## Internal changes
* `mt_import_long`: Replaced dplyr/tidyr standard evaluation functions by using embracing and pivoting.
* `mt_reshape`: Replaced dplyr/tidyr standard evaluation functions by using embracing and pivoting.
* `mt_plot_riverbed` and `mt_heatmap_ggplot`: Replaced geom_raster with geom_tile.
* Minor updates in documentation regarding function links and author information.


# mousetrap 3.1.4

## Announcements
* A book chapter on the mousetrap software packages has been published. It covers many common analyses using the mousetrap R package. Please cite it as follows when using mousetrap in your research:
* Kieslich, P. J., Henninger, F., Wulff, D. U., Haslbeck, J. M. B., & Schulte-Mecklenbeck, M. (2019). Mouse-tracking: A practical guide to implementation and analysis. In M. Schulte-Mecklenbeck, A. Kühberger, & J. G. Johnson (Eds.), _A Handbook of Process Tracing Methods_ (pp. 111-130). New York, NY: Routledge.
* Besides, if you use functions for clustering and mapping trajectories, please also include the following reference:
* Wulff, D. U., Haslbeck, J. M. B., Kieslich, P. J., Henninger, F., & Schulte-Mecklenbeck, M. (2019). Mouse-tracking: Detecting types in movement trajectories. In M. Schulte-Mecklenbeck, A. Kühberger, & J. G. Johnson (Eds.), _A Handbook of Process Tracing Methods_ (pp. 131-145). New York, NY: Routledge.

## Changes in specific functions
* `mt_measures`: Introduced `hover_incl_intial` argument to make inclusion of a potential initial phase without movement optional when calculating hovers. By default, this initial phase is included (as in previous versions).
* `mt_measures`: Multiple values can now be specified in the `hover_threshold` argument. If this is done, hovers and hover_time will be returned in separate variables for each threshold value (the variable name will be suffixed with the threshold value).
* `mt_measures`: Multiple values can now be specified in the `flip_threshold` argument. If this is done, flips (e.g., xpos_flips) will be returned in separate variables for each threshold value (the variable name will be suffixed with the threshold value).

## Internal changes
* When checking the class of an object, `inherits` is now used consistently instead of the `class` function to avoid problems with future R releases (>=4.0.0).


# mousetrap 3.1.3

## Changes in specific functions
* `mt_sample_entropy`: By default, sample entropy is calculated based on the differences of the position values (following Hehman et al., 2015). An optional argument `use_diff` now has been introduced to allow users to override this behavior and use the untransformed values instead, by setting `use_diff=FALSE`.
* `mt_align_start_end`: Now checks, if start and end points are equal for a trial (separately per dimension). If so, returns a warning message as the aligned trajectory values for the respective dimension will be `NaN`/`Inf`/`-Inf`.

# mousetrap 3.1.2

## Changes in documentation
* `citation("mousetrap")` now returns the correct citation for the `mousetrap` package.
* Updated mousetrap book chapter citations (see announcements above for correct citation).

## Changes in specific functions
* `mt_import_long`: internal change (prefix `row_number()` function with dplyr) to accomodate changes in dplyr 0.8.0.

# mousetrap 3.1.1

## Announcements
* Book chapters on the mousetrap software packages have been accepted for publication. See version 3.1.3 above for updated references (as the book chapters meanwhile have been published).
* If you have any questions when using mousetrap, you can now ask them in the mousetrap forum at https://forum.cogsci.nl/index.php?p=/categories/mousetrap

## Changes in specific functions
* `mt_map`: now allows for mapping trajectories onto prototypes separately for different groups of trajectories (the prototypes will be rescaled separately to match the coordinate system of each group).
* `mt_heatmap_ggplot`: now allows for faceting using the `facet_row` and `facet_col` arguments.
* `mt_diffmap`: the `condition` can now simply be specified by providing the corresponding variable name (and the condition values can now be any type, provided that they only contain two levels).
* `mt_plot` and `mt_plot_aggregate`: transparency and line width can now be varied via the `alpha` and `size` arguments.
* `mt_plot_riverbed`: introduced `grid_colors` argument for setting the grid colors (use `grid_colors = NA` to omit grid lines).
* `mt_reshape`: internal changes reflecting changes in `dplyr::summarize_at`.
* `mt_align_start` and `mt_align_start_end`: if `start` is computed internally, it is now ensured that it is a named vector.
* `mt_plot_add_rect`: it is now ensured that this function does not influence the legend (previously, this could happen if the `linetype` argument was used in `mt_plot`)

# mousetrap 3.1.0

## Announcements
* A first paper on the mousetrap software packages has been published. It presents the mousetrap plugin for creating mouse-tracking experiments in OpenSesame and also includes a short demonstration of basic analyses using the mousetrap R package. Please cite it as follows when using the mousetrap plugin in your research.
* Kieslich, P. J., & Henninger, F. (2017). Mousetrap: An integrated, open-source mouse-tracking package. _Behavior Research Methods, 49_(5), 1652-1667. https://doi.org/10.3758/s13428-017-0900-z
* The reference for the mousetrap R package has been updated as Zenodo now supports version independent DOIs.
* Kieslich, P. J., Wulff, D. U., Henninger, F., Haslbeck, J. M. B., & Schulte-Mecklenbeck, M. (2016). Mousetrap: An R package for processing and analyzing mouse-tracking data. https://doi.org/10.5281/zenodo.596640

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

## New data
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
