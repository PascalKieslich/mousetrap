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