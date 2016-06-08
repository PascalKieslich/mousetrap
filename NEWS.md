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