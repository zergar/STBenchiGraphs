% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_results.R
\name{plot_result_size}
\alias{plot_result_size}
\title{Plot Result Size Comparison}
\usage{
plot_result_size(
  df,
  unique_params,
  recipe_order = c(),
  facet_by = "",
  fill_by = "system",
  NA_x_height = 2,
  fill_by_palette = c()
)
}
\arguments{
\item{df}{the dataframe prepared by the prepare_timings_data_comparison() function}

\item{unique_params}{the set of unique parameters that shall be displayed on the x axis}

\item{recipe_order}{the order of the recipes within the graph}

\item{facet_by}{the column (usually a parameter column) to facet the graph on,defaults to no facets}

\item{fill_by}{the column (usually a parameter column) determining the fill color of the bars or lines, defaults to `system`}

\item{NA_x_height}{the y position of the "🗙" indicating a failed run, defaults to `2`}

\item{fill_by_palette}{the color palette the bars shall be filled with, defaults to the ggplot2 standard palette}
}
\value{
a ggplot2 plot
}
\description{
This is one of the results analysis functions for ST_Benchi. It takes a dataframe prepared by the
prepare_timings_data_comparison() function and can plot a wide variety of comparisons
depending on the set of parameters supplied. Different to the `plot_stage_comparison()` and
`plot_full_run()`functions, its results are beased on the length of the returned results (their line count)
and not the latency of the systems.
}
\details{
The function can group and facet the graph by different parameters.

If a run fails to complete (e.g. due to a timeout or error), the function will draw
an "🗙" slightly above the X axis.
}
\examples{
plot_result_size(
  df,
  unique_params = c("system", "band"),
  recipe_order = some_recipe_order,
  facet_by = "recipe",
  fill_by = "system",
  fill_by_palette = systems_colors,
  NA_x_height=30
)
}
