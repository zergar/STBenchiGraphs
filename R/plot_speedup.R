#' Create Execution Time Comparison
#'
#'
#' This function was used in the past to easily compare cold starts and warm starts.
#' It has not been tested in a while and therefore may not work anymore
#'
#'
#' @param df the dataframe prepared by the prepare_timings_data_comparison() function
#' @param unique_params the set of unique parameters that shall be displayed on the x axis
#' @param get_plot wheter to return a plot or a dataframe
#'
#' @return a ggplot2 plot or a dataframe
#' @importFrom dplyr filter arrange collect mutate rename select tbl
#' @importFrom ggplot2 syms aes element_text geom_bar geom_hline ggplot theme
#' @importFrom lubridate interval time_length ymd
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' plot_exec_speedup(
#'   df,
#'   c("system", "recipe")
#' )
#'
#' plot_exec_speedup(
#'   df,
#'   c("system", "vectorize_type"),
#'   get_plot=F
#' )
#' @export
plot_exec_speedup <- function(df, unique_params, get_plot = T) {
  df_plot <- df %>%
    filter(stage == "execution") %>%
    mutate(duration = as.numeric(duration)) %>%
    pivot_wider(names_from = exec_rev, values_from = duration) %>%
    mutate(speedup = cold / warm) %>%
    dplyr::select(parameters, stage, speedup, !!!rlang::syms(unique_params))

  if (get_plot) {
    df_plot %>%
      mutate(parameters = paste(!!!rlang::syms(unique_params))) %>%
      ggplot(aes(x = parameters, y = speedup)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 0.9
      )) +
      geom_hline(yintercept = 1, color = "red")
  } else {
    df_plot %>%
      arrange(!!!rlang::syms(unique_params))
  }

}
