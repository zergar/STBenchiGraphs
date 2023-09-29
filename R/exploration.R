library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


#' Get Timings and Parameter Information for a Certain Run
#'
#' @param conn the database connection
#' @param run_id_input the id of the run
#'
#' @return the dataframe containing timing information
#' @export
#'
#' This function prints the parameter and timings information for a given run id.
#' Its main purpose is to explore a dataset.
#'
#' @examples
#' get_run_timings(conn, 29)
get_run_timings <- function(conn, run_id_input) {
  stages <- c("preprocess", "ingestion", "execution")

  params <- tbl(conn, "benchmark_run") %>%
    filter(id == run_id_input) %>%
    rename(run_id = id) %>%
    dplyr::select(run_id, parameters) %>%
    merge(tbl(conn, "parameters"), by.x = "parameters", by.y = "id")

  timings_data <- tbl(conn, "timings") %>%
    filter(run_id == run_id_input) %>%
    rename(timestamp_host = timestamp) %>%
    collect() %>%
    mutate(timestamp_host = time_length(interval(ymd("1970-01-01"), timestamp_host)))

  print(params)
  print(timings_data)
}

#' Create Execution Time Comparison
#'
#' @param df the dataframe prepared by the prepare_timings_data_comparison() function
#' @param unique_params the set of unique parameters that shall be displayed on the x axis
#' @param get_plot wheter to return a plot or a dataframe
#'
#' @return a ggplot2 plot or a dataframe
#' @export
#'
#' This function was used in the past to easily compare cold starts and warm starts.
#' It has not been tested in a while and therefore may not work anymore
#'
#' @examples
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
