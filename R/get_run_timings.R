#' Get Timings and Parameter Information for a Certain Run
#'
#'
#' This function prints the parameter and timings information for a given run id.
#' Its main purpose is to explore a dataset.
#'
#'
#' @param conn the database connection
#' @param run_id_input the id of the run
#'
#' @return the dataframe containing timing information
#' @importFrom dplyr filter arrange collect mutate rename select tbl
#' @importFrom ggplot2 syms aes element_text geom_bar geom_hline ggplot theme
#' @importFrom lubridate interval time_length ymd
#' @importFrom tidyr pivot_wider
#' @examples
#' get_run_timings(conn, 29)
#' @export

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

