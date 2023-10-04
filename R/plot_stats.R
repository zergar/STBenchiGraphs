#' Plot Resource Utilization Graph for One Benchmark Run
#'
#'
#' This function is a wrapper around the @seealso [plot_docker_stats_single()] function,
#' which creates a single resource utilization graph for at most two variables.
#'
#'
#' @param conn the database connection
#' @param run_id_input the run id to plot
#' @param bench_param a character vector of at most 2 stats that shall be plotted
#' @param stage_names a character vector of stages to be plotted
#'
#' @return a ggplot2 plot of the two selected resource utilization metrics
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate ymd interval time_length
#' @importFrom tidyr pivot_longer separate
#' @export
#'
#' @examples
#' # for run with id 29, create the resource utilization graph for CPU and Memory usage
#' create_stage_graph(conn, 29, c("CPUPerc", "MemUsage"), c("ingestion"))
create_stage_graph <-
  function(conn, run_id_input, bench_param, stage_names) {

    tbl(conn, "benchmark_run") %>%
      filter(id == run_id_input) %>%
      rename(run_id = id) %>%
      dplyr::select(run_id, parameters) %>%
      merge(tbl(conn, "parameters"), by.x = "parameters", by.y = "id") %>%
      print()

    timings_data <- tbl(conn, "timings") %>%
      filter(run_id == run_id_input) %>%
      rename(timestamp_host = timestamp) %>%
      collect() %>%
      mutate(timestamp_host = time_length(interval(ymd("1970-01-01"), timestamp_host))) %>%
      separate(stage, c("stage", "exec_run")) %>%
      filter(stage %in% stage_names)


    stage_data <- tbl(conn, "resource_util") %>%
      filter(run_id == run_id_input) %>%
      collect() %>%
      mutate(stage = tools::file_path_sans_ext(stage)) %>%
      mutate(timestamp_host = time_length(interval(ymd("1970-01-01"), timestamp_host)))

    plot_docker_stats_single(stage_data, timings_data, bench_param, stage_names)
  }



