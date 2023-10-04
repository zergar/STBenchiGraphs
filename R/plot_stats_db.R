#' Plot Single Resource Utilization Graph
#'
#'
#' This function is designed to plot different resource utilization metrics using
#' ggplot2. It uses a dataframe containing resource utilization metrics parsed by the
#' @seealso [parse_docker_stats()] function, and timing information directly from the database.
#'
#' It then plots at most two different metrics in the same graph, and will give two
#' different axis scales for the y axis. It will also color-code the two displayed metrics.
#' The code for this part is adapted from https://finchstudio.io/blog/ggplot-dual-y-axes/
#'
#' Additionally, the function will display every event supplied by the `df_timings` dataframe
#' and display them as horizontal markers, distinguished by different line styles.
#'
#' Together, these two graph types give an in-depth look into how the resource utilization of
#' systems-under-test changes depending on the stage of execution they are in.
#'
#'
#' @param df_docker the dataframe containing only atomic docker stats values
#' @param df_timings the dataframe containingevent markers from ST_Benchi
#' @param bench_params a character vector of at most 2 stats that shall be plotted
#' @param stage_names a character vector of stages to be plotted
#'
#' @return a ggplot2 plot of the two selected resource utilization metrics
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate ymd interval time_length
#' @importFrom tidyr pivot_longer separate
#'
#' @examples
#' # for the df_timings and the df_docker utilization, create the resource utilization
#' # graph for CPU and Memory usage
#' create_stage_graph(df_docker, df_timings, c("CPUPerc", "MemUsage"), c("ingestion"))
#' @export
plot_docker_stats_single <- function(df_docker, df_timings, bench_params, stage_names) {
  df_timings <- df_timings %>%
    filter(marker == "benchi_marker", comment != "inner") %>%
    mutate(event = ordered(event, levels = c("pre", "start", "mid", "end", "terminated")))

  time_start <- df_timings %>%
    filter(stage %in% stage_names,
           event == "start" | event == "pre") %>%
    summarise(min(timestamp_host)) %>%
    as.numeric()

  df_relevant <- df_docker %>%
    filter(stage %in% stage_names)

  docker_start <- df_relevant %>%
    summarise(min(timestamp_host)) %>%
    as.numeric()

  time_diff_h <- round((time_start - docker_start) / 3600)


  df_relevant <- df_relevant %>%
    mutate(timestamp_host = timestamp_host - time_start + time_diff_h * 3600)

  bench_param_left <- bench_params[1]
  bench_param_right <- bench_params[2]


  max_left <- max(df_relevant[[bench_param_left]])
  min_left <- min(df_relevant[[bench_param_left]])
  max_right <- max(df_relevant[[bench_param_right]])
  min_right <- min(df_relevant[[bench_param_right]])

  scale <- (max_right - min_right)/(max_left - min_left)
  shift <- min_left - min_right

  scale_function <- function(x, scale, shift){
    return ((x)*scale - shift)
  }

  # Function to scale secondary variable values
  inv_scale_function <- function(x, scale, shift){
    return ((x + shift)/scale)
  }

  bench_param_left <- sym(bench_param_left)
  bench_param_right <- sym(bench_param_right)

  print(bench_param_left)
  print(bench_param_right)

  df_long <- df_relevant %>%
    mutate(!!bench_param_right := inv_scale_function(!!bench_param_right, scale, shift)) %>%
    dplyr::select(timestamp_host, !!bench_param_left, !!bench_param_right) %>%
    pivot_longer(!timestamp_host, names_to = "metric", values_to = "value")


  df_long %>%
    ggplot(aes(x = timestamp_host, y = value, groups = metric)) +
    geom_line(aes(color = metric)) +
    ylab(label = bench_param_left) %>%
    scale_y_continuous(sec.axis = sec_axis(
      ~ scale_function(., scale, shift),
      labels = scales::label_bytes(),
      name = bench_param_right
    )) +
    geom_vline(
      df_timings %>%
        mutate(timestamp_host = timestamp_host - time_start) %>%
        filter(stage %in% stage_names),
      mapping = aes(
        xintercept = timestamp_host,
        group = dataset,
        linetype = event
      )
    ) +
    labs(x = "Time since first event in seconds") +
    theme(legend.position = "bottom")
}
