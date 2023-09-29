library(dplyr)
library(ggplot2)
library(stringr)

#' Get Latency of Benchmark Sets as Table
#'
#' @param df the dataframe prepared by the prepare_timings_data_comparison() function
#' @param unique_params the set of unique parameters that shall be returned as columns
#' @param recipe_order the order of the recipes within the table
#' @param group_results whether to aggregate the results by the unique params, defaults to `TRUE`
#'
#' @return a table of the latency values
#' @export
#'
#' This function takes a dataframe prepared by the prepare_timings_data_comparison() function
#' and returns the latency information formatted as a table.
#'
#' @examples
#' get_set_timings(
#'   df,
#'   unique_params = c("system", "recipe", "vectorize_type")
#' )
get_set_timings <-
  function(df,
           unique_params,
           recipe_order = c(),
           group_results = T) {
    all_stages = c("preprocess",
                   "ingestion",
                   "execution_cold",
                   "execution_warm")

    if (length(recipe_order) >= 1) {
      df <- df %>%
        mutate(recipe = factor(recipe, levels = recipe_order, ordered = T)) %>%
        arrange(recipe)
    }

    df <- df %>%
      mutate(
        stage = if_else(stage == "execution", paste(stage, exec_rev, sep = "_"), stage),
        dataset = if_else(dataset == "", "both", dataset)
      ) %>%
      mutate(stage = factor(stage, levels = all_stages)) %>%
      dplyr::select(parameters, stage, duration,!!!rlang::syms(unique_params))

    if (group_results) {
      df <- df %>%
        group_by(stage, !!!rlang::syms(unique_params)) %>%
        summarise(duration = if_else(first(str_detect(
          stage, "execution"
        )), mean(duration), sum(duration)))
    }

    df %>%
      arrange(!!!rlang::syms(unique_params))
  }
