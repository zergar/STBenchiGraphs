library(dplyr)
library(ggplot2)

#' Generate Factor for Combination of Parameters
#'
#' @param df the parsed dataframe
#' @param unique_params the parameters
#'
#' @return a character vector as an input for the factor
#' @export
#'
#' This helper function takes all values of a well-defined set of parameters and
#' creates the cross product of their unique values, concatenates each combination into
#' a single string and returns the resulting character vector of strings.
#'
#' @examples
generate_merged_factor <-
  function(df, unique_params) {
    params <- rlang::syms(unique_params)

    df %>%
      dplyr::select(!!!params) %>%
      distinct() %>%
      arrange(!!!params) %>%
      mutate(levels = paste(!!!params)) %>%
      pull(levels)
  }
