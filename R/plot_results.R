library(dplyr)
library(ggplot2)

source("R/helper.R")

#' Plot Result Size Comparison
#'
#' @param df the dataframe prepared by the prepare_timings_data_comparison() function
#' @param unique_params the set of unique parameters that shall be displayed on the x axis
#' @param recipe_order the order of the recipes within the graph
#' @param facet_by the column (usually a parameter column) to facet the graph on,defaults to no facets
#' @param fill_by the column (usually a parameter column) determining the fill color of the bars or lines, defaults to `system`
#' @param NA_x_height the y position of the "🗙" indicating a failed run, defaults to `2`
#' @param fill_by_palette the color palette the bars shall be filled with, defaults to the ggplot2 standard palette
#'
#' @return a ggplot2 plot
#' @export
#'
#' @examples
plot_result_size <-
  function(df,
           unique_params,
           recipe_order = c(),
           facet_by = "",
           fill_by = "system",
           NA_x_height = 2,
           fill_by_palette = c()) {
    if (length(recipe_order) >= 1) {
      df <- df %>%
        mutate(recipe = factor(recipe, levels = recipe_order, ordered = T)) %>%
        arrange(recipe)
    }

    df <- df %>%
      filter(stage == "execution", warm_start_no == 1)


    unique_params <- unique_params[unique_params != facet_by]
    unique_params <- unique_params[unique_params != fill_by]

    omit_x_lab <- F

    if (length(unique_params) == 0) {
      unique_params <- c("system")
      omit_x_lab <- T

    }


    if (length(unique_params) == 1 &&
        df %>% pull(!!sym(unique_params)) %>% is.numeric()) {
      df_plot <- df %>%
        mutate(parameters = !!sym(unique_params))
    } else {
      df_plot <- df %>%
        mutate(parameters = ordered(
          paste(!!!rlang::syms(unique_params)),
          generate_merged_factor(df, unique_params)
        ))
    }

    df_plot <- df_plot %>%
      arrange(parameters)


    if (facet_by != "") {
      df_plot <- df_plot %>%
        group_by(parameters,!!sym(fill_by),!!sym(facet_by))
    } else {
      df_plot <- df_plot %>%
        group_by(parameters,!!sym(fill_by))
    }

    p <- df_plot %>%
      ggplot(aes(
        x = parameters,
        y = line_count,
        group = !!sym(fill_by)
      )) +
      geom_bar(position = "dodge", stat = "identity", aes(fill = !!sym(fill_by))) +
      geom_text(
        mapping = aes(
          x = parameters,
          y = NA_x_height,
          group = !!sym(fill_by),
          color = !!sym(fill_by),
          label = if_else(file_exists, if_else(line_count %in% 0:1, "0", ""), "🗙")
        ),
        data = df_plot,
        position = position_dodge(width = 0.9)
      )
    if ("max_size" %in% colnames(df_plot)) {
      p <- p +
        geom_hline(aes(yintercept = max_size, group = recipe))
    }

    p <- p +
      theme(legend.position = "bottom") +
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_blank()) +
      theme(axis.text.x = element_text(
        angle = 90,
        vjust = 0,
        hjust = 1
      ))

    if (facet_by != "") {
      p <- p +
        facet_grid(cols = vars(!!ensym(facet_by)))
    }

    if (omit_x_lab) {
      p <- p +
        theme(axis.text.x = element_blank())
    }

    p <- p +
      labs(y = "Result Lines Count")

    if (length(fill_by_palette) > 0) {
      p <- p +
        scale_color_manual(values = fill_by_palette) +
        scale_fill_manual(values = fill_by_palette)
    }

    return(p)

  }
