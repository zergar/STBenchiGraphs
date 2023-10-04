#' Plot Parameter Comparison for a Given Stage
#'
#'
#' This is the main drawing function for ST_Benchi. It takes a dataframe prepared by the
#' prepare_timings_data_comparison() function and can plot a wide variety of comparisons
#' depending on the set of parameters supplied. Different to the `plot_full_run()` function,
#' this function only plots a single stage at once, at the benefit of more fine-granular settings
#' for this stage.
#'
#' The function can group and facet the graph by different parameters.
#'
#' If a run fails to complete (e.g. due to a timeout or error), the function will draw
#' an "🗙" near the x axis.
#'
#'
#' @param df the dataframe prepared by the prepare_timings_data_comparison() function
#' @param selected_stage the stage the graph shall be drawn for
#' @param unique_params the set of unique parameters that shall be displayed on the x axis
#' @param recipe_order the order of the recipes within the graph
#' @param facet_by the column (usually a parameter column) to facet the graph on,defaults to no facets
#' @param fill_by the column (usually a parameter column) determining the fill color of the bars or lines, defaults to `system`
#' @param barplot whether to create a bar plot or a line plot, defaults to `TRUE`
#' @param NA_x_height the y position of the "🗙" indicating a failed run, defaults to `2`
#' @param fill_by_palette the color palette the bars shall be filled with, defaults to the ggplot2 standard palette
#'
#' @return a ggplot2 plot
#' @import ggplot2
#' @import dplyr
#' @importFrom ggpattern geom_bar_pattern scale_pattern_alpha_manual
#' @importFrom stringi stri_remove_empty_na
#' @importFrom stringr str_detect
#'
#' @examples
#' library(RColorBrewer)
#' systems_colors <- setNames(brewer.pal(n=5, name="Accent"),
#'   c("beast", "heavyAI", "postgis", "rasdaman", "sedona"))
#'
#' plot_stage_comparison(
#'   df,
#'   selected_stage = "execution",
#'   unique_params = c("system", "recipe"),
#'   facet_by = "vectorize_type",
#'   recipe_order = some_recipe_order_vector,
#'   fill_by = "system",
#'   fill_by_palette = systems_colors
#' )
#' @export
plot_stage_comparison <-
  function(df,
           selected_stage,
           unique_params,
           recipe_order = c(),
           facet_by = "",
           fill_by = "system",
           barplot = T,
           NA_x_height = 2,
           fill_by_palette = c()) {
    #create order for recipes
    if (length(recipe_order) >= 1) {
      df <- df %>%
        mutate(recipe = factor(recipe, levels = recipe_order, ordered = T)) %>%
        arrange(recipe)
    }

    df <- df %>%
      filter(stage == selected_stage) %>%
      mutate(
        stage = if_else(stage == "execution", paste(stage, exec_rev, sep = "_"), stage),
        dataset = if_else(dataset == "", "both", dataset)
      ) %>%
      dplyr::select(-exec_rev)


    # ensure that each paramter name only occurs once
    unique_params <- unique_params[unique_params != facet_by]
    unique_params <- unique_params[unique_params != fill_by]

    omit_x_lab <- F

    if (length(unique_params) == 0) {
      unique_params <- c("system")
      omit_x_lab <- T

    }

    # fix if a parameter is perceived numeric
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

    # make duration numeric
    df_plot <- df_plot %>%
      mutate(duration = as.numeric(duration)) %>%
      arrange(parameters)

    # only use warm starts
    if (selected_stage == "execution") {
      df_plot <- df_plot %>%
        filter(stage == "execution_warm")
    }

    if (facet_by != "") {
      df_plot <- df_plot %>%
        group_by(parameters, !!sym(fill_by), !!sym(facet_by))
    } else {
      df_plot <- df_plot %>%
        group_by(parameters, !!sym(fill_by))
    }

    # prepare statistics for data
    df_plot <- df_plot %>%
      summarise(
        duration_N = n(),
        duration_sd = sd(duration, na.rm = T),
        duration = mean(duration, na.rm = T),
        line_count = max(line_count)
      ) %>%
      mutate(duration_se = duration_sd / duration_N) %>%
      ungroup() %>%
      mutate(duration_na = is.na(duration)) %>%
      rowwise() %>%
      mutate(has_result_alpha = "fill") %>%
      mutate(has_result_alpha = ordered(has_result_alpha, levels = c("fill", "transp")))

    if (selected_stage == "execution") {
      df_plot <- df_plot %>%
        mutate(has_result_alpha = if_else((line_count <= 1), "transp", "fill"))
    }

    # initialize graph
    p <- df_plot %>%
      ggplot(aes(
        x = parameters,
        y = duration,
        group = !!sym(fill_by)
      ))

    # if plot should be a bar plot or a line graph
    if (barplot) {
      p <- p +
        geom_bar(
          position = "dodge",
          stat = "identity",
          color = "black",
          aes(fill = !!sym(fill_by), alpha = has_result_alpha)
        ) +
        scale_alpha_manual(
          guide = "none",
          values = c(0.1, 1),
          breaks = c("transp", "fill")
        )
    } else {
      p <- p +
        # geom_errorbar(aes(ymin=duration - duration_se, ymax=duration + duration_se, colour = !!sym(fill_by)), width=.2, position = position_dodge(width = 0.9)) +
        geom_line(aes(color = !!sym(fill_by))) +
        geom_point(aes(
          color = !!sym(fill_by),
          shape = !!sym(fill_by)
        ))
    }

    # if stage = execution, add 🗙 if run failed

    if (selected_stage == "execution") {
      p <- p +
        geom_text(
          mapping = aes(
            x = parameters,
            y = NA_x_height,
            group = !!sym(fill_by),
            color = !!sym(fill_by),
            label = if_else(show_x, "🗙", "")
          ),
          data = df_plot %>% dplyr::select(-duration,-duration_sd) %>% rowwise() %>%
            mutate(show_x = (
              duration_na ||
                has_result_alpha == "transp"
            )),
          position = position_dodge(width = 0.9)
        )
    }

    p <- p +
      theme(legend.position = "bottom") +
      theme(axis.title.x = element_blank())

    if (barplot) {
      p <- p +
        theme(axis.ticks.x = element_blank())
    }

    if (facet_by != "") {
      p <- p +
        facet_grid(cols = vars(!!ensym(facet_by)))

    }

    if (omit_x_lab) {
      p <- p +
        theme(axis.text.x = element_blank())
    }

    p <- p +
      labs(y = "Duration (sec)")

    if (length(fill_by_palette) > 0) {
      p <- p +
        scale_color_manual(values = fill_by_palette) +
        scale_fill_manual(values = fill_by_palette)
    }

    return(p)
  }
