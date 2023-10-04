#' Plot the Complete Duration of All Stages for a Given Run
#'
#'
#' This is the other main drawing function for ST_Benchi. It takes a dataframe prepared by the
#' prepare_timings_data_comparison() function and can plot a wide variety of comparisons
#' depending on the set of parameters supplied. Different to the `plot_stage_comparison()` function,
#' this function can aggregate multiple stages into a single stacked bar plot.
#'
#' The function can group and facet the graph by different parameters.
#'
#' If a run fails to complete (e.g. due to a timeout or error), the function will draw
#' an "🗙" slightly above the bar of the failed run.
#'
#'
#'
#' @param df the dataframe prepared by the prepare_timings_data_comparison() function
#' @param unique_params the set of unique parameters that shall be displayed on the x axis
#' @param facet_by the column (usually a parameter column) to facet the graph on,defaults to no facets
#' @param fill_by the column (usually a parameter column) determining the fill color of the bars or lines, defaults to `system`
#' @param drop_stages a character vector of stages notto take into account
#' @param postgis_sel_ingest_dur_type the postgis ingestion type to use
#' @param use_warm whether to use warm or cold starts for the execution stage, defaults to `TRUE`
#' @param recipe_order the order of the recipes within the graph
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
#' #' library(RColorBrewer)
#' systems_colors <- setNames(brewer.pal(n=5, name="Accent"),
#'   c("beast", "heavyAI", "postgis", "rasdaman", "sedona"))
#'
#' plot_full_run(
#'   df,
#'   unique_params = c("system", "recipe"),
#'   facet_by = "recipe",
#'   drop_stages = c("preprocess"),
#'   recipe_order = some_recipe_order,
#'   fill_by_palette = systems_colors
#' )
#' @export
plot_full_run <-
  function(df,
           unique_params,
           facet_by = "recipe",
           fill_by = "system",
           drop_stages = c(),
           postgis_sel_ingest_dur_type = "full",
           use_warm = T,
           recipe_order = c(),
           NA_x_height = 2,
           fill_by_palette = c()) {
    # create sorting order
    if (length(recipe_order) >= 1) {
      df <- df %>%
        mutate(recipe = factor(recipe, levels = recipe_order, ordered = T)) %>%
        arrange(recipe)
    }


    #filter selected postgis ingestion latency type
    df <- df %>%
      filter(!(stage == "ingestion" & system == "postgis")) %>%
      bind_rows(
        df %>% filter(
          stage == "ingestion" &
            system == "postgis",
          ingest_dur_type == postgis_sel_ingest_dur_type
        )
      )


    stages <-
      rev(c(
        "preprocess",
        "ingestion",
        if_else(use_warm, "execution_warm", "execution_cold")
      ))

    # ensure that each paramter name only occurs once
    unique_params <- unique_params[unique_params != facet_by]
    unique_params <- unique_params[unique_params != fill_by]

    if (facet_by != "recipe" &&
        fill_by != "recipe" &&
        !"recipe" %in% unique_params) {
      unique_params <- unique_params %>%
        append("recipe")
    }

    # omit the x axis labels if there are no unique params left anymore
    omit_x_lab <- F

    if (length(unique_params) == 0) {
      unique_params <- c("system")
      omit_x_lab <- T

    }


    #calculate average latency for execution, sum latency for other stages
    df_plot <- df %>%
      filter(!stage %in% drop_stages) %>%
      mutate(
        stage = if_else(stage == "execution", paste(stage, exec_rev, sep = "_"), stage),
        dataset = if_else(dataset == "", "both", dataset)
      ) %>%
      group_by(recipe, parameters, stage) %>%
      summarise(duration = if_else(first(str_detect(
        stage, "execution"
      )), mean(duration), sum(duration)),
      line_count = max(line_count)) %>%
      ungroup()


    # create df including all selected params
    all_params <- unique_params %>%
      append(facet_by) %>%
      append(fill_by) %>%
      stri_remove_empty_na() %>%
      unique()

    print(all_params)

    # join params lost in previous group by
    # prepare data to include whether a stage did not yield a result
    df_plot <- df_plot %>%
      left_join(
        df %>%
          dplyr::select(parameters, !!!rlang::syms(all_params)) %>%
          distinct(),
        by = c("parameters", "recipe")
      ) %>%
      mutate(parameters = paste(!!!rlang::syms(unique_params)),
             duration = as.numeric(duration)) %>%
      filter(stage %in% stages) %>%
      mutate(stage = factor(stage, levels = stages)) %>%
      arrange(parameters, stage) %>%
      group_by(!!sym(fill_by), stage) %>%
      mutate(stage = recode(
        stage,
        execution_warm = "execution",
        execution_cold = "execution"
      )) %>%
      mutate(duration_na = is.na(duration), ) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(has_result_alpha = if_else((stage == "execution" &
                                           line_count <= 1), "transp", "fill"))

    # create plot
    p <- df_plot %>%
      ggplot(aes(x = parameters,
                 y = duration)) +
      geom_bar_pattern(
        aes(
          fill = !!sym(fill_by),
          pattern = stage,
          alpha = has_result_alpha,
          pattern_alpha = has_result_alpha
        ),
        color = "black",
        position = "stack",
        stat = "identity"
      ) +
      scale_alpha_manual(
        guide = "none",
        values = c(0.1, 1),
        breaks = c("transp", "fill")
      ) +
      scale_pattern_alpha_manual(
        guide = 'none',
        values = c(0.1, 1),
        breaks = c("transp", "fill")
      ) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
      ) +
      labs(y = "Duration (sec)")

    # if stage = execution, add 🗙 if run failed
    if (!("execution" %in% drop_stages)) {
      df_x <- df_plot %>%
        group_by(parameters,!!sym(fill_by),!!sym(facet_by)) %>%
        summarise(
          duration_nag = any(duration_na) |
            any(has_result_alpha == "transp"),
          NA_x_relheight = if_else(
            any(system == "beast" | system == "sedona"),
            NA_x_height,
            sum(duration, na.rm = T) + NA_x_height
          )
        )

      p <- p +
        geom_text(
          mapping = aes(
            x = parameters,
            y = NA_x_relheight,
            group = !!sym(fill_by),
            color = !!sym(fill_by),
            label = if_else(duration_nag, "🗙", "")
          ),
          data = df_x,
          position = position_dodge(width = 0.9)
        ) +
        guides(group = F,
               color = F,
               label = F)
    }


    if (facet_by != "") {
      p <- p +
        facet_grid(cols = vars(!!ensym(facet_by)), switch = "x")
    }

    if (length(fill_by_palette) > 0) {
      p <- p +
        scale_color_manual(values = fill_by_palette) +
        scale_fill_manual(values = fill_by_palette)
    }


    return(p)
  }
