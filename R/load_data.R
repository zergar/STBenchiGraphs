#' Load Run Sets from DB
#'
#'
#' This is the main function for fetching latency data from the database. It uses
#' the benchmark set ids to identify relevant runs. If benchmarks are performed in
#' groups (e.g. using 2 raster and 3 vector datasets to perform 6 benchmark sets),
#' it is possible to load all six sets at the same time.
#'
#' The results are returned in long form regarding the stage.
#'
#' The duration returned is always the difference between the `start` and the `end` marker.
#' If a `pre` marker exists (as it occurs in some ingestion stages), this duration is
#' also returned and marked accordingly in the `ingest_dur_type` column. Similar, the
#' result also contains one row per cold/warm start.
#'
#' Each row of the returned dataframe also contains the parameters of each run.
#'
#'
#' @param conn the database connection
#' @param ... the set IDs to load
#'
#' @return the latency data asl wellas parameters for the different stages of a run
#' @import dplyr
#' @import dbplyr
#' @import tidyr
#' @importFrom tibble "add_column"
#' @importFrom purrr map_dfr map_lgl
#' @importFrom stringr regex str_detect str_remove str_replace
#' @export
#' @examples
#' conn <- dbConnect(duckdb::duckdb(), dbdir="results.db")
#' prepare_timings_data_comparison(conn, 23)
#' prepare_timings_data_comparison(conn, 17, 18, 19)
prepare_timings_data_comparison <- function(conn, ...) {
  set_ids <- list(...)

  # for each provided set id

  prepare_run <- function(set_id) {
    # get data from db and load all relevant markers

    run <- tbl(conn, "benchmark_run") %>%
      filter(benchmark_set == set_id) %>%
      inner_join(tbl(conn, "timings"), by = c("id" = "run_id")) %>%
      inner_join(tbl(conn, "parameters"), by = c("parameters" = "id")) %>%
      filter(marker == "benchi_marker") %>%
      rename(run_id = id) %>%
      collect()

    # create timestamps where no host timestamp exists by calculating and applying
    # the offset
    if (nrow(run %>% filter(event == "now")) > 0) {
      offset <- run %>%
        filter(event == "now") %>%
        mutate(diff = timestamp - controller_time) %>%
        pull(diff)

      run <- run %>%
        filter(event != "now") %>%
        mutate(timestamp = if_else(is.na(timestamp), controller_time - offset, timestamp))

    }

    # convert timestamps to wide form in order to create latency info later
    run <- run %>%
      dplyr::select(-controller_time)  %>%
      pivot_wider(names_from = event, values_from = timestamp)


    # add eventually missing cols for unnest_longer
    cols <- c(pre = NA_real_, mid = NA_real_, terminated = NA_real_)

    run <- run %>%
      add_column(!!!cols[setdiff(names(cols), names(run))])


    # because sometimes pivot_wider created multiple values, expand them
    run <- run %>%
      unnest_longer(c(pre, start, mid, end, terminated), keep_empty = T)

    # calculate latency
    run <- run %>%
      mutate(duration = end - start)

    # calculate special additional latencies for the splitted ingestion stage
    run_ingest <- run %>% filter(stage == "ingestion")

    run <- run %>%
      filter(stage != "ingestion") %>%
      mutate(ingest_dur_type = NA) %>%
      bind_rows(run_ingest %>%
                  mutate(duration = end - start,
                         ingest_dur_type = "system")) %>%
      dplyr::select(-any_of(c("pre", "start", "mid", "end")))

    run_ingest <- run_ingest %>%
      filter(!map_lgl(pre, is.null)) %>%
      unnest_longer(pre)

    run_full <- run_ingest %>% mutate(duration = end - pre,
                                      ingest_dur_type = "full") %>%
      dplyr::select(-any_of(c("pre", "start", "mid", "end"))) %>%
      drop_na(duration)

    run_pre <- run_ingest %>% mutate(duration = start - pre,
                                     ingest_dur_type = "pre") %>%
      dplyr::select(-any_of(c("pre", "start", "mid", "end"))) %>%
      drop_na(duration)

    run <- run %>%
      bind_rows(run_full) %>%
      bind_rows(run_pre)



    run <- run %>%
      dplyr::select(run_id,
                    parameters,
                    system,
                    stage,
                    dataset,
                    comment,
                    duration,
                    ingest_dur_type)

    # handle warm starts
    run <- run %>%
      filter(str_detect(stage, "execution")) %>%
      mutate(
        exec_rev = if_else(str_detect(stage, "0"), "cold", "warm"),
        warm_start_no = as.integer(str_remove(stage, "execution-")),
        stage = "execution"
      ) %>%
      bind_rows(run %>% filter(str_detect(stage, "execution", negate = T)))

    # aggregate markers from preprocess into single value for the complete stage
    run <- run %>%
      filter(stage == "preprocess" & dataset != "") %>%
      group_by(run_id, parameters, system, dataset) %>%
      summarise(duration = sum(duration)) %>%
      ungroup() %>%
      mutate(stage = "preprocess", comment = "") %>%
      bind_rows(run %>% filter(stage != "preprocess"))


    # remove some rows which may contain latency values not relevant to the performance
    # of the systems
    run <- run %>%
      filter(if_else(system == "sedona", stage != "ingestion", T)) %>%
      filter(if_else(system == "beast", stage != "ingestion", T)) %>%
      filter(if_else(system == "sedona" &
                       stage == "execution", comment == "outer", T)) %>%
      filter(if_else(system == "beast" &
                       stage == "execution", comment == "outer", T)) %>%
      mutate(comment = "")

    # average latency values for all warm starts
    run <- run %>%
      filter(stage == "execution") %>%
      group_by(run_id,
               parameters,
               system,
               dataset,
               exec_rev,
               warm_start_no) %>%
      summarise(duration = mean(duration)) %>%
      ungroup() %>%
      mutate(stage = "execution", comment = "") %>%
      bind_rows(run %>% filter(stage != "execution"))

    # remove the system anme, because it is readded by joining the parameters next
    run <- run %>%
      dplyr::select(-system)

    # add parameters table and clean up values
    run <- run %>%
      inner_join(tbl(conn, "parameters"),
                 by = c("parameters" = "id"),
                 copy = T) %>%
      mutate(across(
        where(is.character),
        ~ str_replace(
          .x,
          "VectorizationType.|VectorFileType.|RasterFileType.|DataType.|Stage.",
          ""
        )
      ))

    # add recipe name
    recipe_name <- tbl(conn, "benchmark_set") %>%
      filter(id == set_id) %>%
      pull(experiment)

    run <- run %>%
      mutate(recipe = str_remove(recipe_name, regex("(.compose){0,1}.ya{0,1}ml")),
             benchmark_set = set_id)


    # add results information
    run <- run %>%
      filter(stage == "execution") %>%
      inner_join(
        tbl(conn, "results"),
        by = c("run_id", "warm_start_no"),
        copy = T
      ) %>%
      mutate(yielded_result = line_count > 0) %>%
      bind_rows(run %>% filter(stage != "execution"))


    return(run)

  }

  result_runs <- set_ids %>%
    map_dfr(prepare_run)

  return(result_runs)
}
