library(DBI)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)


#' Load Run Sets from DB
#'
#' @param conn the database connection
#' @param ... the set IDs to load
#'
#' @return the latency data asl wellas parameters for the different stages of a run
#' @export
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
#'
#' @examples
#' conn <- dbConnect(duckdb::duckdb(), dbdir="results.db")
#' prepare_timings_data_comparison(conn, 23)
#' prepare_timings_data_comparison(conn, 17, 18, 19)
prepare_timings_data_comparison <- function(conn, ...) {
  set_ids <- list(...)

  prepare_run <- function(set_id) {
    run <- tbl(conn, "benchmark_run") %>%
      filter(benchmark_set == set_id) %>%
      inner_join(tbl(conn, "timings"), by = c("id" = "run_id")) %>%
      inner_join(tbl(conn, "parameters"), by = c("parameters" = "id")) %>%
      filter(marker == "benchi_marker") %>%
      rename(run_id = id) %>%
      collect()

    if (nrow(run %>% filter(event == "now")) > 0) {
      offset <- run %>%
        filter(event == "now") %>%
        mutate(diff = timestamp - controller_time) %>%
        pull(diff)

      run <- run %>%
        filter(event != "now") %>%
        mutate(timestamp = if_else(is.na(timestamp), controller_time - offset, timestamp))

    }

    has_mid <- "mid" %in% run$event
    has_pre <- "pre" %in% run$event

    run <- run %>%
      dplyr::select(-controller_time)  %>%
      pivot_wider(names_from = event, values_from = timestamp)

    if (has_mid) {
      run <- run %>%
        unnest_longer(c(start, mid, end))
    } else if (has_pre) {
      run <- run %>%
        unnest_longer(c(pre, start, end))
    } else if (has_pre && has_mid) {
      run <- run %>%
        unnest_longer(c(pre, start, mid, end))
    } else {
      run <- run %>%
        unnest_longer(c(start, end))
    }

    run <- run %>%
      mutate(duration = end - start)

    run_ingest <- run %>% filter(stage == "ingestion")

    run <- run %>%
      filter(stage != "ingestion") %>%
      mutate(ingest_dur_type = NA) %>%
      bind_rows(run_ingest %>%
                  mutate(duration = end - start,
                         ingest_dur_type = "system")) %>%
      dplyr::select(-any_of(c("pre", "start", "mid", "end")))

    if (has_pre) {
      run_ingest <- run_ingest %>%
        filter(!map_lgl(pre, is.null)) %>%
        unnest_longer(pre)

      run_full <- run_ingest %>% mutate(duration = end - pre,
                                        ingest_dur_type = "full") %>%
        dplyr::select(-any_of(c("pre", "start", "mid", "end")))

      run_pre <- run_ingest %>% mutate(duration = start - pre,
                                       ingest_dur_type = "pre") %>%
        dplyr::select(-any_of(c("pre", "start", "mid", "end")))

      run <- run %>%
        bind_rows(run_full) %>%
        bind_rows(run_pre)

    }

    run <- run %>%
      dplyr::select(run_id,
                    parameters,
                    system,
                    stage,
                    dataset,
                    comment,
                    duration,
                    ingest_dur_type)

    run <- run %>%
      filter(str_detect(stage, "execution")) %>%
      mutate(
        exec_rev = if_else(str_detect(stage, "0"), "cold", "warm"),
        warm_start_no = as.integer(str_remove(stage, "execution-")),
        stage = "execution"
      ) %>%
      bind_rows(run %>% filter(str_detect(stage, "execution", negate = T)))

    run <- run %>%
      filter(stage == "preprocess" & dataset != "") %>%
      group_by(run_id, parameters, system, dataset) %>%
      summarise(duration = sum(duration)) %>%
      ungroup() %>%
      mutate(stage = "preprocess", comment = "") %>%
      bind_rows(run %>% filter(stage != "preprocess"))


    run <- run %>%
      filter(if_else(system == "sedona", stage != "ingestion", T)) %>%
      filter(if_else(system == "beast", stage != "ingestion", T)) %>%
      filter(if_else(system == "sedona" &
                       stage == "execution", comment == "outer", T)) %>%
      filter(if_else(system == "beast" &
                       stage == "execution", comment == "outer", T)) %>%
      mutate(comment = "")

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

    run <- run %>%
      dplyr::select(-system)

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

    recipe_name <- tbl(conn, "benchmark_set") %>%
      filter(id == set_id) %>%
      pull(experiment)

    run <- run %>%
      mutate(recipe = str_remove(recipe_name, regex("(.compose){0,1}.ya{0,1}ml")),
             benchmark_set = set_id)


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
