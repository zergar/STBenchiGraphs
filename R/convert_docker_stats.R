library(tidyr)
library(stringr)
library(dplyr)
library(readr)

#' Parse Docker Stats DataFrame
#'
#' @param df The dataframe containing splitted docker stats records
#' @param system_name the name of the system stats are requested for
#' @param stage the stage stats are requested for
#'
#' @return a dataframe containing the stats with only atomic values
#' @export
#'
#' @examples
#' # assume a docker stats dataframe called `df` already exists
#' stats <- parse_docker_stats(df, "postgis", "ingestion")
parse_docker_stats <- function(df, system_name, stage) {
  filter_container <-
    if_else(stage == "preprocess", "preprocess", system_name)

  parsed_df <- df %>%
    separate(MemUsage, c("MemUsage", "MemLimit"), sep = " / ") %>%
    separate(NetIO, c("NetIO_in", "NetIO_out"), sep = " / ") %>%
    separate(BlockIO, c("BlockIO_in", "BlockIO_out"), sep = " / ") %>%
    mutate(
      CPUPerc = parse_number(CPUPerc),
      MemUsage = as.numeric(convert_to_bytes_1024(MemUsage)),
      MemLimit = as.numeric(convert_to_bytes_1024(MemLimit)),
      NetIO_in = as.numeric(convert_to_bytes_1000(NetIO_in)),
      NetIO_out = as.numeric(convert_to_bytes_1000(NetIO_out)),
      BlockIO_in = as.numeric(convert_to_bytes_1000(BlockIO_in)),
      BlockIO_out = as.numeric(convert_to_bytes_1000(BlockIO_out))
    ) %>%
    mutate(stage = stage) %>%
    rename(timestamp_host = timestamp)

  filtered_df <- parsed_df %>%
    filter(str_detect(Name, filter_container))


  if (nrow(parsed_df) == 0) {
    return(parsed_df[FALSE,])
  } else {
    return(filtered_df)
  }
}



#' Convert prefixed byte string to bytes, conversion factor 1024
#'
#' @param values the size in bytes containing a unit
#'
#' @return the size in bytes
#' @export
#'
#' This function converts a string containing value and a byte size unit into the
#' corresponding raw byte value. It is assumed that 1024 byte = 1 kibibyte etc.
#'
#' Function copied from https://www.garysieling.com/blog/r-convert-from-gib-mib-kib-to-bytes/
#'
#' @examples
#' convert_to_bytes_1024("10 MiB")
#' convert_to_bytes_1024("2 KiB")
#' convert_to_bytes_1024("42 B")
convert_to_bytes_1024 <- function(values) {
  sapply(values, function(value) {
    digits <- gsub("[^0-9.]", "", value)
    units <- gsub("[0-9.]", "", value)
    multiplier <- switch (
      units,
      'GiB' = 1024 ^ 3,
      'MiB' = 1024 ^ 2,
      'KiB' = 1024,
      'B' = 1
    )

    as.double(digits) * multiplier
  })
}

#' Convert prefixed byte string to bytes, conversion factor 100
#'
#' @param values the size in bytes containing a unit
#'
#' @return the size in bytes
#' @export
#'
#' This function converts a string containing value and a byte size unit into the
#' corresponding raw byte value. It is assumed that 1000 byte = 1 kilobyte etc.
#'
#' Function adapted from https://www.garysieling.com/blog/r-convert-from-gib-mib-kib-to-bytes/
#'
#' @examples
#' convert_to_bytes_1000("10 MB")
#' convert_to_bytes_1000("2 kB")
#' convert_to_bytes_1000("42 B")
convert_to_bytes_1000 <- function(values) {
  sapply(values, function(value) {
    digits <- gsub("[^0-9.]", "", value)
    units <- gsub("[0-9.]", "", value)
    multiplier <- switch (
      units,
      'GB' = 1000 ^ 3,
      'MB' = 1000 ^ 2,
      'kB' = 1000,
      'B' = 1
    )

    as.double(digits) * multiplier
  })
}
