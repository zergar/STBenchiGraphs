#' Parse Docker Stats DataFrame
#'
#'
#' This function converts the output of `docker stats` into atomic values.
#'
#' @param df The dataframe containing splitted docker stats records
#' @param system_name the name of the system stats are requested for
#' @param stage the stage stats are requested for
#'
#' @return a dataframe containing the stats with only atomic values
#' @importFrom stringr str_detect
#' @importFrom tidyr separate
#' @importFrom dplyr filter if_else mutate rename
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
