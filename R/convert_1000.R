#' Convert prefixed byte string to bytes, conversion factor 1000
#'
#'
#' This function converts a string containing value and a byte size unit into the
#' corresponding raw byte value. It is assumed that 1000 byte = 1 kilobyte etc.
#'
#' Function adapted from https://www.garysieling.com/blog/r-convert-from-gib-mib-kib-to-bytes/
#'
#' @param values the size in bytes containing a unit
#'
#' @return the size in bytes
#' @importFrom stringr str_trim
#' @examples
#' convert_to_bytes_1000("10 MB")
#' convert_to_bytes_1000("2kB")
#' convert_to_bytes_1000("42 B")
#' @export

convert_to_bytes_1000 <- function(values) {
  sapply(values, function(value) {
    digits <- gsub("[^0-9.]", "", value)
    units <- gsub("[0-9.]", "", value) %>%
      str_trim()

    multiplier <- switch (
      units,
      'TB' = 1000 ^ 4,
      'GB' = 1000 ^ 3,
      'MB' = 1000 ^ 2,
      'kB' = 1000,
      'B' = 1
    )

    as.double(digits) * multiplier
  })
}
