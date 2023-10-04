#' Convert prefixed byte string to bytes, conversion factor 1024
#'
#'
#' This function converts a string containing value and a byte size unit into the
#' corresponding raw byte value. It is assumed that 1024 byte = 1 kibibyte etc.
#'
#' Function adapted from https://www.garysieling.com/blog/r-convert-from-gib-mib-kib-to-bytes/
#'
#' @param values the size in bytes containing a unit
#'
#' @return the size in bytes
#' @importFrom stringr str_trim
#'
#' @examples
#' convert_to_bytes_1024("10 MiB")
#' convert_to_bytes_1024("2KiB")
#' convert_to_bytes_1024("42 B")
#' @export
convert_to_bytes_1024 <- function(values) {
  sapply(values, function(value) {
    digits <- gsub("[^0-9.]", "", value)
    units <- gsub("[0-9.]", "", value) %>%
      str_trim()

    multiplier <- switch (
      units,
      'TiB' = 1024 ^ 4,
      'GiB' = 1024 ^ 3,
      'MiB' = 1024 ^ 2,
      'KiB' = 1024,
      'B' = 1
    )

    as.double(digits) * multiplier
  })
}
