#' Connect to ST_Benchi Database
#'
#'
#' This function provides an easy way of connecting to the ST_Benchi results database.
#'
#' @param file the database file
#' @param readonly whether the file shall be opened read-only
#'
#' @return a database connection
#' @importFrom duckdb duckdb
#' @importFrom DBI dbConnect
#' @export
#'
#' @examples
#' connect_stbenchi_db("results.db")
connect_stbenchi_db <- function(file, readonly = T) {
  dbConnect(duckdb::duckdb(), dbdir = file, readonly = readonly)
}
