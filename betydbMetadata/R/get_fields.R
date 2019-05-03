#' get_fields
#'
#' Get names of fields for a row that have a value
#' and is also a field name in betydb table
#'
#' @param tbl_fields Names of fields of specific betydb table that are also field names of row;
#' used to subset row (character vector)
#' @param row Single row of a google sheet (tibble with dim 1 x n)
#'
#' @return Names of fields of row that have a user inputted value
#' and is a field name in specific betydb table (character vector)
#' @export
#'
#' @examples
#' \dontrun{
#' get_fields(tbl_fields = c('name', 'start_date', 'end_date', 'user_id'),
#' row = tibble(name = 'MAC Season 8: Border', start_date = '2019-01-01', end_date = '2019-03-01',
#' description = NA, design = NA))
#' }
get_fields <- function(tbl_fields, row){
  com_ind <- which(!is.na(row))
  com_fields <- names(row)[com_ind]
  com_tbl_fields <- tbl_fields[tbl_fields %in% com_fields]
  return(com_tbl_fields)
}

