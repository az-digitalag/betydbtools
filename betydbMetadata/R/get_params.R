#' get_params
#'
#' Get parameters for a prepared statement
#'
#' @param row single row from google sheet (tibble with dim 1 x n)
#' @param fields names of fields for returned parameters;
#' to be used to subset row (character vector)
#'
#' @return parameters (unnamed list)
#' @export
#'
#' @examples
#' \dontrun{
#' get_params(row = tibble(name = 'MAC Season 8: Border', start_date = '2019-01-01',
#' end_date = '2019-03-31', description = NA, design = NA),
#' fields = c('name', 'start_date', 'end_date'))
#' }
get_params <- function(row, fields){
  params <- unname(as.list(row[fields]))
  return(params)
}

