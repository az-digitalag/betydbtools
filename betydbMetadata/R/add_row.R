#' add_row
#'
#' Add a row of data from google sheet into specified BETYdb table
#'
#' @param row Single row from google sheet (tibble of dim 1 x n)
#' @param dbcon Database connection, as created by [RPostgres::dbConnect]
#' @param tbl_name Name of BETYdb table (character, length 1)
#' @param tbl_fields Names of fields of BETYdb table that are also field names of row;
#' used to subset row (character vector)
#' @param user_id BETYdb user id; only needed when inserting new experiments;
#' default = NULL (integer length 1)
#' @param commit Value indicating whether to commit database transaction;
#' (boolean: TRUE or FALSE, default is FALSE)
#'
#' @return exits on success
#' @export
#'
#' @examples
#' \dontrun{
#' add_row(row = tibble(name = 'MAC Season 8: Border', start_date = '2019-01-01',
#' end_date = '2019-03-31'), dbcon = dbcon, tbl_name = 'experiments',
#' tbl_fields = c('name', 'start_date', 'end_date', 'design', 'description'), user_id = 6000000004,
#' commit = FALSE)
#' }
add_row <- function(row, dbcon, tbl_name, tbl_fields, user_id = NULL, commit = FALSE){
  #user_id only required for experiment upload

  fields <- get_fields(tbl_fields, row)
  params <- get_params(row, fields)
  # extra addition needed to insert data into experiments
  if(tbl_name == 'experiments'){
    fields <- append(fields, 'user_id')
    params <- append(params, as.double(user_id)) #need to create this in experiments chunk
  }
  statement <- get_statement(dbcon, tbl_name, fields)
  DBI::dbBegin(dbcon)
  prepared_statement(dbcon, statement, params)
  ifelse(commit == TRUE, DBI::dbCommit(dbcon), DBI::dbRollback(dbcon))
}

