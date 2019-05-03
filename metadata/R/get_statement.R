#' get_statement
#'
#' Get parameterized statement to insert data into specified BETYdb table
#'
#' @param dbcon Database connection, as created by [RPostgres::dbConnect]
#' @param tbl_name Name of BETYdb table (character, length 1)
#' @param fields Names of fields to include in statement (character vector)
#'
#' @return Parameterized sql statement to be passed into `prepared_statement` (character, length 1)
#' @export
#'
#' @examples
#' \dontrun{
#' get_statement(dbcon = dbcon, tbl_name = 'experiments',
#' fields = c('name', 'start_date', 'end_date', 'user_id'))
#' }
get_statement <- function(dbcon, tbl_name, fields){
  num_params <- length(fields)
  posit_params <- sapply(1:num_params, function(x) paste0('$', x))
  statement <- glue::glue_sql("insert into {DBI::SQL(tbl_name)}
                              ({DBI::SQL(paste(fields, collapse = ', '))})
                              values ({DBI::SQL(paste(posit_params, collapse = ', '))})",
                              .con = dbcon)
  return(statement)
}
                        
