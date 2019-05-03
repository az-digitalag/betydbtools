#' associate_vals
#'
#' Associate two values in BETYdb
#'
#' @param tbl_name Name of BETYdb table (character, length 1)
#' @param col_1 Name of first id column of BETYdb table (character length 1)
#' @param col_2 Name of second id column of BETYdb table (character length 1)
#' @param val_1 First id value corresponding to column 1 (integer length 1)
#' @param val_2 Second id value corresponding to column 2 (integer length 1)
#' @param dbcon Database connection, as created by [RPostgres::dbConnect]
#' @param commit Value indicating whether to commit database transaction;
#' (boolean: TRUE or FALSE, default is FALSE)
#'
#' @return Exits on success
#' @export
#'
#' @examples
#' \dontrun{
#' associate_vals(tbl_name = 'experiment_sites', col_1 = 'experiment_id',
#' col_2 = 'site_id', val_1 = 900000004, val_2 = 900000178, commit = FALSE)
#' }
associate_vals <- function(tbl_name, col_1, col_2, val_1, val_2, dbcon, commit = FALSE){
  statement <- glue::glue_sql("insert into {DBI::SQL(tbl_name)}
                              ({DBI::SQL(paste(c(col_1, col_2), collapse = ', '))})
                              values ($1, $2)",
                              .con = dbcon)
  params <- list(val_1, val_2)
  DBI::dbBegin(dbcon)
  prepared_statement(dbcon, statement, params)
  ifelse(commit == TRUE, DBI::dbCommit(dbcon), DBI::dbRollback(dbcon))
}

