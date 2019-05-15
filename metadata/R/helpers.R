# get row fields that have user inputted value and
# is a field name in specific BETYdb table
#
# parses single row from experiments, sites, treatments, citations,
# and cultivars google sheet objects for fields that have a user inputted value
# and is a field in specific BETYdb table. get_fields() returns the names of these
# fields
#
# tbl_fields: Field names of specific BETYdb table (character, vector)
# row: Single row from google sheet object
#
# returns field names of row that have inputted data and is a field in specific BETYdb table
get_fields <- function(tbl_fields, row){
  com_ind <- which(!is.na(row))
  com_fields <- names(row)[com_ind]
  com_tbl_fields <- tbl_fields[tbl_fields %in% com_fields]
  return(com_tbl_fields)
}

# get parameters for parameterized statement
# subset row for parameter values according to fields.
#
# fields: Names of fields to subset row (character vector returned from get_fields)
# row: Single row from google sheet object
#
# returns unnamed list containing parameter values for parameterized statement
get_params <- function(row, fields){
  params <- unname(as.list(row[fields]))
  return(params)
}

# get parameterized insert statement for specific BETYdb table
#
# tbl_name: Name of BETYdb table to insert data into (character, length 1)
# fields: Names of fields of BETYdb table to add data to (character, vector); should
# be output returned from get_fields
#
# returns a parameterized insert statement
get_statement <- function(dbcon, tbl_name, fields){
  num_params <- length(fields)
  posit_params <- sapply(1:num_params, function(x) paste0('$', x))
  statement <- glue::glue_sql("insert into {DBI::SQL(tbl_name)}
                              ({DBI::SQL(paste(fields, collapse = ', '))})
                              values ({DBI::SQL(paste(posit_params, collapse = ', '))})",
                              .con = dbcon)
  return(statement)
}


# execute a PostgreSQL prepared statement
# this provides a safe and efficient way of executing a
# statement with a list of parameters to be substituted
# exits on success
prepared_statement <- function(con, query, params) {
  stopifnot(
    class(con) == "PqConnection",
    is.character(query),
    length(query) == 1,
    is.list(params)
  )
  qry <- DBI::dbSendStatement(con, query)
  res <- DBI::dbBind(qry, params)
  on.exit(DBI::dbClearResult(res))
}
