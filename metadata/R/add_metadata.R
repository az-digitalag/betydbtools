#' Upload new metadata to BETYdb
#'
#' Add new experiments, sites, treatments, cultivars, and citations to BETYdb. A user id
#' will need to be passed in when adding data to all tables but species and cultivars. Users can
#' choose to commit database transactions by setting the commit parameter to TRUE.
#'
#' @param dbcon Database connection, as created by [RPostgres::dbConnect]
#' @param commit Value indicating whether to commit database transactions (boolean, TRUE/FALSE)
#' @param user_id User id for BETYdb account (integer)
#' @param experiments Experiments google sheet object
#' @param sites Sites google sheet object
#' @param treatments Treatments google sheet object
#' @param cultivars Cultivars google sheet object
#' @param new_cultivars New cultivars with specie ids to add to BETYdb (tibble)
#' @param citations Citations google sheet object
#'
#' @examples
#' \dontrun{
#'
#' library(RPostgres)
#' library(googlesheets)
#' library(DBI)
#'
#' # connect to test database
#' dbcon <- dbConnect(RPostgres::Postgres(),
#' host = 'localhost',
#' user = 'bety',
#' port = 5433)
#'
#' # url to google sheet
#' url <- 'https://docs.google.com/spreadsheets/d/
#' 1BPkZPfynZI9DB9bRJfa335uC_uNjST5jHVf-XUgSVvM/edit#gid=347205052'
#'
#' # get key from URL
#' key <- extract_key_from_url(url)
#'
#' # register sheet
#' gs_obj <- key %>% gs_key(lookup = FALSE)
#'
#' # users sheet
#' users <- gs_obj %>% gs_read(ws = 'users')
#'
#' # experiments sheet
#' experiments <- gs_obj %>% gs_read(ws = 'experiments')
#'
#' # get user id from user login
#' bety_users <- tbl(dbcon, 'users') %>% collect()
#'
#' user_id <- bety_users %>%
#' filter(login == users$login) %>%
#' select(id)
#'
#' # add new experiments to BETYdb, commit database transaction
#' add_experiments(dbcon, commit = TRUE, user_id$id, experiments)
#'
#' }
#'
#' @name add_metadata
NULL

# row: single row from google sheet object
# tbl_name: name of BETYdb table to insert data into
# tbl_fields: name of fields of indicated BETYdb table that is also field name in row
add_row <- function(row, dbcon, tbl_name, tbl_fields, user_id = NULL, commit = FALSE){
  fields <- get_fields(tbl_fields, row)
  params <- get_params(row, fields)
  if(tbl_name != 'cultivars'){ # cultivars does not have user id column # has a previous id column
    fields <- append(fields, 'user_id')
    params <- append(params, as.double(user_id))
  }
  statement <- get_statement(dbcon, tbl_name, fields)
  DBI::dbBegin(dbcon)
  prepared_statement(dbcon, statement, params)
  ifelse(commit == TRUE, DBI::dbCommit(dbcon), DBI::dbRollback(dbcon))
}

#' @export
#' @rdname add_metadata
add_experiments <- function(dbcon, commit, user_id, experiments){

  # for each row of experiments, upload data using add_row
  apply(experiments,
        MARGIN = 1,
        add_row,
        dbcon = dbcon,
        tbl_name = 'experiments',
        tbl_fields = c('name', 'start_date', 'end_date',
                       'description', 'design'),
        user_id = user_id,
        commit = commit)
}

#' @export
#' @rdname add_metadata
add_sites <- function(dbcon, commit, user_id, sites){
  # for each row of sites, upload data using add_row
  apply(sites,
        MARGIN = 1,
        add_row,
        dbcon = dbcon,
        tbl_name = 'sites',
        tbl_fields = c('city', 'state', 'country',
                       'notes', 'sitename', 'greenhouse',
                       'geometry', 'time_zone'),
        user_id = user_id,
        commit = commit)
}

#' @export
#' @rdname add_metadata
add_treatments <- function(dbcon, commit, user_id, treatments){
  # for each row of treatments, upload data using add_row
  apply(treatments,
        MARGIN = 1,
        add_row,
        dbcon = dbcon,
        tbl_name = 'treatments',
        tbl_fields = c('name', 'definition', 'control'),
        user_id = user_id,
        commit = commit)
}

#' @export
#' @rdname add_metadata
add_species <- function(dbcon, cultivars){
  # need to determine which species to add
  bety_species <- tbl(dbcon, 'species') %>% collect()
  unq_spp <- unique(cultivars$species)
  spp_to_add <- unq_spp[!unq_spp %in% bety_species$scientificname]
  if(length(spp_to_add) != 0){
    statement <- glue::glue_sql("insert into species (scientificname) values ($1)",
                                .con = dbcon)
    for(spp in spp_to_add){
      params <- unname(as.list(spp))
      prepared_statement(dbcon, statement, params)
    }
  }
}

#' @export
#' @rdname add_metadata
add_cultivars <- function(dbcon, commit, new_cultivars){
  # for each row of new cultivars, upload data using add_row
  apply(new_cultivars,
        MARGIN = 1,
        add_row,
        dbcon = dbcon,
        tbl_name = 'cultivars',
        tbl_fields = c('name', 'specie_id', 'ecotype', 'notes'),
        commit = commit)
}

#' @export
#' @rdname add_metadata
add_citations <- function(dbcon, commit, user_id, citations){
  # for each row of citations, upload data using add_row
  apply(citations,
        MARGIN = 1,
        add_row,
        dbcon = dbcon,
        tbl_name = 'citations',
        tbl_fields = c('author', 'year', 'title',
                       'journal', 'volume', 'page',
                       'url', 'pdf', 'doi'),
        user_id = user_id,
        commit = commit)
}
