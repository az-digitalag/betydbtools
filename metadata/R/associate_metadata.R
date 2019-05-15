#' Associate metadata added to BETYdb
#'
#' After new metadata has been added to BETYdb, associations must be made between experiments
#' and sites, experiments and treatments, sites and cultivars, and citations and sites.
#'
#' @param dbcon Database connection, as created by [RPostgres::dbConnect]
#' @param commit Value indicating whether to commit database transactions (boolean, TRUE/FALSE)
#' @param sites Sites google sheet object
#' @param treatments Treatments google sheet object
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
#' # sites sheet
#' sites <- gs_obj %>% gs_read(ws = 'sites')
#'
#' # associate experiments with sites in BETYdb, commit database transaction
#' associate_experiments_sites(sites, dbcon, commit = TRUE)
#'
#' }
#'
#'
#' @importFrom dplyr tbl collect filter select %>%
#' @importFrom rlang .data
#'
#' @name associate_metadata
NULL


# tbl_name : name of BETYdb table to associate values
# col_1 : name of first column of specified BETYdb table
# col_2: name of second column of specified BETYdb table
# val_1: value corresponding to first column
# val_2: value corresponding to second column
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

#' @rdname associate_metadata
#' @export
associate_experiments_sites <- function(dbcon, commit, sites){
  bety_sites <- tbl(dbcon, 'sites') %>% collect() # bety sites table
  bety_experiments <- tbl(dbcon, 'experiments') %>% collect() # bety experiments table
  new_site <- sites$sitename # new sites added
  for(site in new_site){
    row <- sites %>% filter(.data$sitename == site)
    site_id <- bety_sites %>%
      filter(.data$sitename == row$sitename) %>%
      select(.data$id)
    exp_id <- bety_experiments %>%
      filter(.data$name == row$experiment) %>%
      select(.data$id)
    associate_vals(tbl_name = 'experiments_sites',
                   col_1 = 'experiment_id',
                   col_2 = 'site_id',
                   val_1 = as.double(exp_id$id),
                   val_2 = as.double(site_id$id),
                   dbcon = dbcon,
                   commit = commit)
  }
}

#' @rdname associate_metadata
#' @export
associate_experiments_treatments <- function(dbcon, commit, treatments){
  bety_treatments <- tbl(dbcon, 'treatments') %>% collect() # bety treatments table
  bety_experiments <- tbl(dbcon, 'experiments') %>% collect() # bety experiments table
  new_treat <- treatments$name # new treatments
  for(treat in new_treat){
    row <- treatments %>% filter(.data$name == treat)
    treat_id <- bety_treatments %>%
      filter(.data$name == treat) %>%
      select(.data$id)
    associate_exp <- strsplit(row$experiment, split = ', ')[[1]]
    for(exp in associate_exp){
      exp_id <- bety_experiments %>%
        filter(.data$name == exp) %>%
        select(.data$id)
      associate_vals(tbl_name = 'experiments_treatments',
                     col_1 = 'experiment_id',
                     col_2 = 'treatment_id',
                     val_1 = as.double(exp_id$id),
                     val_2 = as.double(treat_id$id),
                     dbcon = dbcon,
                     commit = commit)
    }
  }
}

#' @rdname associate_metadata
#' @export
associate_sites_cultivars <- function(dbcon, commit, sites){
  bety_sites <- tbl(dbcon, 'sites') %>% collect() # bety sites table
  bety_species <- tbl(dbcon, 'species') %>% collect() # bety species table
  bety_cultivars <- tbl(dbcon, 'cultivars') %>% collect() # bety cultivars table
  new_site <- sites$sitename
  for(site in new_site){
    row <- sites %>% filter(.data$sitename == site)
    site_id <- bety_sites %>%
      filter(.data$sitename == row$sitename) %>%
      select(.data$id)
    spp_id <- bety_species %>%
      filter(.data$scientificname == row$species) %>%
      select(.data$id)
    cultivar_id <- bety_cultivars %>%
      filter(.data$name == row$cultivar & .data$specie_id == as.double(spp_id$id)) %>%
      select(.data$id)
    associate_vals(tbl_name = 'sites_cultivars',
                   col_1 = 'site_id',
                   col_2 = 'cultivar_id',
                   val_1 = as.double(site_id$id),
                   val_2 = as.double(cultivar_id$id),
                   dbcon = dbcon,
                   commit = commit)
  }
}

#' @rdname associate_metadata
#' @export
associate_citations_sites <- function(dbcon, commit, citations, sites){
  bety_citations <- tbl(dbcon, 'citations') %>% collect()
  bety_sites <- tbl(dbcon, 'sites') %>% collect()
  # get citation id (assuming only one citation)
  citation_id <- bety_citations %>%
    filter(.data$author == citations$author & .data$year == citations$year & .data$title == citations$title) %>%
    select(.data$id)
  new_site <- sites$sitename # new sites added
  for(site in new_site){
    row <- sites %>% filter(.data$sitename == site)
    site_id <- bety_sites %>%
      filter(.data$sitename == row$sitename) %>%
      select(.data$id)
    associate_vals(tbl_name = 'citations_sites',
                   col_1 = 'citation_id',
                   col_2 = 'site_id',
                   val_1 = as.double(citation_id$id),
                   val_2 = as.double(site_id$id),
                   dbcon = dbcon,
                   commit = commit)
  }
}
