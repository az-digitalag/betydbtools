#' Get new cultivars
#'
#' Only combinations of cultivar and specie id that have not yet been added to the BETYdb
#' cultivars table should be added when running add_cultivars(). get_new_cultivars() will
#' return subset of cultivars and specie ids from google sheet that are not present in
#' BETYdb and that should be added.
#'
#' The output of this function should be passed into add_cultivars().
#'
#' @param dbcon Database connection, as created by [RPostgres::dbConnect]
#' @param cultivars Cultivars google sheet object
#'
#' @return Combinations of cultivar and specie id to be added to BETYdb cultivars table (tibble)
#' @importFrom dplyr anti_join select collect tbl %>% filter
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
#' # cultivars sheet
#' cultivars <- gs_obj %>% gs_read(ws = 'cultivars')
#'
#' # get new cultivars to add to BETYdb
#' get_new_cultivars(dbcon, cultivars)
#'
#' }
#' @export
get_new_cultivars <- function(dbcon, cultivars){
  # create specie_id column in cultivars
  bety_species <- tbl(dbcon, 'species') %>% collect() # read in updated bety species table
  cultivars$specie_id <- vector('numeric', nrow(cultivars)) # create specie_id column
  for(i in 1:nrow(cultivars)){ # get specie id from specie name
    sci_name <- cultivars$species[i]
    spp_id <- bety_species %>%
      filter(.data$scientificname == sci_name) %>%
      select(.data$id)
    cultivars$specie_id[i] <- as.double(spp_id$id)
  }
  # get subset of cultivars to be uploaded
  bety_cultivars <- tbl(dbcon, 'cultivars') %>%
    select(.data$name, .data$specie_id) %>%
    collect()
  bety_cultivars$specie_id <- as.double(bety_cultivars$specie_id)
  new_cultivars <- anti_join(cultivars,
                             bety_cultivars,
                             by = c('name', 'specie_id'))
  return(new_cultivars)
}
