#' add_specie
#'
#' Add a specie to BETYdb species table
#'
#' @param species_to_add names of species to add to specie table (character vector)
#' @param dbcon Database connection, as created by [RPostgres::dbConnect]
#' @param commit value indicating whether to commit database transaction
#' (boolean: TRUE or FALSE; default is FALSE)
#'
#' @return exits silently on success
#' @export
#'
#' @examples
#' \dontrun{
#' add_specie(species_to_add = c('Sorghum bicolor'), dbcon = dbcon,
#' commit = FALSE)
#' }
add_specie <- function(species_to_add, dbcon, commit = FALSE){
  if(length(species_to_add) != 0){
    for(spp in species_to_add){
      DBI::dbBegin(dbcon)
      spp_insert <- glue::glue_sql("insert into species (scientificname) values ({spp})",
                                   .con = dbcon)
      ifelse(commit == TRUE, DBI::dbCommit(dbcon), DBI::dbRollback(dbcon))
    }
  }
}

