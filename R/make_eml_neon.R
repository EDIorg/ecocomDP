#' make_eml_neon
#'
#' @description  
#'     Make an EML metadata record for a NEON data product that has been 
#'     reformatted into the ecocomDP.
#'
#' @usage make_eml_neon(eml, x)
#' 
#' @param eml
#'     (eml) An EML record for a NEON data product.
#' @param x
#'     (list of data frames) The ecocomDP tables for the NEON data product.
#'     Each item of the list must be named after the ecocomDP tables (e.g.
#'     observation, taxon, dataset_summary, etc.).
#' @param dp.id 
#'     (character) NEON data product ID
#'
#' @return 
#'     EML metadata for a NEON data product.
#'         
#' @export
#'

make_eml_neon <- function(
  eml,
  x,
  dp.id
  ){
  
  # Check input arguments -----------------------------------------------------
  
  # Edit title ----------------------------------------------------------------
  
  message('Editing:')
  
  message('<title>')
  
  title <- unlist(eml@dataset@title)
  
  title <- stringr::str_replace(
    title,
    ' at.*',
    ' (Reformatted to ecocomDP Design Pattern)'
    )
  
  title <- new(
    "title",
    title
    )
  
  eml@dataset@title <- as(list(title), "ListOftitle")
  
  # Edit abstract -------------------------------------------------------------
  
  message('<abstract>')
  
  lns <- paste0('This data package is formatted according to the "ecocomDP", a data package design pattern for ecological community surveys, and data from studies of composition and biodiversity. For more information on the ecocomDP project, contact EDI or see https://environmentaldatainitiative.org.',
                '\n',
                '\n',
                'This data package was derived from the NEON data product found here: ',
                paste0(
                  'http://data.neonscience.org/data-product-view?dpCode=',
                  dp.id
                  )
                )
  
  abstract <- as(set_TextType(text = lns), "abstract")
  
  eml@dataset@abstract <- abstract
  
  # Edit publication date -----------------------------------------------------
  
  message("<pubDate>")
  
  eml@dataset@pubDate <- as(format(Sys.time(), "%Y-%m-%d"), "pubDate")
  
  # Edit distribution ---------------------------------------------------------
  
  message("<distribution>")
  
  null_distribution <- list(NULL)
  
  eml@dataset@distribution <- as(null_distribution, "ListOfdistribution")
  
  # Edit geographic coverage --------------------------------------------------
  
  message('<geographicCoverage>')
  
  # Get bounding box info from location table
  
  locations <- unique.data.frame(
    select(
      x$location,
      location_id,
      latitude,
      longitude
      )
    )[complete.cases(locations), ]
  
  # 
  
  list_of_coverage <- list()
  
  if (!missing(geographic.coordinates) & !missing(geographic.description)){
    geographic_description <- new("geographicDescription", geographic.description)
    bounding_coordinates <- new("boundingCoordinates",
                                westBoundingCoordinate = as.character(geographic.coordinates[4]),
                                eastBoundingCoordinate = as.character(geographic.coordinates[2]),
                                northBoundingCoordinate = as.character(geographic.coordinates[1]),
                                southBoundingCoordinate = as.character(geographic.coordinates[3]))
    geographic_coverage <- new("geographicCoverage",
                               geographicDescription = geographic_description,
                               boundingCoordinates = bounding_coordinates)
    list_of_coverage[[(length(list_of_coverage)+1)]] <- geographic_coverage
  }

}
