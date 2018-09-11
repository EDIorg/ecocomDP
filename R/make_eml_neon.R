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
    )[complete.cases(x$location), ]
  
  geocov_data <- list(
    bounding_box = list(
      box_west = min(locations$longitude),
      box_east = max(locations$longitude),
      box_north = max(locations$latitude),
      box_south = min(locations$latitude)
      ),
    sites = locations
    )
  
  # Create geographic coverage
  
  list_of_coverage <- list()

  set_geo_coverage <- function(site, west, east, north, south){
    geographic_description <- new("geographicDescription", site)
    bounding_coordinates <- new("boundingCoordinates",
                                westBoundingCoordinate = as.character(west),
                                eastBoundingCoordinate = as.character(east),
                                northBoundingCoordinate = as.character(north),
                                southBoundingCoordinate = as.character(south))
    geographic_coverage <- new("geographicCoverage",
                               geographicDescription = geographic_description,
                               boundingCoordinates = bounding_coordinates)
    geographic_coverage
  }
  
  list_of_coverage <- mapply(
    set_geo_coverage,
    site = geocov_data$sites$location_id,
    west = geocov_data$sites$longitude,
    east = geocov_data$sites$longitude,
    north = geocov_data$sites$latitude,
    south = geocov_data$sites$latitude
  )
  
  geographic_description <- new("geographicDescription", 'Bounding box of sampling sites.')
  bounding_coordinates <- new("boundingCoordinates",
                              westBoundingCoordinate = as.character(geocov_data$bounding_box$box_west),
                              eastBoundingCoordinate = as.character(geocov_data$bounding_box$box_east),
                              northBoundingCoordinate = as.character(geocov_data$bounding_box$box_north),
                              southBoundingCoordinate = as.character(geocov_data$bounding_box$box_south))
  geographic_coverage <- new("geographicCoverage",
                             geographicDescription = geographic_description,
                             boundingCoordinates = bounding_coordinates)
  list_of_coverage[[(length(list_of_coverage)+1)]] <- geographic_coverage
  
  eml@dataset@coverage@geographicCoverage <- as(list_of_coverage, "ListOfgeographicCoverage")
  
  # Edit temporal coverage --------------------------------------------------
  
  message('<temporalCoverage>')
  
  dates <- lubridate::ymd_hm(x$observation$observation_datetime)
  
  eml@dataset@coverage@temporalCoverage <- as()
  
  temp <- set_coverage(
    begin = substr(floor_date(min(dates), unit = 'day'), 1, 10),
    end = substr(ceiling_date(max(dates), unit = 'day'), 1, 10)
  )
  eml@dataset@coverage@temporalCoverage <- as(
    temp@temporalCoverage,
    'ListOftemporalCoverage'
    )
  
  # Edit taxonomic coverage ---------------------------------------------------
  
  taxa_table <- select(spread(
    data = x$taxon, 
    key = taxon_rank, 
    value = taxon_name
    ),
    -taxon_id
  )
  if (sum(colnames(test) == '<NA>') > 0){
    taxa_table <- test[ , !colnames(test) == '<NA>']
  }
  
  taxcov <- list()
  for (i in 1:nrow(taxa_table)){
    taxa.row <- taxa_table[i, ]
    use_i <- !is.na(as.character(taxa.row))
    if (sum(use_i) > 0){
      taxcov[[i]] <- set_taxonomicCoverage(
        as.list(
          # taxa.row[1 , use_i, drop = F]
          taxa.row[use_i]
        )
      )
    } 
  }

  eml@dataset@coverage@taxonomicCoverage <- as(
    taxcov,
    'ListOftaxonomicCoverage'
  )
  
  #
  # Edit methods --------------------------------------------------------------
  
  #

}

