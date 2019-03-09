#' Make dataset_summary table
#'
#' @description  
#'     Make the ecocomDP dataset_summary table.
#'
#' @usage 
#'     make_dataset_summary(parent.package.id, child.package.id, sample.dates,
#'     taxon.table)
#'
#' @param parent.package.id
#'     (character) Parent data package identifier composed of scope, 
#'     identifier, and revision (e.g. 'edi.101.1').
#' @param child.package.id
#'     (character) Child data package identifier composed of scope, 
#'     identifier, and revision (e.g. 'edi.101.1').
#' @param sample.dates
#'     (character) Sample dates from which to calculate date fields of the
#'     dataset_summary table.
#' @param taxon.table
#'     (data frame) The ecocomDP taxon table.
#'
#' @return 
#'     A data frame of the dataset_summary table.
#'     
#' @export
#'

make_dataset_summary <- function(parent.package.id, child.package.id, sample.dates, taxon.table){
  
  if (!is.character(sample.dates)){
    stop('Input argument "sample.dates" must be of class = character.')
  }
  
  message('Creating dataset_summary table')

  # Initialize dataset_summary table
  
  dataset_summary <- data.frame(
    package_id = rep(NA_character_, length(child.package.id)),
    original_package_id = rep(NA_character_, length(child.package.id)),
    length_of_survey_years = rep(NA, length(child.package.id)),
    number_of_years_sampled = rep(NA, length(child.package.id)),
    std_dev_interval_betw_years = rep(NA, length(child.package.id)),
    max_num_taxa = rep(NA, length(child.package.id)),
    geo_extent_bounding_box_m2 = rep(NA, length(child.package.id)),
    stringsAsFactors = FALSE
  )
  
  # Add content to fields
  
  dataset_summary$package_id <- child.package.id
  
  dataset_summary$original_package_id <- parent.package.id
  
  dates <- dataCleanr::iso8601_read(sample.dates)
  
  if (is.integer(dates)){

    dataset_summary$length_of_survey_years <- max(dates) - min(dates)
    
    dataset_summary$number_of_years_sampled <- length(unique(dates))

    dataset_summary$std_dev_interval_betw_years <- round(sd(diff(unique(dates))/1), 2)

  } else if (is.POSIXct(dates)){

    dates <- dates[order(dates)]
    dates <- dates[!is.na(dates)]
    dates_int <- interval(
      dates[1],
      dates[length(dates)]
    )
    
    dataset_summary$length_of_survey_years <-  round(
      lubridate::time_length(
        interval(
          min(dates),
          max(dates)
        ), 
        unit = 'year'
      ), 
      2
    )
    
    dataset_summary$number_of_years_sampled <- length(
      unique(
        lubridate::year(
          dates
        )
      )
    )
    
    dataset_summary$std_dev_interval_betw_years <- sd(
      diff(
        lubridate::year(
          dates
        )
      )
    )

  }
  
  dataset_summary$max_num_taxa <- nrow(taxon.table)
  
  eml <- suppressMessages(
    EDIutils::api_read_metadata(
      parent.package.id
    )
  )
  
  # Add column geo_extent_bounding box_m2 ---------------------------------
  # https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
  
  westBoundingCoordinate <- min(
    as.numeric(
      xml2::xml_text(
        xml2::xml_find_all(
          eml,
          "//dataset/coverage/geographicCoverage/boundingCoordinates/westBoundingCoordinate"
        )
      )
    )
  )

  eastBoundingCoordinate <- max(
    as.numeric(
      xml2::xml_text(
        xml2::xml_find_all(
          eml,
          "//dataset/coverage/geographicCoverage/boundingCoordinates/eastBoundingCoordinate"
        )
      )
    )
  )
  
  southBoundingCoordinate <- min(
    as.numeric(
      xml2::xml_text(
        xml2::xml_find_all(
          eml,
          "//dataset/coverage/geographicCoverage/boundingCoordinates/southBoundingCoordinate"
        )
      )
    )
  )
  
  northBoundingCoordinate <- max(
    as.numeric(
      xml2::xml_text(
        xml2::xml_find_all(
          eml,
          "//dataset/coverage/geographicCoverage/boundingCoordinates/northBoundingCoordinate"
        )
      )
    )
  )
  
  getDistanceFromLatLonInKm<-function(lat1,lon1,lat2,lon2) {
    R <- 6371; # Radius of the earth in km
    dLat <- deg2rad(lat2-lat1);	# deg2rad below
    dLon = deg2rad(lon2-lon1); 
    a <- sin(dLat/2) * sin(dLat/2) + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon/2) * sin(dLon/2) 
    c <- 2 * atan2(sqrt(a), sqrt(1-a)) 
    d <- R * c # Distance in km
    return(d)
  }
  
  deg2rad<-function(deg) {
    return(deg * (pi/180))
  }
  
  get_area_square_meters<-function(lon_west,lon_east,lat_north,lat_south){
    xdistN<-1000*getDistanceFromLatLonInKm(lat_north,lon_east,lat_north,lon_west) 
    xdistS<-1000*getDistanceFromLatLonInKm(lat_south,lon_east,lat_south,lon_west) 
    ydist<-1000*getDistanceFromLatLonInKm(lat_north,lon_east,lat_south,lon_east)
    area<-ydist*(xdistN+xdistS/2)
    return(area)
  }
  
  dataset_summary$geo_extent_bounding_box_m2 <- round(get_area_square_meters(westBoundingCoordinate,eastBoundingCoordinate,northBoundingCoordinate,southBoundingCoordinate))
  
  dataset_summary
  
}