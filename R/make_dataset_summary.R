#' Make dataset_summary table
#'
#' @description  
#'     Make the ecocomDP dataset_summary table.
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
#' @param north
#'     (numeric) North bounding coordinate of the study area in decimal 
#'     degrees with values north of the equator being positive.
#' @param east
#'     (numeric) East bounding coordinate of the study area in decimal 
#'     degrees with values west of the prime meridian being negative.
#' @param south
#'     (numeric) South bounding coordinate of the study area in decimal 
#'     degrees with values south of the equator being negative.
#' @param west
#'     (numeric) West bounding coordinate of the study area in decimal 
#'     degrees with values west of the equator being negative.
#' @param evironment (character) EDI Data Repository environment in which the L0 exists, and which will be used to get the geographic bounding area from the EML metadata. Default is "production".
#'
#' @return 
#'     (data frame) A data frame of the dataset_summary table
#'     
#' @export
#'
make_dataset_summary <- function(parent.package.id, 
                                 child.package.id, 
                                 sample.dates, 
                                 taxon.table,
                                 north,
                                 east,
                                 south,
                                 west,
                                 environment = "production"){
  # FIXME: Inputs should be emls not package.ids
  message('Creating dataset_summary')
    
  # Validate inputs
  if (!is.character(sample.dates)){
    stop('Input argument "sample.dates" must be of class = character.')
  }

  # Initialize dataset_summary table
  dataset_summary <- data.frame(
    package_id = rep(NA_character_, length(child.package.id)),
    original_package_id = rep(NA_character_, length(child.package.id)),
    length_of_survey_years = rep(NA, length(child.package.id)),
    number_of_years_sampled = rep(NA, length(child.package.id)),
    std_dev_interval_betw_years = rep(NA, length(child.package.id)),
    max_num_taxa = rep(NA, length(child.package.id)),
    geo_extent_bounding_box_m2 = rep(NA, length(child.package.id)),
    stringsAsFactors = FALSE)
  
  # Add package_id and original_package_id
  dataset_summary$package_id <- child.package.id
  dataset_summary$original_package_id <- parent.package.id
  
  # Add temporal metrics
  dates <- dataCleanr::iso8601_read(sample.dates) # FIXME: Use lubridate instead
  if (is.integer(dates)) {
    dataset_summary$length_of_survey_years <- max(dates) - min(dates)
    dataset_summary$number_of_years_sampled <- length(unique(dates))
    dataset_summary$std_dev_interval_betw_years <- round(sd(diff(unique(dates))/1), 2)
  } else {
    dates <- dates %>% na.omit() %>% sort()
    # length_of_survey_years
    dataset_summary$length_of_survey_years <-  round(
      lubridate::time_length(
        lubridate::interval(min(dates), max(dates)), 
        unit = 'year'))
    # number_of_years_sampled
    dataset_summary$number_of_years_sampled <- length(
      unique(
        lubridate::year(dates)))
    # std_dev_interval_betw_years
    dataset_summary$std_dev_interval_betw_years <- round(
      sd(
        diff(
          lubridate::year(dates))),
      2)
  }
  
  # Add max_num_taxa
  dataset_summary$max_num_taxa <- nrow(taxon.table)
  
  # Add geo_extent_bounding_box_m2
  
  if (stringr::str_detect(
    parent.package.id, 
    "(^knb-lter-[:alpha:]+\\.[:digit:]+\\.[:digit:]+)|(^[:alpha:]+\\.[:digit:]+\\.[:digit:]+)")) {
    # Source is EDI
    eml <- suppressMessages(api_read_metadata(parent.package.id, environment = environment))
    west <- min(
      as.numeric(
        xml2::xml_text(
          xml2::xml_find_all(
            eml,
            "//dataset/coverage/geographicCoverage/boundingCoordinates/westBoundingCoordinate"))))
    east <- max(
      as.numeric(
        xml2::xml_text(
          xml2::xml_find_all(
            eml,
            "//dataset/coverage/geographicCoverage/boundingCoordinates/eastBoundingCoordinate"))))
    south <- min(
      as.numeric(
        xml2::xml_text(
          xml2::xml_find_all(
            eml,
            "//dataset/coverage/geographicCoverage/boundingCoordinates/southBoundingCoordinate"))))
    north <- max(
      as.numeric(
        xml2::xml_text(
          xml2::xml_find_all(
            eml,
            "//dataset/coverage/geographicCoverage/boundingCoordinates/northBoundingCoordinate"))))
  } else if (stringr::str_detect(x, "^DP.\\.[:digit:]+\\.[:digit:]+")) {
    # Source is NEON
    # TODO: Add methods for extracting this info from a NEON data product
  }

  dataset_summary$geo_extent_bounding_box_m2 <- round(
    calulate_geo_extent_bounding_box_m2(
      west, east, north, south))
  
  # Return
  dataset_summary
  
}








#' Calculate geo_extent_bounding_box_m2 of the dataset_summary table
#'
#' @param lon_west 
#'     (numeric) West longitude in decimal degrees and negative if west of the 
#'     prime meridian.
#' @param lon_east 
#'     (numeric) East longitude in decimal degrees and negative if west of the 
#'     prime meridian.
#' @param lat_north 
#'     (numeric) North latitude in decimal degrees.
#' @param lat_south 
#'     (numeric) North latitude in decimal degrees.
#'
#' @return
#'     (numeric) Area in square meters.
#'     
#' @export
#'
calulate_geo_extent_bounding_box_m2 <- function(
  lon_west, lon_east, lat_north, lat_south) {
  df <- data.frame(
    longitude = c(lon_west, lon_east, lon_east, lon_west),
    latitude = c(lat_north, lat_north, lat_south, lat_south))
  geosphere::areaPolygon(df)
}
