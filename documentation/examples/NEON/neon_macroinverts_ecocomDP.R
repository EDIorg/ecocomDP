library(tidyverse)

# Install and load devtools
# install.packages("devtools")
library(devtools)

# Install and load ecocomDP
# install_github("EDIorg/ecocomDP")

library(ecocomDP)

# Install and load neonUtilities
# install_github("NEONScience/NEON-utilities/neonUtilities", dependencies=TRUE)
library(neonUtilities)

#################################################################################
# macroinvert dpid
my_dpid <- 'DP1.20120.001'
my_site_list <- c('COMO', 'ARIK')

# get list of tables for the data product that is available with the neonUtilities package 
# we could probably just copy/hard-code this into ecocomDP to reduce dependencies
neon_data_table_names_and_info <- neonUtilities::table_types %>% filter(productID == my_dpid)

lookup_neon_data_availability(
  dpid = my_dpid,
  data_table_name = 'inv_fieldData'
)$siteID %>% unique()

# # download all field data -- could take a long time
# inv_fieldData <- ecocomDP::get_neon_datatable(
#   dpid = my_dpid,
#   data_table_name = 'inv_fieldData'
# )

# download field data for all dates for two neon sites -- much quicker 
inv_fieldData <- ecocomDP::get_neon_datatable(
  dpid = my_dpid,
  data_table_name = 'inv_fieldData',
  sample_location_list = my_site_list
)

# download macroinvert counts for two sites 
inv_taxonomyProcessed <- ecocomDP::get_neon_datatable(
  dpid = my_dpid,
  data_table_name = 'inv_taxonomyProcessed',
  data_package_type = 'expanded',
  sample_location_list = my_site_list
)


# REQUIRED TABLES

# location
table_location <- inv_fieldData %>%
  select(namedLocation, decimalLatitude, decimalLongitude, elevation) %>%
  distinct() %>%
  rename(
    location_id = namedLocation,
    latitude = decimalLatitude,
    longitude = decimalLongitude
  )

# taxon
table_taxon <- inv_taxonomyProcessed %>%
  select(acceptedTaxonID, taxonRank, scientificName) %>%
  distinct() %>%
  rename(taxon_id = acceptedTaxonID,
         taxon_rank = taxonRank,
         taxon_name = scientificName)

# observation
table_observation <- inv_taxonomyProcessed %>% 
  select(uid,
         sampleID,
         namedLocation, 
         collectDate,
         subsamplePercent,
         individualCount,
         estimatedTotalCount,
         acceptedTaxonID) %>%
  left_join(inv_fieldData %>% select(sampleID, benthicArea)) %>%
  mutate(variable_name = 'density',
         value = estimatedTotalCount / benthicArea,
         unit = 'count per square meter') %>% rename(observation_id = uid,
                                                     event_id = sampleID,
                                                     # package_id = NA,
                                                     location_id = namedLocation,
                                                     observation_datetime = collectDate,
                                                     taxon_id = acceptedTaxonID) %>%
  mutate(package_id = NA) %>%
  select(observation_id, event_id, package_id,
         location_id, observation_datetime,
         taxon_id, variable_name, value, unit)

# dataset_summary

getDistanceFromLatLonInKm<-function(lat1,lon1,lat2,lon2) {
  R <- 6371; # Radius of the earth in km
  dLat <- deg2rad(lat2-lat1);  # deg2rad below
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

date_times <- ymd_hm(table_observation$observation_datetime)

table_dataset_summary <- data.frame(
  package_id  = NA,
  original_package_id = NA,
  length_of_survey_years = diff(range(year(date_times))),
  number_of_survey_years = diff(range(year(date_times))),
  std_dev_interval_betw_years = sd(year(date_times)),
  max_num_taxa <- nrow(table_taxon),
  geo_extent_bounding_box = round(
    get_area_square_meters(
      min(table_location$longitude),
      max(table_location$longitude),
      max(table_location$latitude),
      min(table_location$latitude)
    )
  )
)

