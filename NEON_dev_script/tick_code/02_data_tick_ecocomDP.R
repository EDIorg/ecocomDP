# Reformat NEON Tick data to EcocomDP formatting
# Authors: Wynne Moss, Melissa Chen, Brendan Hobart, Matt Bitters
# Date: 2/11/2020

### Script to reformat NEON tick abundances
# uses the data from script 01_data_tick.R (downloads, cleans, links NEON tick data)
# follows same basic protocol as Mosquito data (Natalie Robinson) 

#### Load packages ####
# devtools::install_github("https://github.com/EDIorg/ecocomDP")
library(ecocomDP)
library(tidyverse)

#### Read in data ####
# see script 01 for details
tick_long <- readRDS("data/tck_longform.Rdata")
tick_site_env <- readRDS("data/tck_sitexspecies_env.Rdata")

#### Create location table ####

tick_location <- tick_site_env %>%
  select(namedLocation, decimalLatitude, decimalLongitude, elevation, siteID) %>%
  distinct() %>%
  rename(
    location_id = namedLocation,
    latitude = decimalLatitude,
    longitude = decimalLongitude,
    parent_location_id = siteID
  )
# 300 tick plots (matches the env data; each row corresponds to a tick drag plot)
nrow(tick_location)
length(unique(tick_location$location_id))
length(unique(tick_site_env$namedLocation))
length(unique(tick_site_env$plotID))

#### Create location ancillary table ####
tick_location_ancillary <- tick_site_env %>%
  select(namedLocation, domainID, siteID, plotID, plotType, nlcdClass) %>%
  distinct() %>% pivot_longer(domainID:nlcdClass, names_to = "variable_name", values_to = "value")%>%
  select(
    location_ancillary_id = namedLocation,
    location_id = namedLocation, 
    variable_name,
    value)
length(unique(tick_location_ancillary$location_id)) # 300


#### Create taxon table ####
tick_taxon <- tick_long %>%
  select(acceptedTaxonID, taxonRank, scientificName) %>%
  rename(taxon_id = acceptedTaxonID,
         taxon_rank = taxonRank,
         taxon_name = scientificName) %>%
  distinct() 


#### Create observation table ####
# NEON event ID is the sampling bout, not the unique tick drag (~5000 possible levels as of 7/2020)
# however abundances and remarks are at the drag level
# the event ID really should be the uid_field rather than the column "eventID" so that it links with ancillary table
# uid_field *would* be unique to each row if we were only measuring one taxon
# the fact that we have counts for multiple species/lifestages means that uid_field is repeated and can't serve as the unique observation_id

tick_long %>%
  select(uid_field, namedLocation, collectDate, Species_LifeStage, acceptedTaxonID, LifeStage, IndividualCount, totalSampledArea) %>%
  mutate(density = IndividualCount/totalSampledArea, package_id = "DP1.10093.001") %>%
  mutate(variable_name = paste(LifeStage, "density", sep = "_"), unit = "numberPerMeterSquared") %>%
  mutate(observation_id = row_number()) %>%
  select(observation_id, event_id = uid_field, package_id, location_id = namedLocation, observation_datetime = collectDate, 
         taxon_id = acceptedTaxonID, variable_name, value = density, unit) -> tick_observation_table

tick_observation_table %>% filter(is.na(value))
# lots of 0s 
nrow(tick_observation_table) 
length(unique(tick_long$uid_field))*length(unique(tick_long$Species_LifeStage)) # each row is density for a unique drag x lifestage x species

#### Create observation ancillary table ####  
tick_site_env %>%
  select(uid_field, targetTaxaPresent, totalSampledArea, samplingMethod, samplingProtocolVersion, remarks_field, countPending) %>%
  mutate(observation_ancillary_id = row_number(), totalSampledArea = as.character(totalSampledArea)) %>%
  pivot_longer(targetTaxaPresent:TickCount_Pending, names_to = "variable_name", values_to = "value") %>%
  mutate(unit = case_when(variable_name == "totalSampledArea" ~ "squareMeters")) %>%
  select(observation_ancillary_id, event_id = uid_field, variable_name, value, unit) -> tick_observation_ancillary

#### Create dataset summary table ####
# not sure yet how to do this? 
#### Write out data ####
readr::write_csv(
  tick_location,
  'data/tick_table_location.csv')

readr::write_csv(
  tick_location_ancillary,
  'data/tick_table_location_ancillary.csv')

readr::write_csv(
  tick_observation_table,
  'data/tick_table_observation.csv')

readr::write_csv(
  tick_observation_ancillary,
  'data/tick_table_observation_ancillary.csv')

readr::write_csv(
  tick_taxon,
  'data/tick_table_taxon.csv')


