# clean out workspace
rm(list = ls())
gc()

# options
options(stringsAsFactors = FALSE)

# load packages
library(tidyverse)
library(neonUtilities)
library(ecocomDP)


# neonUtilities:::table_types %>% filter(grepl('mam',tableName))

# test with smammals
my_result <- ecocomDP::map_neon_data_to_ecocomDP(
  neon.data.product.id = "DP1.10072.001",
  site = c("MAYF", "PRIN"),
  startdate = "2016-1",
  enddate = "2018-11")

# test with Algae
my_result <- ecocomDP::map_neon_data_to_ecocomDP(
  neon.data.product.id = "DP1.20166.001",
  site = c("MAYF", "PRIN"),
  startdate = "2016-1",
  enddate = "2018-11",
  check.size = FALSE)

# checking tables
my_result$neon_metadata %>% head()

my_result$ecocomDP_tables$location %>% head()
my_result$ecocomDP_tables$taxon %>% head()

my_result$ecocomDP_tables$observation %>%
  dplyr::filter(!is.na(value)) %>%
  head()

my_result$ecocomDP_tables$observation_ancillary %>%  head()
