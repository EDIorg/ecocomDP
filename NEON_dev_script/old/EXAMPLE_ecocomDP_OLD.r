# clean out workspace
rm(list = ls())
gc()

# options
options(stringsAsFactors = FALSE)

# load packages
library(tidyverse)

# install ecocomDP from github
devtools::install_github('EDIorg/ecocomDP')

# load package
library('ecocomDP')

# view help files for neon mapping function
?ecocomDP::map_neon_data_to_ecocomDP



# Retrieve and map NEON ALGAE data to ecocomDP format
# for Algae
my_result <- ecocomDP::map_neon_data_to_ecocomDP(
  neon.data.product.id = "DP1.20166.001",
  site = c("MAYF", "PRIN"),
  startdate = "2016-1",
  enddate = "2018-11",
  check.size = FALSE)



# see names of items in returned list
names(my_result)



# view some info about the data product
my_result$neon_metadata %>% names()
my_result$neon_metadata$siteCodes


# view ecocomDP tables
my_result$ecocomDP_tables %>% names()
# [1] "location"              "taxon"                 "observation"           "observation_ancillary"

# location tables
my_result$ecocomDP_tables$location

# taxon tables first 5 rows
my_result$ecocomDP_tables$taxon %>% head()

# taxon tables first 5 rows
my_result$ecocomDP_tables$taxon %>% head()

# observation tables first 5 rows where values != NA
my_result$ecocomDP_tables$observation %>% 
  filter(!is.na(value)) %>% 
  head()

# observation_ancillary tables first 5 rows
my_result$ecocomDP_tables$observation_ancillary %>% head()
