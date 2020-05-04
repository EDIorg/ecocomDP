# goal
# read an ecocomDP data package
# export dwc-a EML package
## Usage: basename(output type, packageId)

# Basic layout 
# 1. configure R, load libs
## > remotes::install_github("EDIorg/ecocomDP")
## > install.packages("tidyverse")

# 1.b config script
# eventually will support 2 DwC types: choose occurence-core or event-core. 
# occurence is simpler (sightings), event is a better fit for most of our data

# 2. load data
# TO DO: write a function that reads pkg metadata and dataTables. copy something from one of the L0 conversion scripts.

# 3. convert tables

# 4. export tables

# 5. construct EML 
# again, reuse functions from L1 build


# ------------------------------
# ------------------------------
# LOAD LIBRARIES
library(ecocomDP)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)

# ------------------------------
# ------------------------------
### SCRIPT CONFIG
# path set up, more to come.
parent_path <- "."
dwca_config_path = paste(sep="/", parent_path, "dwca_config")

# msg the user
message('using config files at: ', dwca_config_path)

# LOAD FUNCTIONS
# TO DO: learn good R practice - where should these live?
source("./R/func_create_tables_dwca_occurrence_core.R")


# PROCESS command line options (fixed for now, add these later)
darwin_core_type <- 'occurrence' 
# L1_pkg <- 'edi.193.3' 

# # load DwC layout and mappings for this type
# # load occurrence_config
# config is a 1-col table: table_name.field_name
dwca_occurrence_core_config <- read_csv('./dwca_config/dwca_occurrence_core_config.csv')

# # load occurrence_mapping
# csv showing how to fill each column in the config (6 cols, maybe reduced to 3 or 4)
dwca_occurrence_core_mapping <- read_csv('./dwca_config/dwca_occurrence_core_mappings.csv')


# # OR
# # config is a 1-col table: table_name.field_name
# dwca_occurrence_core_config <- read_csv('../../dwca_event_core_config.csv')

# # load event_mapping
# # csv showing how to fill each column in the config
# dwca_occurrence_core_mapping <- read_csv('../../dwca_event_core_mappings.csv')

# ------------------------------
# ------------------------------
### READ DATA
# using pastaprog code for now
source("./R/edi_193_3.R")

# assign tables to a var for that type (read pkg function will use their actual names, assign based on known str frag)
dt_obs <- dt1
dt_obs_ancil <- dt2
dt_loc_ancil <- dt3
dt_tax_ancil <- dt4
dt_summary <- dt5
dt_loc <- dt6
dt_tax <- dt7
# dt_var_mapping <- ___


# ------------------------------
# ------------------------------
### CONVERT ECOCOMDP ENTITIES TO DwC-A
# 

if (darwin_core_type == 'occurrence') {
  # call a function to create occurrence core. inputs: data objects, dwca_mappings, dwca_config
  print('calling function to create DwC-A, occurrence core')
  # temp_C <- fahrenheit_to_celsius(110)
  # print(temp_C)
  occurrence_table <- func_create_table_dwca_occurrence_core(
    dwca_occurrence_core_config,
    dwca_occurrence_core_mapping,
    dt_obs,
    dt_loc,
    dt_tax
  )
  
 
  
} else if (darwin_core_type == 'event') {
  # call a function to create event core. inputs: data objects, dwca_mappings, dwca_config  
  # print('calling function to create DwC-A, event core')

}
  

# ------------------------------
# ------------------------------
### ASSEMBLE DwC-A required entity: meta.xml
# 
# the meta.xml file is required, gbif cannot read EML attributeList, it reads meta.xml instead.


# ------------------------------
# ------------------------------
### ASSEMBLE EML METADATA
# 

# from original dataset, augmented for GBIF

# for occurrence core entity

# for meta.xml

# dataSource or other provenance

