# Make data tables for ecocomDP from map.txt.

# this R script reads in a NTL dataset from the LTER repository
# converts it to the most flexible data model and writes it out with EML as datapackage

# Set up most flexible precursor data model (MFP)
# Observations.csv : "observation_id", "event_id", "study_id", "sampling_location_id", "datetime", "taxon_id", "variable_name", "value", "unit"
# Sampling_location.csv: "sampling_location_id", "sampling_location_name", "latitude", "longitude", "elevation", "parent_sampling_location_id"
# Sampling_location_description.csv: "sampling_location_id", "datetime", "variable_name", "value", "unit"
# Taxon.csv: "taxon_id", "taxon_level", "taxon_name", "authority_system", "authority_taxon_id"
# Taxon_descr.csv: "taxon_id", "datetime", "variable_name", "value", "author"
# Event_data.csv: "event_id", "variable_name", "value"
# Study.csv: "study_id", "taxon_group", "ecological_group", "intent", "study_design", "methods", "original_dataset_id"

# Requires --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(EML)

# Parameters ------------------------------------------------------------------

package_id <- "ntl.7.17"

working_dir <- "/Users/csmith/Desktop/popcom/knb-lter-ntl.7.17"

# Get the data file from the repository ---------------------------------------

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/7/17/307b9c3bb1bb42825c3514086fe43acc"

infile1 <- sub("^https",
               "http",
               infile1) 

dt1 <-read.csv(infile1, 
               header=TRUE, 
               sep=",") 

#tmpDateFormat<-"%Y"
#dt1$year4<-as.Date(dt1$year4,format=tmpDateFormat, origin = "1900-01-01")

# Get corresponding EML file --------------------------------------------------

f <- "http://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-ntl.7.17&contentType=application/xml"
eml <- read_eml(f)

# Put together observation table ----------------------------------------------

# Rename column headings to match

dt2 <- dt1

colnames(dt2) <- c("sampling_location_id", 
                   "datetime", 
                   "event_id",
                   "variable_name", 
                   "taxon_id", 
                   "value")

taxon_id_vec <- dt2$taxon_id

dt2 <- mutate(dt2, taxon_id = paste(dt2$sampling_location_id, 
                                    dt2$datetime, 
                                    dt2$taxon_id, 
                                    seq(1,dim(dt2)[1]), 
                                    sep = "_" ))

dt2 <- mutate(dt2, event_id = paste(dt2$sampling_location_id, 
                                    dt2$event_id,
                                    datetime, 
                                    sep = "_"))

dt2 <- mutate(dt2, observation_id = paste("ntl7_17.", 
                                          seq(1,dim(dt2)[1]), 
                                          sep = ""))

dt2 <- mutate(dt2, study_id = rep("ntl7_17", 
                                  dim(dt2)[1]))

dt2 <- mutate(dt2, unit = "number")

df_observation <- select(dt2, 
                         observation_id, 
                         event_id, 
                         study_id, 
                         sampling_location_id, 
                         datetime, taxon_id, 
                         variable_name, 
                         value, 
                         unit)

write.csv(df_observation, 
          file = paste(working_dir,
                       "/observations.csv",
                       sep = ""),
          row.names = FALSE)

# Put together sampling location table ----------------------------------------

dt3 <- select(dt2, 
              sampling_location_id)

dt3 <- distinct(dt3)

dt3 <- mutate(dt3, 
              sampling_location_name = c("Allequash Lake","Big Musky Lake","Crystal Bog Lake","Crystal Lake","Fish Lake","Lake Mendota","Lake Monona","Sparkling Lake","Trout Bog Lake","Trout Lake","Lake Wingra"),
              latitude = c(46.0367,46.0162,46.008,46.0018,43.2872,43.09885,43.06337,46.0091,46.0412,46.0461,43.053),
              longitude = c(-89.6291,-89.6135,-89.606,-89.6136,-89.6531,-89.405545,-89.36086,-89.6995,-89.6861,-89.6751,-89.425),
              elevation = c(495,494,500,503,502,261,259,258,495,495,495),
              parent_sampling_location_id = rep("NA",dim(dt3)[1]))

df_sampling_location <- select(dt3, 
                               sampling_location_id, 
                               sampling_location_name, 
                               latitude, 
                               longitude, 
                               elevation, 
                               parent_sampling_location_id)

write.csv(df_sampling_location, 
          file = paste(working_dir,
                       "/sampling_location.csv",
                       sep = ""),
          row.names = FALSE)

# Put together sampling location description ----------------------------------

dt4 <- data.frame(datetime = sort(rep(unique(dt1$year4), 
                                      length(dt3$sampling_location_id))))

dt4 <- mutate(dt4, 
              sampling_location_id = rep(dt3$sampling_location_id, 
                                         length(unique(dt1$year4))))

dt4 <- mutate(dt4,
              variable_name = rep("NA", dim(dt4)[1]),
              value = rep("NA", dim(dt4)[1]),
              unit = rep("NA", dim(dt4)[1]))

df_sampling_location_description <- select(dt4,
                                           sampling_location_id,
                                           datetime,
                                           variable_name, 
                                           value, 
                                           unit)

write.csv(df_sampling_location_description, 
          file = paste(working_dir,
                       "/sampling_location_description.csv",
                       sep = ""),
          row.names = FALSE)

# Put together study table ----------------------------------------------------

dt5 <- data.frame(study_id = unique(dt2$study_id),
                  taxon_group = "common name",
                  ecological_group = "NA",
                  intent = "NA",
                  study_design = "observational",
                  methods = "Find detailed methods here: doi:10.6073/pasta/8c1ba9aab5724e9dc2ce7e3950302679",
                  original_dataset_id = "doi:10.6073/pasta/8c1ba9aab5724e9dc2ce7e3950302679")

df_study <- dt5

write.csv(df_study, 
          file = paste(working_dir,
                       "/study.csv",
                       sep = ""),
          row.names = FALSE)

# Put together event_data table -----------------------------------------------

df_event_data <- data.frame(event_id = dt2$event_id,
                            variable_name = rep("NA", length(dt2$event_id)),
                            value = rep("NA", length(dt2$event_id)))

write.csv(df_event_data, 
          file = paste(working_dir,
                       "/event_data.csv",
                       sep = ""),
          row.names = FALSE)

# Put together taxon table ----------------------------------------------------

df_taxon <- data.frame(taxon_id = dt2$taxon_id,
                       taxon_level = rep("common name", length(unique(dt2$taxon_id))),
                       taxon_name = taxon_id_vec,
                       authority_system = "NA",
                       authority_taxon_id = "NA")

write.csv(df_taxon, 
          file = "/Users/csmith/Desktop/popcom/knb-lter-ntl.7.17/taxon.csv",
          row.names = FALSE)

# Put together taxon description ----------------------------------------------

df_taxon_descr <- data.frame(taxon_id = dt2$taxon_id,
                             datetime = dt2$datetime,
                             variable_name = "NA",
                             value = "NA",
                             author = "NA")

write.csv(df_taxon_descr, 
          file = paste(working_dir,
                       "/taxon_descr.csv",
                       sep = ""),
          row.names = FALSE)

# Write EML -------------------------------------------------------------------

write_eml(eml,
          file = paste(working_dir,
                       "/",
                       package_id,
                       ".xml",
                       sep = ""))

