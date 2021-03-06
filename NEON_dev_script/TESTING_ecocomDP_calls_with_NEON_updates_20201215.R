# DATA
# testing with NEON algae "DP1.20166.001" -- returns data
# my_search_result$id[1]
# my_search_result$title[1]
# my_search_result$source_id[1]

library(tidyverse)



my_search_result <- ecocomDP::search_data("NEON")
View(my_search_result)



###############################################
###############################################

#BEETLE update

my_result <- ecocomDP:::map_neon.ecocomdp.10022.001.001(
  site = c('ABBY','BARR'),
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)


my_result_mapped <- ecocomDP:::map_neon_data_to_ecocomDP(
  data.product.id = "neon.ecocomdp.10022.001.001",
  site = c('ABBY','BARR'),
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.001",
  site = c('ABBY','BARR'),
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$tables$dataset_summary

names(my_result_read_data[[1]]$tables)
names(my_result_read_data[[1]]$metadata)
names(my_result_read_data[[1]]$metadata$orig_NEON_data_product_info)
my_result_read_data[[1]]$metadata$data_package_info


View(my_result_read_data[[1]]$tables$observation)

# check for repeated observations -- Colin's validation seems to be getting false positives here
obs_tab <- my_result_read_data[[1]]$tables$observation
obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)


tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

my_result_read_data[[1]]$validation_issues



###############################################
###############################################

#ALGAE 
rm(list=ls())

my_result <- ecocomDP:::map_neon.ecocomdp.20166.001.001(
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.20166.001.001",
  site = c('COMO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$tables$dataset_summary
my_result_read_data[[1]]$tables$location %>% as.data.frame()
my_result_read_data[[1]]$tables$location_ancillary %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$taxon %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$observation %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$observation_ancillary %>% as.data.frame() %>% head()

View(my_result_read_data[[1]]$tables$observation)

# check for repeated observations -- Colin's validation seems to be getting false positives here
obs_tab <- my_result_read_data[[1]]$tables$observation
obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)


tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

my_result_read_data[[1]]$validation_issues


# prob_rec <- observation_ancillary_long %>% dplyr::filter(event_id == "SUGG.20170710.PHYTOPLANKTON.2")

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()


###############################################
###############################################

# MACROINVERTEBRATE

rm(list=ls())

my_result <- map_neon.ecocomdp.20120.001.001(
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)

my_result_read_data[[1]]$tables$dataset_summary
my_result_read_data[[1]]$tables$location %>% as.data.frame()
my_result_read_data[[1]]$tables$location_ancillary %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$taxon %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$observation %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$observation_ancillary %>% as.data.frame() %>% head()

View(my_result_read_data[[1]]$tables$observation)

# check for repeated observations -- Colin's validation seems to be getting false positives here
obs_tab <- my_result_read_data[[1]]$tables$observation
obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)


tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()



###############################################
###############################################

# MOSQUITO -- currently not available?

rm(list=ls())

my_result <- map_neon.ecocomdp.10043.001.001(
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10043.001.001",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues


names(my_result_read_data[[1]]$tables)

my_result_read_data[[1]]$tables$dataset_summary
my_result_read_data[[1]]$tables$location %>% as.data.frame()
my_result_read_data[[1]]$tables$location_ancillary %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$taxon %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$observation %>% as.data.frame() %>% head()
my_result_read_data[[1]]$tables$observation_ancillary %>% as.data.frame() %>% head()

View(my_result_read_data[[1]]$tables$observation)

# check for repeated observations -- Colin's validation seems to be getting false positives here
obs_tab <- my_result_read_data[[1]]$tables$observation


obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)
# there appear to be dups, but I think they're real, have different uids and vals, but same taxa and event_id

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()


###############################################
###############################################

# BIRD

rm(list=ls())

# currently fails on "remarks" column
my_result <- map_neon.ecocomdp.10003.001.001(
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result %>% names()



my_result_read_data <- read_data(
  id = "neon.ecocomdp.10003.001.001",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)
names(my_result_read_data[[1]]$metadata)
names(my_result_read_data[[1]]$metadata$orig_NEON_data_product_info)
my_result_read_data[[1]]$metadata$data_package_info

# check for repeated observations -- Colin's validation seems to be getting false positives here
obs_tab <- my_result_read_data[[1]]$tables$observation
head(as.data.frame(obs_tab))

obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)


tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()

###############################################
###############################################

# FISH

rm(list=ls())

my_result <- map_neon.ecocomdp.20107.001.001(
  site = c(c('COMO','LECO')),
  startdate = "2016-01",
  enddate = "2018-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.20107.001.001",
  site = c(c('COMO','LECO')),
  startdate = "2016-01",
  enddate = "2018-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)

# check for repeated observations -- Colin's validation seems to be getting false positives here
obs_tab <- my_result_read_data[[1]]$tables$observation
head(as.data.frame(obs_tab))


obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)


tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()

###############################################
###############################################

# HERPS

rm(list=ls())

my_result <- map_neon.ecocomdp.10022.001.002(
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result %>% names()
my_result$tables %>% names()
my_result$metadata %>% names()


my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.002",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)

obs_tab <- my_result_read_data[[1]]$tables$observation
head(as.data.frame(obs_tab))


obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)
print(obs_summary_tab)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()

###############################################
###############################################

# PLANTS

rm(list=ls())

my_result <- map_neon.ecocomdp.10058.001.001(
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.10058.001.001",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)

obs_tab <- my_result_read_data[[1]]$tables$observation
head(as.data.frame(obs_tab))


obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)
print(obs_summary_tab)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()

###############################################
###############################################

# SMALL_MAMMAL

rm(list=ls())

my_result <- map_neon.ecocomdp.10072.001.001(
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.10072.001.001",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)

obs_tab <- my_result_read_data[[1]]$tables$observation
head(as.data.frame(obs_tab))


obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)
print(obs_summary_tab)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()


###############################################
###############################################

# TICK

rm(list=ls())

my_result <- map_neon.ecocomdp.10093.001.001(
  site = c("BART","DSNY"),
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10093.001.001",
  site= c("NIWO","DSNY", "BART"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)

obs_tab <- my_result_read_data[[1]]$tables$observation
head(as.data.frame(obs_tab))


obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)
print(obs_summary_tab)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()


###############################################
###############################################

# ZOOPLANKTON (MACROINVERTEBRATES)

rm(list=ls())

my_result <- map_neon.ecocomdp.20219.001.001(
  site = c("BARC","SUGG"),
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.20219.001.001",
  site = c("BARC","SUGG"),
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)

obs_tab <- my_result_read_data[[1]]$tables$observation
head(as.data.frame(obs_tab))


obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)
print(obs_summary_tab)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()

###############################################
###############################################

# TICK_PATHOGENS

rm(list=ls())

my_result <- map_neon.ecocomdp.10092.001.001(
  site = c("ORNL","OSBS"),
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  package = "basic",
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.10092.001.001",
  site = c("ORNL","OSBS"),
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data[[1]]$validation_issues

names(my_result_read_data[[1]]$tables)

obs_tab <- my_result_read_data[[1]]$tables$observation
head(as.data.frame(obs_tab))


obs_summary_tab <- obs_tab %>% group_by_at(vars(-c(observation_id, value, unit))) %>% 
  summarize(
    n_values = length(value),
    values = paste(value, collapse = "|"),
    observation_ids = paste(observation_id, collapse = "|")) %>%
  dplyr::filter(n_values > 1)
print(obs_summary_tab)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()
