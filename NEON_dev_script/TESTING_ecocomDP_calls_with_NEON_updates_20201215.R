# DATA
# testing with NEON algae "DP1.20166.001" -- returns data
# my_search_result$id[1]
# my_search_result$title[1]
# my_search_result$source_id[1]



###############################################
###############################################

#BEETLE update

my_result <- map_neon.ecocomdp.10022.001.001(
  site = c('ABBY','BARR'),
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_mapped <- map_neon_data_to_ecocomDP(
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



my_search_result <- ecocomDP::search_data("NEON")
View(my_search_result)

###############################################
###############################################

#ALGAE 

my_result <- map_neon.ecocomdp.20166.001.001(
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.20166.001.001",
  site = c('COMO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

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
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site = c('COMO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()

###############################################
###############################################

# BEETLE

rm(list=ls())

my_result <- map_neon.ecocomdp.10022.001.001(
  site= c("ABBY","BARR"), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.001",
  site = c("ABBY","BARR"), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

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

# currently fails on "remarks" column
my_result <- map_neon.ecocomdp.10043.001.001(
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2018-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result$dataset_summary


my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site = c('COMO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

obs_anci <- my_result_read_data[[1]]$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()
