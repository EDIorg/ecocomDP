
rm(list=ls())

devtools::load_all()


# # algae
# my_dataset <- read_data(
#   id = "neon.ecocomdp.20166.001.001",
#   site = c('COMO','SUGG'), 
#   startdate = "2017-06",
#   enddate = "2019-09",
#   token = Sys.getenv("NEON_TOKEN"),
#   check.size = FALSE)

# macroinverts
my_dataset <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)


# detecting data types
ecocomDP:::detect_data_type(ants_L1)
ecocomDP:::detect_data_type(ants_L1)
ecocomDP:::detect_data_type(ants_L1$tables)
ecocomDP:::detect_data_type(ants_L1$tables$observation)
ecocomDP:::detect_data_type(list(a = ants_L1, b = ants_L1))
ecocomDP:::detect_data_type(list(a = ants_L1, b = ants_L1))

ecocomDP:::detect_data_type(my_dataset)
ecocomDP:::detect_data_type(my_dataset)
ecocomDP:::detect_data_type(my_dataset$tables)
ecocomDP:::detect_data_type(my_dataset$tables$observation)
ecocomDP:::detect_data_type(list(a = my_dataset, b = my_dataset))
ecocomDP:::detect_data_type(list(a = my_dataset, b = my_dataset))

# error out with informative message
ecocomDP:::detect_data_type(ants_L1$metadata)
ecocomDP:::detect_data_type(my_dataset$metadata)
ecocomDP:::detect_data_type(list(a="a"))

# this detects "list_of_datasets" -- might want to improve logic in the future?
ecocomDP:::detect_data_type(list(a = ants_L1, b = ants_L1))

# test new flatten with autodetect
flat <- flatten_data(ants_L1)
flat <- flatten_data(ants_L1)
flat <- flatten_data(ants_L1$tables)
flat <- flatten_data(my_dataset)
flat <- flatten_data(my_dataset)
flat <- flatten_data(my_dataset$tables)

# should error with message
flat <- flatten_data(ants_L1$validation_issues)
flat <- flatten_data(my_dataset$validation_issues)


#########################################################
###########################################################
# accum by site

plot_taxa_accum_sites(my_dataset)

plot_taxa_accum_sites(
  data = my_dataset %>% flatten_data())



###########################################################
# plot ecocomDP dataset
plot_taxa_accum_sites(ants_L1)

# plot flattened ecocomDP data
plot_taxa_accum_sites(flatten_data(ants_L1))

# plot an ecocomDP observation table
plot_taxa_accum_sites(
  data = ants_L1$tables$observation)

# tidy syntax
ants_L1 %>% plot_taxa_accum_sites()

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_data() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_accum_sites()
###########################################################
###########################################################











###########################################################
###########################################################
# accum by time

# looks weird for SUGG for macroinvert dataset using both methods
plot_taxa_accum_time(
  data = my_dataset)

plot_taxa_accum_time(
  data = my_dataset$tables$observation,
  id = my_dataset$id)

plot_taxa_accum_time(
  data = my_dataset %>% flatten_data())


###########################################################

# plot ecocomDP formatted dataset
plot_taxa_accum_time(
  data = ants_L1)

# plot flattened ecocomDP dataset
plot_taxa_accum_time(
  data = flatten_data(ants_L1))

# plot ecocomDP observation table
plot_taxa_accum_time(
  data = ants_L1$tables$observation)

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_data() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_accum_time()

###########################################################
###########################################################










###########################################################
###########################################################
# richness by time

# this is also messed up for macroinverts -- COMO is weird
# RENAME from "plot_taxa_diversity" to "plot_richness_time"
plot_taxa_diversity(
  data = my_dataset$tables$observation,
  id = my_dataset$id)

plot_taxa_diversity(
  data = my_dataset)

plot_taxa_diversity(
  data = my_dataset,
  time_window_size = "year")

plot_taxa_diversity(
  data = my_dataset,
  time_window_size = "month")

plot_taxa_diversity(
  data = my_dataset,
  time_window_size = "day")

plot_taxa_diversity(
  data = my_dataset %>% flatten_data(),
  time_window_size = "year")

plot_taxa_diversity(
  data = my_dataset %>% flatten_data(),
  time_window_size = "month")

my_dataset %>% plot_taxa_diversity()

my_dataset %>% flatten_data() %>% 
  dplyr::filter(grepl("^SUGG",location_id)) %>% 
  plot_taxa_diversity()

my_dataset %>% flatten_data() %>% 
  dplyr::filter(grepl("^SUGG",location_id)) %>% 
  plot_taxa_diversity(time_window_size = "day")

my_dataset %>% flatten_data() %>% 
  dplyr::filter(grepl("^SUGG",location_id)) %>% 
  plot_taxa_diversity(time_window_size = "month")

my_dataset %>% flatten_data() %>% 
  dplyr::filter(grepl("^SUGG",location_id)) %>% 
  plot_taxa_diversity(time_window_size = "year")

###########################################################

# plot richness through time for ecocomDP formatted dataset by 
# observation date
plot_taxa_diversity(ants_L1)

# plot richness through time for ecocomDP formatted dataset by 
# aggregating observations within a year
plot_taxa_diversity(
  data = ants_L1,
  time_window_size = "year")

# plot richness through time for ecocomDP observation table
plot_taxa_diversity(ants_L1$tables$observation)

# plot richness through time for flattened ecocomDP dataset 
plot_taxa_diversity(flatten_data(ants_L1))


# Using Tidy syntax:
# plot ecocomDP formatted dataset richness through time by 
# observation date
ants_L1 %>% plot_taxa_diversity()

ants_L1 %>% plot_taxa_diversity(time_window_size = "day")

# plot ecocomDP formatted dataset richness through time 
# aggregating observations within a month
ants_L1 %>% plot_taxa_diversity(time_window_size = "month")

# plot ecocomDP formatted dataset richness through time 
# aggregating observations within a year
ants_L1 %>% plot_taxa_diversity(time_window_size = "year")

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_data() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2007-01-01") %>%
  plot_taxa_diversity(
    time_window_size = "year")


###########################################################
###########################################################











###########################################################
###########################################################
# sample coverage by site and time
# RENAME 
plot_sample_space_time(
  data = my_dataset$tables$observation,
  id = my_dataset$id)

plot_sample_space_time(
  data = my_dataset)

plot_sample_space_time(
  data = my_dataset %>% flatten_data())

plot_sample_space_time(
  data = my_dataset$tables$observation,
  id = names(my_dataset))

plot_sample_space_time(
  data = ants_L1)

my_dataset %>%
  plot_sample_space_time()

# filter location id
my_dataset %>% 
  flatten_data() %>%
  dplyr::filter(grepl("SUGG",location_id)) %>%
  plot_sample_space_time()

###########################################################

# plot ecocomDP formatted dataset
plot_sample_space_time(ants_L1)

# plot flattened ecocomDP dataset
plot_sample_space_time(flatten_data(ants_L1))

# plot ecocomDP observation table
plot_sample_space_time(ants_L1$tables$observation)

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_data() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_sample_space_time()

# tidy syntax, filter data by site ID
ants_L1 %>% 
  flatten_data() %>% 
  dplyr::filter(
    as.numeric(location_id) > 4) %>%
  plot_sample_space_time()

###########################################################
###########################################################












###########################################################
###########################################################
# plot shared taxa across sites -- this seems to work fine
my_dataset <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)


plot_taxa_shared_sites(
  data = my_dataset$tables$observation,
  id = my_dataset$id)

plot_taxa_shared_sites(
  data = my_dataset)

plot_taxa_shared_sites(
  data = ants_L1$tables$observation,
  id = ants_L1$id)

plot_taxa_shared_sites(
  data = ants_L1)


neon_data <- ecocomDP::read_data(
  id = "neon.ecocomdp.20120.001.001",
  site = c('ARIK','CARI','MAYF'))

plot_taxa_shared_sites(neon_data)

###########################################################

# plot ecocomDP formatted dataset
plot_taxa_shared_sites(ants_L1)

# plot flattened ecocomDP dataset
plot_taxa_shared_sites(flatten_data(ants_L1))

# plot ecocomDP observation table
plot_taxa_shared_sites(ants_L1$tables$observation)

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_data() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_shared_sites()

# tidy syntax, filter data by site ID
ants_L1 %>% 
  flatten_data() %>% 
  dplyr::filter(
    as.numeric(location_id) > 4) %>%
  plot_taxa_shared_sites()

###########################################################



###########################################################
###########################################################
# plot rank frequencies
# this is in taxon table... should we make an option to plot frequencies in the actual data?

plot_taxa_rank(
  data = my_dataset)


plot_taxa_rank(
  data = my_dataset,
  facet_var = "location_id") #e.g., "location_id", "datetime" must be a column name in observation or taxon table


plot_taxa_rank(
  data = my_dataset,
  facet_var = "datetime") #e.g., "location_id", "datetime" must be a column name in observation or taxon table

plot_taxa_rank(
  data = ants_L1,
  facet_var = "datetime") 

###########################################################

# plot ecocomDP formatted dataset
plot_taxa_rank(ants_L1)

# download and plot NEON macroinvertebrate data
my_dataset <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO'), 
  startdate = "2017-06",
  enddate = "2019-09",
  check.size = FALSE)

plot_taxa_rank(my_dataset)

# facet by location
plot_taxa_rank(
  data = my_dataset,
  facet_var = "location_id")

# plot flattened ecocomDP dataset
plot_taxa_rank(
  data = flatten_data(my_dataset),
  facet_var = "location_id")


# tidy syntax, filter data by date
my_dataset %>% 
  flatten_data() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_rank()

# tidy syntax, filter data by site ID
my_dataset %>% 
  flatten_data() %>% 
  dplyr::filter(
    grepl("COMO",location_id)) %>%
  plot_taxa_rank()
###########################################################








###########################################################
###########################################################
# Plot stacked taxa by site
plot_taxa_occur_freq(
  data = my_dataset,
  facet_var = "location_id",
  color = "location_id",
  min_occurrence = 5)

plot_taxa_occur_freq(
  data = my_dataset,
  facet_var = "location_id",
  min_occurrence = 30)

# different ways to make the same plot
plot_taxa_occur_freq(
  data = my_dataset)

plot_taxa_occur_freq(
  data = my_dataset %>% flatten_data())

plot_taxa_occur_freq(
  data = my_dataset$tables %>% flatten_tables())

###########################################################

# plot ecocomDP formatted dataset
plot_taxa_occur_freq(ants_L1)

# plot flattened ecocomDP dataset
plot_taxa_occur_freq(flatten_data(ants_L1))

# facet by location color by taxon_rank
plot_taxa_occur_freq(
  data = ants_L1,
  facet_var = "location_id",
  color_var = "taxon_rank")

# color by location, only include taxa with > 10 occurrences
plot_taxa_occur_freq(
  data = ants_L1,
  facet_var = "location_id",
  color_var = "location_id",
  min_occurrence = 5)

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_data() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_occur_freq()
###########################################################
###########################################################







###########################################################
###########################################################
# boxplots

# plot ecocomDP formatted dataset
plot_taxa_abund(my_dataset)

# plot flattened ecocomDP dataset, log(x+1) transform abundances
plot_taxa_abund(
  data = flatten_data(my_dataset),
  trans = "log1p")

# facet by location color by taxon_rank, log 10 transformed
plot_taxa_abund(
  data = my_dataset,
  facet_var = "location_id",
  color_var = "taxon_rank",
  trans = "log10")

# facet by location, only plot taxa of rank = "species"
plot_taxa_abund(
  data = my_dataset,
  facet_var = "location_id",
  min_relative_abundance = 0.01,
  trans = "log1p")

# color by location, only include taxa with > 10 occurrences
plot_taxa_abund(
  data = my_dataset,
  color_var = "location_id",
  trans = "log10")

# tidy syntax, filter data by date
my_dataset %>% 
  flatten_data() %>% 
  dplyr::filter(
    !grepl("^SUGG", location_id)) %>%
  plot_taxa_abund(
    trans = "log1p",
    min_relative_abundance = 0.005,
    facet_var = "location_id")




###########################################################

# Read a dataset of interest
dataset <- ants_L1

# plot ecocomDP formatted dataset
plot_taxa_abund(dataset)

# plot flattened ecocomDP dataset, log(x+1) transform abundances
plot_taxa_abund(
  data = flatten_data(dataset),
  trans = "log1p")

# facet by location color by taxon_rank, log 10 transformed
plot_taxa_abund(
  data = dataset,
  facet_var = "location_id",
  color_var = "taxon_rank",
  trans = "log10")

# facet by location, minimum rel. abund = 0.05
plot_taxa_abund(
  data = dataset,
  facet_var = "location_id",
  min_relative_abundance = 0.05,
  trans = "log1p")

# color by location, log 10 transform
plot_taxa_abund(
  data = dataset,
  color_var = "location_id",
  trans = "log10")

# tidy syntax, filter data by date
dataset %>% 
  flatten_data() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_abund(
    trans = "log1p",
    min_relative_abundance = 0.01)



















###########################################################
###########################################################

# plot map of sites
plot_sites(ants_L1)

plot_sites(flatten_data(ants_L1))


# download and plot NEON macroinvertebrate data
my_dataset <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO'), 
  startdate = "2017-06",
  enddate = "2019-09",
  check.size = FALSE)

plot_sites(my_dataset)




