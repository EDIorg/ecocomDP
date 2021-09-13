
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







#########################################################
###########################################################
# accum by site

plot_taxa_accum_sites(
  dataset = my_dataset)

plot_taxa_accum_sites(
  flat_data = my_dataset %>% flatten_dataset())


###########################################################
# plot ecocomDP dataset
plot_taxa_accum_sites(
  dataset = ants_L1)

# plot flattened ecocomDP data
plot_taxa_accum_sites(
  flat_data =  flatten_dataset(ants_L1))

# plot an ecocomDP observation table
plot_taxa_accum_sites(
  observation = ants_L1[[1]]$tables$observation)

# tidy syntax
ants_L1 %>% plot_taxa_accum_sites()

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_accum_sites(flat_data = .)
###########################################################
###########################################################











###########################################################
###########################################################
# accum by time

# looks weird for SUGG for macroinvert dataset using both methods
plot_taxa_accum_time(
  dataset = my_dataset)

plot_taxa_accum_time(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

plot_taxa_accum_time(
  flat_data = my_dataset %>% flatten_dataset())


###########################################################

# plot ecocomDP formatted dataset
plot_taxa_accum_time(
  dataset = ants_L1)

# plot flattened ecocomDP dataset
plot_taxa_accum_time(
  flat_data = flatten_dataset(ants_L1))

# plot ecocomDP observation table
plot_taxa_accum_time(
  observation = ants_L1[[1]]$tables$observation)

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_accum_time(flat_data = .)

###########################################################
###########################################################










###########################################################
###########################################################
# richness by time

# this is also messed up for macroinverts -- COMO is weird
# RENAME from "plot_taxa_diversity" to "plot_richness_time"
plot_taxa_richness(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

plot_taxa_richness(
  dataset = my_dataset)

plot_taxa_richness(
  dataset = my_dataset,
  time_window_size = "year")

plot_taxa_richness(
  dataset = my_dataset,
  time_window_size = "month")

plot_taxa_richness(
  dataset = my_dataset,
  time_window_size = "day")

plot_taxa_richness(
  observation = my_dataset %>% flatten_dataset(),
  time_window_size = "year")

plot_taxa_richness(
  observation = my_dataset %>% flatten_dataset(),
  time_window_size = "month")

my_dataset %>% plot_taxa_richness()

my_dataset %>% flatten_dataset() %>% 
  dplyr::filter(grepl("^SUGG",location_id)) %>% 
  plot_taxa_richness(observation = .)

my_dataset %>% flatten_dataset() %>% 
  dplyr::filter(grepl("^SUGG",location_id)) %>% 
  plot_taxa_richness(observation = ., time_window_size = "day")

my_dataset %>% flatten_dataset() %>% 
  dplyr::filter(grepl("^SUGG",location_id)) %>% 
  plot_taxa_richness(observation = ., time_window_size = "month")

my_dataset %>% flatten_dataset() %>% 
  dplyr::filter(grepl("^SUGG",location_id)) %>% 
  plot_taxa_richness(observation = ., time_window_size = "year")

###########################################################

# plot richness through time for ecocomDP formatted dataset by 
# observation date
plot_taxa_richness(
  dataset = ants_L1)

# plot richness through time for ecocomDP formatted dataset by 
# aggregating observations within a year
plot_taxa_richness(
  dataset = ants_L1,
  time_window_size = "year")

# plot richness through time for ecocomDP observation table
plot_taxa_richness(
  observation = ants_L1[[1]]$tables$observation)

# plot richness through time for flattened ecocomDP dataset 
plot_taxa_richness(
  flat_data = flatten_dataset(ants_L1))


# Using Tidy syntax:
# plot ecocomDP formatted dataset richness through time by 
# observation date
ants_L1 %>% plot_taxa_richness()

ants_L1 %>% plot_taxa_richness(time_window_size = "day")

# plot ecocomDP formatted dataset richness through time 
# aggregating observations within a month
ants_L1 %>% plot_taxa_richness(time_window_size = "month")

# plot ecocomDP formatted dataset richness through time 
# aggregating observations within a year
ants_L1 %>% plot_taxa_richness(time_window_size = "year")

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2007-01-01") %>%
  plot_taxa_richness(
    flat_data = .,
    time_window_size = "year")


###########################################################
###########################################################











###########################################################
###########################################################
# sample coverage by site and time
# RENAME 
plot_sample_space_time(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

plot_sample_space_time(
  dataset = my_dataset)

plot_sample_space_time(
  flat_data = my_dataset %>% flatten_dataset())

plot_sample_space_time(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

plot_sample_space_time(
  dataset = ants_L1)

my_dataset %>%
  plot_sample_space_time()

# filter location id
my_dataset %>% 
  flatten_dataset() %>%
  dplyr::filter(grepl("SUGG",location_id)) %>%
  plot_sample_space_time(flat_data = .)

###########################################################

# plot ecocomDP formatted dataset
plot_sample_space_time(
  dataset = ants_L1)

# plot flattened ecocomDP dataset
plot_sample_space_time(
  flat_data = flatten_dataset(ants_L1))

# plot ecocomDP observation table
plot_sample_space_time(
  observation = ants_L1[[1]]$tables$observation)

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_sample_space_time(flat_data = .)

# tidy syntax, filter data by site ID
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    as.numeric(location_id) > 4) %>%
  plot_sample_space_time(flat_data = .)

###########################################################
###########################################################












###########################################################
###########################################################
# plot shared taxa across sites -- this seems to work fine

plot_taxa_shared_sites(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

plot_taxa_shared_sites(
  dataset = my_dataset)

plot_taxa_shared_sites(
  observation = ants_L1[[1]]$tables$observation,
  id = names(ants_L1))

plot_taxa_shared_sites(
  dataset = ants_L1)

###########################################################

# plot ecocomDP formatted dataset
plot_taxa_shared_sites(
  dataset = ants_L1)

# plot flattened ecocomDP dataset
plot_taxa_shared_sites(
  flat_data = flatten_dataset(ants_L1))

# plot ecocomDP observation table
plot_taxa_shared_sites(
  observation = ants_L1[[1]]$tables$observation)

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_shared_sites(flat_data = .)

# tidy syntax, filter data by site ID
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    as.numeric(location_id) > 4) %>%
  plot_taxa_shared_sites(flat_data = .)

###########################################################



###########################################################
###########################################################
# plot rank frequencies
# this is in taxon table... should we make an option to plot frequencies in the actual data?
plot_taxa_rank(
  observation = my_dataset[[1]]$tables$observation,
  taxon = my_dataset[[1]]$tables$taxon,
  id = names(my_dataset))

plot_taxa_rank(
  dataset = my_dataset)


plot_taxa_rank(
  facet_var = "location_id", #e.g., "location_id", "datetime" must be a column name in observation or taxon table
  taxon = my_dataset[[1]]$tables$taxon, 
  observation = my_dataset[[1]]$tables$observation, 
  id = names(my_dataset))


plot_taxa_rank(
  dataset = my_dataset,
  facet_var = "location_id") #e.g., "location_id", "datetime" must be a column name in observation or taxon table


plot_taxa_rank(
  dataset = my_dataset,
  facet_var = "datetime") #e.g., "location_id", "datetime" must be a column name in observation or taxon table

plot_taxa_rank(
  dataset = ants_L1,
  facet_var = "datetime") 

###########################################################

# plot ecocomDP formatted dataset
plot_taxa_rank(
  dataset = ants_L1)

plot_taxa_rank(
  dataset = ants_L1,
  facet_var = "location_id")

# plot flattened ecocomDP dataset
plot_taxa_rank(
  flat_data = flatten_dataset(ants_L1),
  facet_var = "location_id")

# plot ecocomDP observation table
plot_taxa_rank(
  observation = ants_L1[[1]]$tables$observation,
  taxon = ants_L1[[1]]$tables$taxon,
  facet_var = "location_id")

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_rank(flat_data = .)

# tidy syntax, filter data by site ID
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    as.numeric(location_id) > 4) %>%
  plot_taxa_rank(flat_data = .)
###########################################################








###########################################################
###########################################################
# Plot stacked taxa by site
plot_taxa_occurrence_frequency(
  dataset = my_dataset,
  facet_var = "location_id",
  color = "location_id",
  rank = "species",
  cutoff = 15)

plot_taxa_occurrence_frequency(
  dataset = my_dataset,
  facet_var = "location_id",
  cutoff = 20)

# different way sto make the same plot
plot_taxa_occurrence_frequency(
  dataset = my_dataset,
  rank = "order")

plot_taxa_occurrence_frequency(
  flat_data = my_dataset %>% flatten_dataset(),
  rank = "order")

plot_taxa_occurrence_frequency(
  flat_data = my_dataset[[1]]$tables %>% flatten_tables(),
  rank = "order")

###########################################################

# plot ecocomDP formatted dataset
plot_taxa_occurrence_frequency(
  dataset = ants_L1)

# plot flattened ecocomDP dataset
plot_taxa_occurrence_frequency(
  flat_data = flatten_dataset(ants_L1))

# facet by location color by taxon_rank
plot_taxa_occurrence_frequency(
  dataset = ants_L1,
  facet_var = "location_id",
  color_var = "taxon_rank")

# facet by location, only plot taxa of rank = "species"
plot_taxa_occurrence_frequency(
  dataset = ants_L1,
  facet_var = "location_id",
  rank = "Species", cutoff = 2)

# color by location, only include taxa with > 10 occurrences
plot_taxa_occurrence_frequency(
  dataset = ants_L1,
  color_var = "location_id",
  cutoff = 5)

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_occurrence_frequency(flat_data = .)








###########################################################
###########################################################
# boxplots

# plot ecocomDP formatted dataset
plot_taxa_abundance(
  dataset = my_dataset)

# plot flattened ecocomDP dataset, log(x+1) transform abundances
# rank = order
plot_taxa_abundance(
  flat_data = flatten_dataset(my_dataset),
  rank = "order",
  trans = "log1p")

# facet by location color by taxon_rank, log 10 transformed
plot_taxa_abundance(
  dataset = my_dataset,
  facet_var = "location_id",
  color_var = "taxon_rank",
  trans = "log10")

# facet by location, only plot taxa of rank = "species"
plot_taxa_abundance(
  dataset = my_dataset,
  facet_var = "location_id",
  cutoff = 10,
  rank = "species", 
  trans = "log1p")

# color by location, only include taxa with > 10 occurrences
plot_taxa_abundance(
  dataset = my_dataset,
  color_var = "location_id",
  trans = "log10")

# tidy syntax, filter data by date
my_dataset %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    !grepl("^SUGG", location_id)) %>%
  plot_taxa_abundance(flat_data = .,
                      trans = "log1p",
                      cutoff = 2,
                      facet_var = "location_id")




###########################################################

# plot ecocomDP formatted dataset
plot_taxa_abundance(
  dataset = ants_L1)

# plot flattened ecocomDP dataset, log(x+1) transform abundances
plot_taxa_abundance(
  flat_data = flatten_dataset(ants_L1),
  trans = "log1p")

# facet by location color by taxon_rank, log 10 transformed
plot_taxa_abundance(
  dataset = ants_L1,
  facet_var = "location_id",
  color_var = "taxon_rank",
  trans = "log10")

# facet by location, only plot taxa of rank = "species"
plot_taxa_abundance(
  dataset = ants_L1,
  facet_var = "location_id",
  rank = "Species", cutoff = 5,
  trans = "log1p")

# color by location, only include taxa with > 10 occurrences
plot_taxa_abundance(
  dataset = ants_L1,
  color_var = "location_id",
  trans = "log10")

# tidy syntax, filter data by date
ants_L1 %>% 
  flatten_dataset() %>% 
  dplyr::filter(
    lubridate::as_date(datetime) > "2003-07-01") %>%
  plot_taxa_abundance(flat_data = .,
                      trans = "log1p")
























###########################################################
###########################################################
# plot map of sites
plot_sites(
  dataset = ants_L1)

plot_sites(
  flat_data = flatten_dataset(ants_L1))

plot_sites(
  observation = ants_L1[[1]]$tables$observation,
  location = ants_L1[[1]]$tables$location)




plot_sites(
  dataset = my_dataset)

plot_sites(
  flat_data = my_dataset %>% flatten_dataset())

plot_sites(
  dataset = my_dataset,
  location = my_dataset[[1]]$tables$location)

plot_sites(
  location = my_dataset[[1]]$tables$location,
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))



######################################################
# examples of flatten data
my_dataset[[1]]$tables %>% flatten_data()
my_dataset[[1]]$tables %>% flatten_data(tables = .)
my_dataset %>% flatten_data(dataset = .)

# fails as expected with error message
list(a = c(1:5)) %>% flatten_data()

# examples for fxn documentation
flat <- flatten_data(tables = ants_L1[[1]]$tables)
flat

flat <- flatten_data(dataset = ants_L1)
flat

flat <- flatten_dataset(ants_L1)
flat

flat <- flatten_tables(ants_L1[[1]]$tables)
flat
