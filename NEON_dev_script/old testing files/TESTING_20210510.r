###############################################
###############################################

# BIRD -- event_id maps to NEON eventID
# NOTE: taxa need to be added within event_id to get abundances per event

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10003.001.001",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)


tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum())

# my_result_read_data$tables %>% base::list2env(.GlobalEnv)
# 
# observation$observation_id %>% setdiff(observation_ancillary$observation_id)
# observation_ancillary$observation_id %>% setdiff(observation$observation_id)

###############################################
###############################################

#BEETLE -- event_id maps to NEON sampleID, boutID is in ancillary table
# no expected dup taxa within event_id
my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.001",
  site = c('ABBY','BARR'),
  startdate = "2019-06",
  # enddate = "2019-09",
  enddate = "2021-02",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

# tab_flat %>% filter(event_id == "ABBY_003.E.20200811")
# seems to be true dups in the sorting table?

###############################################
###############################################

# HERPS -- event_id maps to NEON sampleID, boutID is in ancillary table
# no dup taxa within event_id

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.002",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2019-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

###############################################
###############################################

# MOSQUITO -- event_id maps to NEON sampleID, neon_event_id maps to eventID
# no dup taxa expected within event_id, but some do exist. 

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10043.001.001",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)


tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

###############################################
###############################################

# PLANTS
# event_id = paste0(location_id,"_",subplot_id,"_",year,"-",boutNumber)
# needs post-processing to separate/aggregate data properly for each spatial scale

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10058.001.001",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

###############################################
###############################################

# SMALL_MAMMAL - event_id maps to plot/grid by year/month and should not have dup taxa
# event_id = paste(location_id, year, month, sep = "_"))

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10072.001.001",
  site= c("NIWO","DSNY"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

###############################################
###############################################

# TICK_PATHOGEN
# event_id = namedLocation/plotID by collectDate
# event_id should not have dup taxa
# note -- was calculating postivitiy rate incorrectly, should be fixed now. 

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10092.001.001",
  site = c("ORNL","OSBS"),
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

###############################################
###############################################

# TICK -- event_id maps to neon sampleID
# also record neon_event_id -- which has multiple samplesIDs
# should be no dup in taxa by lifestage combos within a sample/event_id
# coungs should be summed among lifestages within a sample

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10093.001.001",
  site= c("NIWO","DSNY", "BART"), 
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

###############################################
###############################################

# FISH
# event_id = NEON eventID
# should be no dup taxa within event_id

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20107.001.001",
  site = c(c('COMO','LECO')),
  startdate = "2016-01",
  enddate = "2018-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

###############################################
###############################################

# MACROINVERTEBRATE
# event_id = sampleID
# neon_event_id = NEON's eventID
# should not be dup taxa within event_id/sampleID -- different size classes and life states added together in a sampleID -- this is different than what was sent to neonDivData previously

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2021-03",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

###############################################
###############################################

#ALGAE -- event_id = neon_sample_id
# neon_event_id == NEON eventID, which has multiple samples
# no dup taxa should occur within an event_id/neon_sample_id -- but have been seeing some
# shoudl we add dup taxa densities together?

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20166.001.001",
  site = c('COMO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)

# SUGG.20171113.EPIPHYTON.8

###############################################
###############################################

# ZOOPLANKTON (MACROINVERTEBRATES)
# event_id = neon_sample_id
# neon_event_id = NEON's eventID, multiple samples map to a NEON event ID

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20219.001.001",
  site = c("BARC","SUGG"),
  startdate = "2016-01",
  enddate = "2017-11",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_data() %>% 
  as.data.frame()

View(tab_flat)
plot_taxa_sample_time(my_result_read_data$tables$observation, my_result_read_data$id)

tab_flat %>% group_by(event_id) %>%
  summarize(no_dup_taxa = taxon_id %>% duplicated() %>% sum()) %>%
  dplyr::filter(no_dup_taxa > 0)







