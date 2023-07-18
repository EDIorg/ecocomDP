# # test install from my dev repo
# detach(packages:ecocomDP)
# remotes::install_github("sokole/ecocomDP@development")
# library(ecocomDP)

library(tidyverse)

devtools::load_all()

###############################################
###############################################

# Get data ---- 

# # NEON MACROINVERTEBRATE
# # event_id = sampleID
# # neon_event_id = NEON's eventID
# # should not be dup taxa within event_id/sampleID -- different size classes and life states added together in a sampleID -- this is different than what was sent to neonDivData previously
# 
# neon_data_list <- read_data(
#   id = "neon.ecocomdp.20120.001.001",
#   site= c('COMO','LECO','SUGG'), 
#   startdate = "2017-06",
#   enddate = "2021-03",
#   token = Sys.getenv("NEON_TOKEN"),
#   check.size = FALSE)
# 
# # EDI aquatic inverts
# edi_data_list <- read_data("edi.290.2")
# 
# # save data
# saveRDS(neon_data_list, "TEST_DATA/neon_data_list.RDS")
# saveRDS(edi_data_list, "TEST_DATA/edi_data_list.RDS")

###############################################
###############################################

# Read data ----

neon_data_list <- readRDS("TEST_DATA/neon_data_list.RDS")
edi_data_list <- readRDS("TEST_DATA/edi_data_list.RDS")

###############################################
###############################################

# Flatten and stack data ----

flat_neon <- neon_data_list %>% flatten_data()

flat_edi <- edi_data_list %>% flatten_data()

stacked_data <- bind_rows(
  flat_neon,
  flat_edi)

###############################################
###############################################

# plot stacked data ----

# testing plot_sample_space_time
stacked_data %>% plot_sample_space_time()

stacked_data %>% plot_sample_space_time() +
  ggplot2::theme(legend.position = "none")

neon_data_list %>% plot_sample_space_time()
edi_data_list %>% plot_sample_space_time()


flat_neon %>% plot_sample_space_time()
flat_edi %>% plot_sample_space_time()

flat_neon %>% plot_sample_space_time(color_var = "location_id")
flat_edi %>% plot_sample_space_time(shape_var = "location_id")
# need to allow plotting parameters

# help files example
# Read a dataset of interest
dataset <- read_data("edi.193.5")

# Plot the dataset
plot_sample_space_time(dataset)

# Flatten the dataset, manipulate, then plot
dataset %>%
  flatten_data() %>%
  dplyr::filter(lubridate::as_date(datetime) > "2003-07-01") %>%
  dplyr::filter(as.numeric(location_id) > 4) %>%
  plot_sample_space_time()

ants_L1 %>% plot_sample_space_time(color_var = "location_id")

#########################################
# plotting fxns that seem good

stacked_data %>% plot_taxa_rank(facet_var = "package_id")

my_plot <- stacked_data %>% plot_sites() 
print(my_plot)

my_plot2 <- my_plot +
  theme_minimal()
print(my_plot2)

stacked_data %>% plot_taxa_abund(
  facet_var = "package_id",
  min_relative_abundance = 0.005)

stacked_data %>% plot_taxa_occur_freq(
  facet_var = "package_id",
  min_occurrence = 2)
