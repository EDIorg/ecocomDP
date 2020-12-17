

my_search_result <- ecocomDP::search_data("NEON")
my_search_result[1,]




# DATA
# testing with NEON algae "DP1.20166.001" -- returns data
my_result_inv <- map_neon_data_to_ecocomDP.NEON.20120.001.L2.01(
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_inv$dataset_summary



# DATA
# testing with NEON algae "DP1.20166.001" -- returns data
my_result_alg <- map_neon_data_to_ecocomDP.NEON.20166.001.L2.01(
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_alg$dataset_summary


my_result_alg_read_data <- ecocomDP::read_data(
  id = "NEON.20166.001.L2.01",
  site = c('COMO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

tab_flat <- my_result_alg_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

prob_rec <- observation_ancillary_long %>% dplyr::filter(event_id == "SUGG.20170710.PHYTOPLANKTON.2")

obs_anci <- my_result_alg_read_data$DP1.20166.001$tables$observation_ancillary
obs_anci$observation_ancillary_id %>% duplicated() %>% sum()