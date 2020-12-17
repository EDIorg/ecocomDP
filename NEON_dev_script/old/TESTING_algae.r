
my_search_result_data <- ecocomDP::read_data(id = "DP1.20166.001", 
                                             token = Sys.getenv("NEON_TOKEN"),
                                             site = "ARIK")



my_result <- map_neon_data_to_ecocomDP.ALGAE(
  token = Sys.getenv("NEON_TOKEN"),
  site = "ARIK")


my_search_result_data[[1]]$tables$location %>% head()

my_search_result_data[[1]]$tables$taxon %>% head()
my_search_result_data[[1]]$tables$table_location_ancillary %>% head()

