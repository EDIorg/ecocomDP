library(devtools)
# install my version of package from github
devtools::install_github('sokole/ecocomDP')

library(ecocomDP)

# view all data in data catalog
ecocomDP::search_data()

# view all NEON data in catalog
ecocomDP::search_data(text = 'NEON')

# view all beetle data in catalog
ecocomDP::search_data(taxa = 'carabidae')

# view NEON carabid data
ecocomDP::search_data(text = 'NEON', taxa = 'carabidae')

# view NEON carabid data
my_result <- ecocomDP::search_data(taxa = 'algae')

# This is what the call will look like, not yet implemented
my_data <- ecocomDP::aggregate_ecocomDP(package.id = 'DP1.20166.001')

# small mammals -- not yet implemented
my_data <- ecocomDP::map_neon_data_to_ecocomDP(
  neon.data.product.id = "DP1.10072.001",
  site = c("MAYF", "PRIN"),
  startdate = "2016-1",
  enddate = "2018-11")

# Algae -- currently only NEON DP that is mapped
my_data <- ecocomDP::map_neon_data_to_ecocomDP(
  neon.data.product.id = "DP1.20166.001",
  site = c("MAYF", "PRIN"),
  startdate = "2016-1",
  enddate = "2018-11",
  check.size = FALSE)
