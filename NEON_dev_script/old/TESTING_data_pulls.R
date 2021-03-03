library(ecocomDP)
library(tidyverse)


a_packages <- ecocomDP::search_data("macroinvert")

my_data_package <- ecocomDP::read_data("edi.284.1")



my_data_package <- ecocomDP::read_data("edi.290.1")
my_flat_data <- my_data_package[[1]]$tables %>% ecocomDP::flatten_ecocomDP()
my_flat_data %>% head()



# does work
my_data_package <- ecocomDP:::read_data_edi("edi.290.1")




my_data_package <- ecocomDP::read_data("DP1.20120.001",
                                       token = Sys.getenv("NEON_TOKEN"))

# read in data using ecocomDP
all_data <- ecocomDP::read_data(
  id = "DP1.20120.001",
  site= c('COMO','LECO'),
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"))
my_flat_data <- all_data[[1]]$tables %>% ecocomDP::flatten_ecocomDP()
my_flat_data %>% head()



my_list_of_tables <- all_data[[1]]$tables
flat_data <- ecocomDP::flatten_ecocomDP(my_list_of_tables)


all_data <- ecocomDP::read_data("edi.290.1")
my_list_of_tables <- all_data[[1]]$tables
flat_data <- ecocomDP::flatten_ecocomDP(my_list_of_tables)
