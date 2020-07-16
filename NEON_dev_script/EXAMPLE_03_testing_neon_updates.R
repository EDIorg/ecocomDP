
# get token from system environmental variable
# see https://www.neonscience.org/neon-api-tokens-tutorial for information on how to get and set up token
my_neon_token <- Sys.getenv("NEON_TOKEN")

# search for invertebrate data products
my_search_result <- ecocomDP::search_data(text = "invertebrate")
View(my_search_result)

# pull data for the NEON aquatic "Macroinvertebrate collection" data product
# function not yet compatible with "token" argument in updated neonUtilities
t1 <- Sys.time()
my_search_result_data <- ecocomDP::read_data(id = "DP1.20120.001", 
                                             site = c("ARIK", "POSE"),
                                             token = my_neon_token)
t2 <- Sys.time()
t2-t1


t1 <- Sys.time()
my_search_result_data <- ecocomDP::read_data(id = "DP1.20120.001", 
                                             site = c("ARIK", "POSE"),
                                             token = NA)
t2 <- Sys.time()
t2-t1


all_tabs <- map_neon_data_to_ecocomDP.BEETLE(
  site = c("CPER", "HARV"),
  token = my_neon_token)
