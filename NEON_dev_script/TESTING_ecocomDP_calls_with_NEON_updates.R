
# devtools::install_github("sokole/ecocomDP@master")
# devtools::install_github("sokole/ecocomDP@working")

library(ecocomDP)

# testing with NEON macroinvert "DP1.20120.001"
my_result_inv_mapping <- map_neon_data_to_ecocomDP.MACROINVERTEBRATE(
  site= c('COMO','LECO'), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)


my_result_inv_neonUtilities <- neonUtilities::loadByProduct(
  dpID = "DP1.20120.001",
  site= c('COMO','LECO'), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

all_tables <- ecocomDP::read_data(
  id = "DP1.20120.001",
  site= c('COMO','LECO'), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)




# DATA
# testing with NEON algae "DP1.20166.001" -- returns data
my_result_alg <- map_neon_data_to_ecocomDP.ALGAE(
  site= c('COMO','LECO'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_alg_read_data <- ecocomDP::read_data(
  id = "DP1.20166.001",
  site = c('COMO','LECO'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)







# WARNING NO DATA
# testing with NEON algae "DP1.20166.001" -- returns no taxon data
# this case works as intended and returns an empty list that 
# read data seems to be able to handle
my_result_alg <- map_neon_data_to_ecocomDP.ALGAE(
  site = c('COMO','LECO'), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_alg_read_data <- ecocomDP::read_data(
  id = "DP1.20166.001",
  site = c('COMO','LECO'), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

































# read EDI data
d <- read_data("edi.193.3")

# read NEON data
# NOTE -- without NEON API Token, you'll reach a rate limit, pause for 99 seconds
d <- read_data("DP1.20166.001")

# read NEON data with token
# NOTE -- if you use a NEON API Token, don't reach rate limit 
d <- read_data(
  id = "DP1.20166.001",
  token = Sys.getenv("NEON_TOKEN"))


# Read a NEON dataset with filters
d <- read_data(
  id = "DP1.20166.001",
  site = c("MAYF", "PRIN"),
  startdate = "2016-01",
  enddate = "2018-11",
  token = Sys.getenv("NEON_TOKEN"))

# Read multiple datasets from different sources
d <- read_data(
  id = c("edi.193.3", "DP1.20166.001"),
  token = Sys.getenv("NEON_TOKEN"))


# testing with NEON macroinvert
my_result <- map_neon_data_to_ecocomDP.MACROINVERTEBRATE(
  site= c('COMO','LECO'), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"))


# testing with NEON macroinvert
my_result <- read_data(
  id = "DP1.20120.001",
  site= c('COMO','LECO'), 
  startdate = "2019-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  path = NULL)

