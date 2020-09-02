
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