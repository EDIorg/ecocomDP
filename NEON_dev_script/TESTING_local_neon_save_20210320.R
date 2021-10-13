# DATA
# testing with NEON algae "DP1.20166.001" -- returns data
# my_search_result$id[1]
# my_search_result$title[1]
# my_search_result$source_id[1]

# library(tidyverse)



my_search_result <- ecocomDP::search_data("NEON")
View(my_search_result)

###############################################
###############################################

#BEETLE update
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2021-03",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)



my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  neon.data.read.path = "my_result/DP1.20120.001_20210430145730.RDS")

neon_raw_data <- readRDS("my_result/DP1.20120.001_20210430145730.RDS")

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  neon.data.list = neon_raw_data)



# list.files("my_result/")

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

###############################################
###############################################

#BEETLE update
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.001",
  site = c('ABBY','BARR'),
  startdate = "2019-06",
  # enddate = "2019-09",
  enddate = "2021-02",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)



my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.001",
  neon.data.read.path = "my_result/DP1.10022.001_20210430133402.RDS")

neon_raw_data <- readRDS("my_result/DP1.10022.001_20210430133402.RDS")

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.001",
  neon.data.list = neon_raw_data)



# list.files("my_result/")

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

tab_flat <- my_result_read_data$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)


###############################################
###############################################

#BIRD update
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10003.001.001",
  site = c('ABBY','BARR'),
  startdate = "2019-06",
  # enddate = "2019-09",
  enddate = "2021-02",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10003.001.001",
  neon.data.list = NA)

###############################################
###############################################

# HERPS
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.002",
  site= c("NIWO","DSNY"),
  startdate = "2016-01",
  enddate = "2017-11",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.002",
  neon.data.list = NA)
###############################################
###############################################

# MOSQUITO
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10043.001.001",
  site= c("NIWO","DSNY"),
  startdate = "2016-01",
  enddate = "2018-11",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10043.001.001",
  neon.data.list = NA)
###############################################
###############################################

# PLANT
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10058.001.001",
  site= c("NIWO","DSNY"),
  startdate = "2016-01",
  enddate = "2017-11",
  # neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10058.001.001",
  neon.data.list = NA)
###############################################
###############################################

# SMALL_MAMMALS
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10072.001.001",
  site= c("NIWO","DSNY"),
  startdate = "2016-01",
  enddate = "2017-11",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info


my_result_read_data <- read_data(
  id = "neon.ecocomdp.10072.001.001",
  neon.data.list = NA)
###############################################
###############################################

# TICK_PATHOGEN
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10092.001.001",
  site = c("ORNL","OSBS"),
  startdate = "2016-01",
  enddate = "2017-11",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10092.001.001",
  neon.data.list = NA)
###############################################
###############################################

# TICKS
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10093.001.001",
  site = c("ORNL","OSBS"),
  startdate = "2016-01",
  enddate = "2017-11",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info
  
my_result_read_data <- read_data(
  id = "neon.ecocomdp.10093.001.001",
  neon.data.list = NA)
###############################################
###############################################

# FISH
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20107.001.001",
  site = c('COMO','LECO'),
  startdate = "2016-01",
  enddate = "2018-11",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20107.001.001",
  neon.data.list = NA)
###############################################
###############################################

# MACROINVERTEBRATE
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site = c('COMO','LECO'),
  startdate = "2019-06",
  enddate = "2019-11",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  neon.data.list = NA)
###############################################
###############################################

# ALGAE
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20166.001.001",
  site = c('COMO','LECO'),
  startdate = "2017-06",
  enddate = "2019-11",
  neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20166.001.001",
  neon.data.list = NA)
###############################################
###############################################

# ZOOPS
rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20219.001.001",
  site = c("BARC","SUGG"),
  startdate = "2016-01",
  enddate = "2017-11",
  # neon.data.save.dir = "my_result",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

my_result_read_data$validation_issues
my_result_read_data$metadata$data_package_info

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20219.001.001",
  neon.data.list = NA)
