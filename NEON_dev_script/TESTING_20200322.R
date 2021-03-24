###############################################
###############################################
# PLANT

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10058.001.001",
  neon.data.read.path = "my_result/DP1.10058.001_20210320222014.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

###############################################
###############################################
# BIRDS

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10003.001.001",
  neon.data.read.path = "my_result/DP1.10003.001_20210320221508.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)


###############################################
###############################################
# BEETLES

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.001",
  neon.data.read.path = "my_result/DP1.10022.001_20210320221443.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

###############################################
###############################################
# HERPS

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10022.001.002",
  neon.data.read.path = "my_result/DP1.10022.001_20210320221443.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

###############################################
###############################################
# MOSQUITO

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10043.001.001",
  neon.data.read.path = "my_result/DP1.10043.001_20210320221902.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)


###############################################
###############################################
# SMALL_MAMMAL

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10072.001.001",
  neon.data.read.path = "my_result/DP1.10072.001_20210320222054.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)


###############################################
###############################################
# TICK_PATHOGEN

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10092.001.001",
  neon.data.read.path = "my_result/DP1.10092.001_20210320222153.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)


###############################################
###############################################
# TICKS

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.10093.001.001",
  neon.data.read.path = "my_result/DP1.10093.001_20210320222321.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

###############################################
###############################################
# FISH

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20107.001.001",
  neon.data.read.path = "my_result/DP1.20107.001_20210320222348.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

###############################################
###############################################
# MACROINVERTEBRATE

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  neon.data.read.path = "my_result/DP1.20120.001_20210320222420.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

###############################################
###############################################
# ALGAE

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20166.001.001",
  neon.data.read.path = "my_result/DP1.20166.001_20210320222638.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)

###############################################
###############################################
# ZOOPS

rm(list = ls())

my_result_read_data <- read_data(
  id = "neon.ecocomdp.20219.001.001",
  neon.data.read.path = "my_result/DP1.20219.001_20210320222716.RDS")

my_result_read_data[[1]]$validation_issues
my_result_read_data[[1]]$metadata$data_package_info

tab_flat <- my_result_read_data[[1]]$tables %>% 
  ecocomDP::flatten_ecocomDP() %>% 
  as.data.frame()

View(tab_flat)
