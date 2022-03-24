# Note: uid is associated with mos_expertTaxonomistIDProcessed table 

neon.data.product.id = "DP1.10043.001"

# TEST 2022 data model
# neon.data.list <- readRDS("NEON_dev_script/neon.data.list_mos.RDS")
neon.data.list_2022 <- readRDS("NEON_dev_script/neon.data.list_mos_RELEASE-2022.RDS")
# neon.data.list <- readRDS("NEON_dev_script/neon.data.list_mos_RELEASE-2022.RDS")




# TEST 2021 data model
neon.data.list_2021 <- readRDS("NEON_dev_script/neon.data.list_mos_RELEASE-2021.RDS")
# neon.data.list <- readRDS("NEON_dev_script/neon.data.list_mos_RELEASE-2021.RDS")



neon_edp_2021 <- map_neon.ecocomdp.10043.001.001(neon.data.list = neon.data.list_2021)
neon_edp_2022 <- map_neon.ecocomdp.10043.001.001(neon.data.list = neon.data.list_2022)

nrow(neon_edp_2021$observation)
nrow(neon_edp_2022$observation)
names(neon_edp_2021) %>% setdiff(names(neon_edp_2022))
names(neon_edp_2022) %>% setdiff(names(neon_edp_2021))


library(ecocomDP)
neon_edp_old_2021 <- ecocomDP::read_data(
    id = "neon.ecocomdp.10043.001.001",
    site = "BART",
    startdate = "2017-01",enddate = "2019-01",
    # package = "basic",
    release = "RELEASE-2021",
    check.size = FALSE)

flat_old_2021 <- neon_edp_old_2021 %>% ecocomDP::flatten_data()
flat_2021 <- neon_edp_2021 %>% ecocomDP::flatten_data()
flat_2022 <- neon_edp_2022 %>% ecocomDP::flatten_data()

names(flat_2021) %>% setdiff(names(flat_2022))
names(flat_2022) %>% setdiff(names(flat_2021))

names(flat_2021) %>% setdiff(names(flat_old_2021))
names(flat_old_2021) %>% setdiff(names(flat_2021))

flat_2021$observation_id %>% setdiff(flat_2022$observation_id)

flat_old_2021$observation_id %>% setdiff(flat_2021$observation_id)


# 
# # errors out
# library(ecocomDP)
# neon_edp_old_2022 <- ecocomDP::read_data(
#   id = "neon.ecocomdp.10043.001.001",
#   site = "BART",
#   startdate = "2017-01",enddate = "2019-01",
#   # package = "basic",
#   release = "RELEASE-2022",
#   check.size = FALSE)


# # TEST 2022 data model v2
# neon.data.list <- neonUtilities::loadByProduct(
#   dpID = "DP1.10043.001",site = "BART",
#   startdate = "2017-01-01",enddate = "2019-01-01",
#   package = "basic",release = "RELEASE-2022",
#   check.size = FALSE)
# 
# saveRDS(neon.data.list, file = "NEON_dev_script/neon.data.list_mos_RELEASE-2022.RDS")
# 
# 
# 
# # TEST old data model
# neon.data.list <- neonUtilities::loadByProduct(
#   dpID = "DP1.10043.001",site = "BART",
#   startdate = "2017-01-01",enddate = "2019-01-01",
#   package = "basic",release = "RELEASE-2021",
#   check.size = FALSE)
# 
# saveRDS(neon.data.list, file = "NEON_dev_script/neon.data.list_mos_RELEASE-2021.RDS")

##################################################

# download all mos data from both 2021 and 2022 RELEASE
# save locally

# neon_data_list_full_2021 <- neonUtilities::loadByProduct(
#   dpID = "DP1.10043.001",
#   release = "RELEASE-2021",
#   check.size = FALSE)
# saveRDS(neon_data_list_full_2021,"NEON_raw_data/DP1.10043.001_RELEASE-2021.RDS")
#   
# neon_data_list_full_2022 <- neonUtilities::loadByProduct(
#   dpID = "DP1.10043.001",
#   release = "RELEASE-2022",
#   check.size = FALSE)
# saveRDS(neon_data_list_full_2022,"NEON_raw_data/DP1.10043.001_RELEASE-2022.RDS")


# read local data back in  
neon_data_list_full_2022 <- readRDS("NEON_raw_data/DP1.10043.001_RELEASE-2022.RDS")
neon_data_list_full_2021 <- readRDS("NEON_raw_data/DP1.10043.001_RELEASE-2021.RDS")


# make sure to use old version of ecocomDP (installed from CRAN as of 2022-03-24)
detach(package:ecocomDP)
library(ecocomDP)

list_2021_old <- neon_data_list_full_2021 %>%
  ecocomDP:::map_neon.ecocomdp.10043.001.001()

# # this should fail with: 
# # Error: Join columns must be present in data.
# # x Problem with `archiveID`.
# list_2022_old <- neon_data_list_full_2022 %>%
#   ecocomDP:::map_neon.ecocomdp.10043.001.001()


# load dev version of package in this dir
detach(package:ecocomDP)
devtools::load_all()

list_2021_new <- neon_data_list_full_2021 %>%
  ecocomDP:::map_neon.ecocomdp.10043.001.001()

list_2022_new <- neon_data_list_full_2022 %>%
  ecocomDP:::map_neon.ecocomdp.10043.001.001()

# flatten data and compare rows, col names
flat_2021_old <- list_2021_old %>% flatten_data()
flat_2021_new <- list_2021_new %>% flatten_data()
flat_2022_new <- list_2022_new %>% flatten_data()


# are any col names different from 2021 release between old and new mapping methods?

names(flat_2021_old) %>% setdiff(names(flat_2021_new))
# none missing

names(flat_2021_new) %>% setdiff(names(flat_2021_old))
# [1] "remarks_sorting"


nrow(flat_2021_old)
nrow(flat_2021_old %>% dplyr::distinct())
# [1] 95369

nrow(flat_2021_new)
# [1] 95360

flat_2021_old$observation_id %>% setdiff(flat_2021_new$observation_id)
flat_2021_new$observation_id %>% setdiff(flat_2021_old$observation_id)
# difference is not in obs_id, must be dups?


flat_2021_old$observation_id %>% duplicated() %>% sum()
# [1] 9
# there were duplicate records with old mapping 

dup_obs_id_list <- flat_2021_old$observation_id[flat_2021_old$observation_id %>% duplicated() %>% which()]

dup_old <- flat_2021_old %>% dplyr::filter(observation_id %in% dup_obs_id_list)
dup_new <- flat_2021_new %>% dplyr::filter(observation_id %in% dup_obs_id_list)
