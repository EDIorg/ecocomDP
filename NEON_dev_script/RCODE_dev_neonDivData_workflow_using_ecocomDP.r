# Create data package for tidyNEON data using ecocomDP
# Eric R. Sokol 
# 3/4/2021


# devtools::install_github("EDIorg/ecocomDP")
# library(ecocomDP)
library(tidyverse)


my_out_dir <- "neon_div_data"

# create out dir if it doesn't exist
if(!my_out_dir %in% list.files()) dir.create("neon_div_data")

# get list of available NEON data products
neon_ecocomdp_data_list <- ecocomDP::search_data("NEON")


data_set_log <- data.frame()

# i<-7 #macroinverts

for(i in 1:nrow(neon_ecocomdp_data_list)){
  
  # get method id
  my_dp_id <- neon_ecocomdp_data_list$id[i]
  
  # initialize vars
  data_list_i <- list()
  data_flat <- data.frame()
  
  # download data
  try({
    data_list <- read_data(
      id = my_dp_id,
      site= c('COMO','LECO'), 
      startdate = "2017-06",
      enddate = "2021-02",
      token = Sys.getenv("NEON_TOKEN"),
      check.size = FALSE)
    
    data_flat <- data_list[[1]]$tables %>% ecocomDP::flatten_ecocomDP()
    
  })
  
  
  if(length(data_list > 0) && nrow(data_flat) > 0){
    
    # get data set info for log
    data_set_log_i <- data.frame(
      taxon_group = neon_ecocomdp_data_list$title[i] %>% 
        stringr::str_split(" ") %>% unlist() %>% dplyr::first(),
      data_package_id = data_list[[1]]$tables$dataset_summary$package_id,
      n_taxa = data_flat$taxon_id %>% unique() %>% length(),
      sites = data_flat$siteID %>% unique() %>% paste(.,collapse = "|"),
      start_date = data_flat$observation_datetime %>% lubridate::as_date() %>% min(),
      end_date = data_flat$observation_datetime %>% lubridate::as_date() %>% max(),
      data_package_title = neon_ecocomdp_data_list$title[i],
      neon_ecocomdp_mapping_method = my_dp_id,
      original_neon_data_product_id = data_list[[1]]$tables$dataset_summary$original_package_id)
    
    # write data to out dir
    package_id <- data_list[[1]]$tables$dataset_summary$package_id
    package_filename <- paste0(my_out_dir, "/",package_id,".RDS")
    saveRDS(data_list, file = package_filename)
    
  }else{
    data_set_log_i <- data.frame(
      taxon_group = neon_ecocomdp_data_list$title[i] %>% 
        stringr::str_split(" ") %>% unlist() %>% dplyr::first(),
      data_package_id = "DATA NOT FOUND",
      n_taxa = NA,
      sites = NA_character_,
      start_date = NA_character_,
      end_date = NA_character_,
      data_package_title = neon_ecocomdp_data_list$title[i],
      neon_ecocomdp_mapping_method = my_dp_id,
      original_neon_data_product_id = neon_ecocomdp_data_list$source_id[i])
  }
  
  
  # log data set 
  data_set_log <- dplyr::bind_rows(data_set_log, data_set_log_i)
  
  # indexing
  message(paste0("Finished ",my_dp_id, " | ", i, " out of ", nrow(neon_ecocomdp_data_list)))
  
}


# write out data set log file
my_filename <- paste0(my_out_dir, "/dataset_log.txt")

readr::write_delim(
  data_set_log,
  path = my_filename,
  delim = "\t")
