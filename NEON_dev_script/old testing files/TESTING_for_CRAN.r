
rm(list=ls())
gc()

search_result <- ecocomDP::search_data("beetle")

# ----
# edi beetles
my_id <- "edi.251.1"
my_data_edi <- ecocomDP::read_data(my_id)
my_data_flat <- my_data_edi$tables %>% ecocomDP::flatten_data()
my_data_edi$tables %>% list2env(.GlobalEnv)

obs_anc_summary <- observation_ancillary %>%
  group_by(event_id, variable_name) %>%
  summarize(n_records = n(),
            n_unique_vals = length(unique(value)))

obs_anc_summary %>% dplyr::filter(n_records > 1)
obs_anc_summary %>% dplyr::filter(n_unique_vals > 1)




# ----
# edi mammals

search_result <- ecocomDP::search_data("mammal")
search_result %>% select(id, title)

# ----
my_id <- "edi.329.1"
my_data_edi <- ecocomDP::read_data(my_id)
my_data_flat <- my_data_edi$tables %>% ecocomDP::flatten_data()
my_data_edi$tables %>% list2env(.GlobalEnv)

obs_anc_summary <- observation_ancillary %>%
  group_by(event_id, variable_name) %>%
  summarize(n_records = n(),
            n_unique_vals = length(unique(value)))

obs_anc_summary %>% dplyr::filter(n_records > 1)
obs_anc_summary %>% dplyr::filter(n_unique_vals > 1)

# ----
my_id <- "edi.328.1"
my_data_edi <- ecocomDP::read_data(my_id)
my_data_flat <- my_data_edi$tables %>% ecocomDP::flatten_data()
my_data_edi$tables %>% list2env(.GlobalEnv)

obs_anc_summary <- observation_ancillary %>%
  group_by(event_id, variable_name) %>%
  summarize(n_records = n(),
            n_unique_vals = length(unique(value)))

obs_anc_summary %>% dplyr::filter(n_records > 1)
obs_anc_summary %>% dplyr::filter(n_unique_vals > 1)