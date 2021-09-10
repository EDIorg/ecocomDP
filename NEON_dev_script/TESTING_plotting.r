
rm(list=ls())

devtools::load_all()


# # algae
# my_dataset <- read_data(
#   id = "neon.ecocomdp.20166.001.001",
#   site = c('COMO','SUGG'), 
#   startdate = "2017-06",
#   enddate = "2019-09",
#   token = Sys.getenv("NEON_TOKEN"),
#   check.size = FALSE)

# macroinverts
my_dataset <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)




# accum by site

plot_taxa_accum_sites(
  dataset = my_dataset)

plot_taxa_accum_sites(
  flat_table = my_dataset %>% flatten_data())

plot_taxa_accum_sites(
  dataset = ants_L1)

plot_taxa_accum_sites(
  flat_table = ants_L1 %>% flatten_data())




# accum by time

# looks weird for SUGG for macroinvert dataset using both methods
plot_taxa_accum_time(
  dataset = my_dataset)

plot_taxa_accum_time(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

# ants looks okay
plot_taxa_accum_time(
  dataset = ants_L1)







# richness by time

# this is also messed up for macroinverts -- COMO is weird
# RENAME from "plot_taxa_diversity" to "plot_richness_time"
plot_taxa_richness(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))





# sample coverage by site and time
# RENAME 
plot_sample_space_time(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

plot_sample_space_time(
  dataset = my_dataset)

plot_sample_space_time(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

plot_sample_space_time(
  dataset = ants_L1)


# plot shared taxa across sites -- this seems to work fine
plot_taxa_shared_sites(
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))

plot_taxa_shared_sites(
  dataset = my_dataset)

plot_taxa_shared_sites(
  observation = ants_L1[[1]]$tables$observation,
  id = names(ants_L1))

plot_taxa_shared_sites(
  dataset = ants_L1)





# plot rank frequencies
# this is in taxon table... should we make an option to plot frequencies in the actual data?
plot_taxa_rank(
  observation = my_dataset[[1]]$tables$observation,
  taxon = my_dataset[[1]]$tables$taxon,
  id = names(my_dataset))

plot_taxa_rank(
  dataset = my_dataset)


plot_taxa_rank(
  facet_var = "location_id", #e.g., "location_id", "datetime" must be a column name in observation or taxon table
  taxon = my_dataset[[1]]$tables$taxon, 
  observation = my_dataset[[1]]$tables$observation, 
  id = names(my_dataset))


plot_taxa_rank(
  dataset = my_dataset,
  facet_var = "location_id") #e.g., "location_id", "datetime" must be a column name in observation or taxon table


plot_taxa_rank(
  dataset = my_dataset,
  facet_var = "datetime") #e.g., "location_id", "datetime" must be a column name in observation or taxon table

plot_taxa_rank(
  dataset = ants_L1,
  facet_var = "datetime") 




# Plot stacked taxa by site
plot_stacked_taxa_by_site(
  tables = my_dataset[[1]]$tables, 
  id = names(my_dataset), 
  rank = "species")




# plot map of sites
plot_sites(
  dataset = ants_L1)

plot_sites(
  flat_table = ants_L1[[1]]$tables %>% flatten_tables())

plot_sites(
  dataset = ants_L1,
  location = ants_L1[[1]]$tables$location)

plot_sites(
  location = ants_L1[[1]]$tables$location,
  observation = ants_L1[[1]]$tables$observation,
  id = names(ants_L1))



plot_sites(
  dataset = my_dataset)

plot_sites(
  flat_table = my_dataset %>% flatten_data())

plot_sites(
  dataset = my_dataset,
  location = my_dataset[[1]]$tables$location)

plot_sites(
  location = my_dataset[[1]]$tables$location,
  observation = my_dataset[[1]]$tables$observation,
  id = names(my_dataset))


my_dataset[[1]]$tables %>% flatten_data()
my_dataset %>% flatten_data()

# fails as expected with error message
list(a = c(1:5)) %>% flatten_data()


flat <- flatten_data(tables = dataset[[1]]$tables)
flat

flat <- flatten_data(dataset = ants_L1)
flat

flat <- flatten_dataset(ants_L1)
flat

flat <- flatten_tables(ants_L1[[1]]$tables)
flat
