# data_list_i <- ecocomDP::read_data(
#   id = "neon.ecocomdp.10043.001.001",
#   # site= c('COMO','LECO','BART','NIWO'),
#   # startdate = "2017-06",
#   # enddate = "2019-02",
#   neon.data.save.dir = "NEON_raw_data",
#   token = Sys.getenv("NEON_TOKEN"),
#   check.size = FALSE
# )


data_list_i <- ecocomDP::read_data(
  id = "neon.ecocomdp.10043.001.001",
  site= "BART",
  startdate = "2017-06",
  enddate = "2019-02",
  neon.data.save.dir = "NEON_raw_data",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE,
  release = "RELEASE-2022"
)

ecocomDP::plot_sample_space_time(data_list_i)
# ecocomDP::plot_sites(data_list_i)
ecocomDP::plot_taxa_abund(data_list_i)
ecocomDP::plot_taxa_accum_sites(data_list_i)
ecocomDP::plot_taxa_rank(data_list_i,facet_var = "siteID")
ecocomDP::plot_taxa_occur_freq(data_list_i,min_occurrence = 10,facet_var = "siteID")

dat_flat <- data_list_i %>% ecocomDP::flatten_data()
dat_flat %>% dplyr::filter(siteID == c("TREE","BART")) %>%
  ecocomDP::plot_taxa_diversity(time_window_size = "year")
