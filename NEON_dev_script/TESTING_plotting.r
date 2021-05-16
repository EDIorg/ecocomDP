
rm(list=ls())

# algae
my_result_read_data <- read(
  id = "neon.ecocomdp.20166.001.001",
  site = c('COMO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

# macroinverts
my_result_read_data <- read(
  id = "neon.ecocomdp.20120.001.001",
  site= c('COMO','LECO','SUGG'), 
  startdate = "2017-06",
  enddate = "2019-09",
  token = Sys.getenv("NEON_TOKEN"),
  check.size = FALSE)

flat_data <- my_result_read_data[[1]]$tables %>% flatten()

my_result_read_data %>% ecocomDP::plot_alpha_diversity()
my_result_read_data %>% ecocomDP::plot_sampling_times()
my_result_read_data %>% ecocomDP::plot_taxa_accum_sites()
my_result_read_data %>% ecocomDP::plot_taxa_accum_time()
my_result_read_data %>% ecocomDP::plot_taxa_shared_sites()

