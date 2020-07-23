
my_result <- map_neon_data_to_ecocomDP.BEETLE(site = c('ABBY','BARR'),
                                              startdate = "2019-06",
                                              enddate = "2019-09")





my_result <- map_neon_data_to_ecocomDP(
  neon.data.product.id = "DP1.10022.001",
  site = c('ABBY','BARR'),
  startdate = "2019-06",
  enddate = "2019-09")


my_result <- map_neon_data_to_ecocomDP.BEETLE(site = c('ABBY','BARR'),
                                              startdate = "2016-01", enddate = "2018-09",
                                              check.size = FALSE,
                                              token = Sys.getenv("NEON_TOKEN"))
