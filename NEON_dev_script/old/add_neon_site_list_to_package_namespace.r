# add NEON site list to package data
neon_site_list <- readr::read_csv("neon-field-sites.csv")
usethis::use_data(neon_site_list,
                  internal = TRUE)
