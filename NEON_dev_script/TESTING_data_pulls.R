library(ecocomDP)
library(tidyverse)


a_packages <- ecocomDP::search_data("macroinvert")

my_data_package <- ecocomDP::read_data("edi.284.1")

# doesn't work
my_data_package <- ecocomDP::read_data("edi.290.1")

# does work
my_data_package <- ecocomDP:::read_data_edi("edi.290.1")




my_data_package <- ecocomDP::read_data("DP1.20120.001",
                                       token = Sys.getenv("NEON_TOKEN"))