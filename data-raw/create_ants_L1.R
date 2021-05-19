# This script creates the example dataset "ants_L1"

library(ecocomDP)

# Read from EDI data repository
ants_L1 <- read_data(id = "edi.193.4")

# Save to /data
usethis::use_data(ants_L1)