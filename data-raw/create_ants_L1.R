# This script creates the example dataset "ants_L1"

library(ecocomDP)

# Read from EDI data repository
ants_L1 <- read_data(id = "edi.193.5")

# Return as tibble
ants_L1$edi.193.5$tables <- lapply(ants_L1$edi.193.5$tables, tidyr::as_tibble)

# Save to /data
usethis::use_data(ants_L1, overwrite = TRUE)
