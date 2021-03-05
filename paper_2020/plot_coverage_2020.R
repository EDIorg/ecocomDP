# plot ds coverage for paper figure
# historgrams for 
# temporal coverage - num years
# temporal evenness (interval SD)
# geo coverage - square km
# taxon coverage - # taxa by group

# hsitogram and bar charts are stacked bars, e.g., for each taxon group, NEON= color 1, EDI=color 2
# to use multi labels and bars side-by-side, you would have to pre-bin and use a bar chart instead of a histogram.
# ggplot does not have an option to break the Y axis.

# set up
setwd('/Users/mob/Desktop/EDI_files_at_msi/EDI/thematic_standardization/ecocomDP_pop_com/git_checkouts/ecocomDP/paper_2020')
library(ggplot2)
library(cowplot)
library(readr)
library(dplyr)
library(data.table)


# load data:
# NEON_dataset_coverage <- read_csv("NEON_dataset_coverage.csv")
# EDI_dataset_coverage <- read_csv("EDI_dataset_coverage.csv") 
NEON_dataset_coverage <- data.table::fread("NEON_dataset_coverage.csv")
EDI_dataset_coverage <- data.table::fread("EDI_dataset_coverage.csv") 
## edit this file manually to remove some extra fields in rows 42, 60, 61

#, col_types = ("ccnnnnnnnnnc"))


# manipulate data
# calc temporal cov for NEON
# NEON_dataset_coverage$num_years <- (NEON_dataset_coverage$max_year - NEON_dataset_coverage$min_year) +1

# PREPARE SPATIAL, TEMPORAL data -------------------------------
# combine eric, colin files
# grab, rename a subset of incoming files for temporal, geo coverage.
NEON_1 <- data.frame(NEON_dataset_coverage$dpid, NEON_dataset_coverage$site_id, NEON_dataset_coverage$n_years, NEON_dataset_coverage$std_dev_interval_betw_years, NEON_dataset_coverage$areaKm2 )
colnames(NEON_1) <- c("L1_id", "site_id", "n_years", "sd_year_interval", "area_km2" )
NEON_1$area_m2 = NA

EDI_1 <- data.frame(EDI_dataset_coverage$L1_id, EDI_dataset_coverage$L0_id, EDI_dataset_coverage$n_years, EDI_dataset_coverage$std_dev_interval_betw_years, EDI_dataset_coverage$geo_extent_bounding_box_m2)
colnames(EDI_1) <- c("L1_id", "site_id", "n_years", "sd_year_interval", "area_m2" )
EDI_1$area_km2 <- EDI_dataset_coverage$geo_extent_bounding_box_m2/1000000

combined_geo_temporal_cov <- dplyr::bind_rows(NEON_1, EDI_1)

# add column for source
combined_geo_temporal_cov$Source <- ifelse(grepl("DP1.", combined_geo_temporal_cov$L1_id, ignore.case = F), "NEON", 
                ifelse(grepl("edi.", combined_geo_temporal_cov$L1_id, ignore.case = F), "EDI", 
                       ifelse(grepl("knb-lter", combined_geo_temporal_cov$L1_id, ignore.case = F), "EDI", 
                               "Other")))

# 
# PREPARE TAXON data --------------------------------------------
# Get EDI taxonomic info into long form
df <- dplyr::select(EDI_dataset_coverage, L1_id, class)

res <- data.frame(
  L1_id = NA_character_,
  class = NA_character_,
  stringsAsFactors = FALSE)

for (i in 1:nrow(df)) {
  row <- df[i, ]
  classes <- unlist(stringr::str_split(row$class, "\\|"))
  res <- dplyr::bind_rows(
    res,
    data.frame(
      L1_id = df[i, "L1_id"],
      class = classes,
      stringsAsFactors = FALSE))
}
# remove the first NA line (during setup); remove the entries with NA
res <- res[-1, ]
res <- res[res$class != "NA", ]
# add a column for labels
res$taxon_label <- NA

# Label taxa with larger group names
# use same labels for NEON and EDI data.
res$taxon_label <- 
  ifelse(grepl("Actinopterygii", res$class, ignore.case = T), "Fish", 
         ifelse(grepl("Amphibia", res$class, ignore.case = T), "Amphibian",
                ifelse(grepl("Anthozoa", res$class, ignore.case = T), "Cnidarian",  # Coral/Anemone
                ifelse(grepl("Arachnida", res$class, ignore.case = T), "Arthropod",
                ifelse(grepl("Aves", res$class, ignore.case = T), "Bird",
                ifelse(grepl("Bacillariophyceae|Chlorophyceae|Cyanophyceae|Florideophyceae|Phaeophyceae", res$class, ignore.case = T), "Algae",
                ifelse(grepl("Bivalvia|Cephalopoda", res$class, ignore.case = T), "Mollusc",
                ifelse(grepl("Branchiopoda", res$class, ignore.case = T), "Arthropod", # crustacean
                ifelse(grepl("Chilopoda", res$class, ignore.case = T), "Other terrestrial invertebrate",
                ifelse(grepl("Bryopsida", res$class, ignore.case = T), "Plant", # don't know breakdown for neon data, so all plants together.
                ifelse(grepl("Calcarea", res$class, ignore.case = T), "Sponge",
                ifelse(grepl("Chondrichthyes|Chondrostei|Elasmobranchii", res$class, ignore.case = T), "Fish",
                ifelse(grepl("Chondrostei", res$class, ignore.case = T), "Fish",
                ifelse(grepl("Clitellata", res$class, ignore.case = T), "Worm", # terrestrial or aquatic?
                ifelse(grepl("Collembola", res$class, ignore.case = T), "Arthropod", # centipede
                ifelse(grepl("Cycadopsida", res$class, ignore.case = T), "Plant",
                ifelse(grepl("Demospongiae", res$class, ignore.case = T), "Sponge",
                ifelse(grepl("Diplopoda", res$class, ignore.case = T), "Arthropod",
                ifelse(grepl("Echinoidea", res$class, ignore.case = T), "Echinoderm",
                ifelse(grepl("Ascidiacea|Entoprocta", res$class, ignore.case = T), "Other aquatic invertebrate",
                ifelse(grepl("Euchelicerata", res$class, ignore.case = T), "Arthropod",
                ifelse(grepl("Gastropoda", res$class, ignore.case = T), "Mollusc",
                ifelse(grepl("Granuloreticulosea", res$class, ignore.case = T), "Other", #Protozoan
                ifelse(grepl("Gymnolaemata", res$class, ignore.case = T), "Other aquatic invertebrate",
                ifelse(grepl("Holothuroidea", res$class, ignore.case = T), "Echinoderm",                                                          
                ifelse(grepl("Holostei", res$class, ignore.case = T), "Fish",                                                          
                ifelse(grepl("Hydrozoa", res$class, ignore.case = T), "Cnidarian",                                                          
                ifelse(grepl("Insecta", res$class, ignore.case = T), "Arthropod",                                                          
                ifelse(grepl("Jungermanniopsida", res$class, ignore.case = T), "Plant", # liverwort                                                          
                ifelse(grepl("Lycopodiopsida", res$class, ignore.case = T), "Plant", # herbaceous                                                          
                ifelse(grepl("Magnoliopsida|Gnetopsida", res$class, ignore.case = T), "Plant",                                                          
                ifelse(grepl("Malacostraca", res$class, ignore.case = T), "Arthropod", # crustacean                                                          
                ifelse(grepl("Mammalia", res$class, ignore.case = T), "Mammal",                                                           
                ifelse(grepl("Maxillopoda", res$class, ignore.case = T), "Arthropod", # crustaceans (copepods, barnacles)                                                          
                ifelse(grepl("Ophiuroidea|Asteroidea", res$class, ignore.case = T), "Echinoderm",                                                          
                ifelse(grepl("Ostracoda", res$class, ignore.case = T), "Arthropod", # crustacean                                                          
                ifelse(grepl("Pinopsida", res$class, ignore.case = T), "Plant", # cone bearing                                                          
                ifelse(grepl("Polychaeta", res$class, ignore.case = T), "Worm",                                                          
                ifelse(grepl("Polypodiopsida", res$class, ignore.case = T), "Plant", # fern                                                          
                ifelse(grepl("Polytrichopsida", res$class, ignore.case = T), "Plant", # mosses                                                          
                ifelse(grepl("Reptilia", res$class, ignore.case = T), "Reptile",                                                          
                ifelse(grepl("Sagittoidea", res$class, ignore.case = T), "Worm", # not annelid                                                          
                ifelse(grepl("Sphagnopsida", res$class, ignore.case = T), "Plant", # mosses                                                          
                ifelse(grepl("Stenolaemata", res$class, ignore.case = T), "Other aquatic invertebrate", # bryozoan                                                          
                ifelse(grepl("Teleostei", res$class, ignore.case = T), "Fish",                                                           
                ifelse(grepl("Trepaxonemata", res$class, ignore.case = T), "Other",                # Flatworm                                           
                ifelse(grepl("Ulvophyceae", res$class, ignore.case = T), "Algae",                                                           
                          NA
                )))))))))))))))))))))))))))))))))))))))))))))))
         
# set Labels for the datasets with no class avialable:
res <- within(res, taxon_label[L1_id == "edi.351.1"] <- "Plant")
res <- within(res, taxon_label[L1_id == "edi.328.1"] <- "Arthropod")
res <- within(res, taxon_label[L1_id == "edi.350.1"] <- "Other aquatic invertebrate")
res <- within(res, taxon_label[L1_id == "edi.338.1"] <- "Algae") #phytoplankton
res <- within(res, taxon_label[L1_id == "edi.349.1"] <- "Arthropod") #Lepidoptera
res <- within(res, taxon_label[L1_id == "edi.342.1"] <- "Plant")
res <- within(res, taxon_label[L1_id == "edi.333.1"] <- "Arthropod")
res <- within(res, taxon_label[L1_id == "knb-lter-ntl.344.5"] <- "Other")  # Microbe (OTU)

EDI_2_taxon <- res
                                     
#      ifelse(grepl("Cyanophyceae", res$class, ignore.case = T), "Algae",
#   ifelse(grepl("Ascidiacea", res$class, ignore.case = T), "Tunicate",
#   ifelse(grepl("Phaeophyceae", res$class, ignore.case = T), "Algae",                                                           
#     ifelse(grepl("Florideophyceae", res$class, ignore.case = T), "Algae",
#         ifelse(grepl("Chlorophyceae", res$class, ignore.case = T), "Algae",
#    ifelse(grepl("Elasmobranchii", res$class, ignore.case = T), "Fish",
# ifelse(grepl("Chondrichthyes|Chondrostei", res$class, ignore.case = T), "Fish",  NA )

                              
                              
                          
## NEON data
NEON_2_taxon <- data.frame(NEON_dataset_coverage$dpid, NEON_dataset_coverage$taxon_type)
colnames(NEON_2_taxon) <- c("L1_id", "class" )



# add column for taxonomic groups -- NEON
NEON_2_taxon$taxon_label <- 
  ifelse(grepl("algae", NEON_2_taxon$class, ignore.case = T), "Algae", 
         ifelse(grepl("macroinvertebrate", NEON_2_taxon$class, ignore.case = T), "Other terrestrial invertebrate", 
                ifelse(grepl("beetle", NEON_2_taxon$class, ignore.case = T), "Arthropod", 
                       ifelse(grepl("bird", NEON_2_taxon$class, ignore.case = T), "Bird", 
                              ifelse(grepl("fish", NEON_2_taxon$class, ignore.case = T), "Fish", 
                                     ifelse(grepl("mosquito", NEON_2_taxon$class, ignore.case = T), "Mosquito*", 
                                            ifelse(grepl("plant", NEON_2_taxon$class, ignore.case = T), "Plant", 
                                                   ifelse(grepl("small_mammal", NEON_2_taxon$class, ignore.case = T), "Mammal", 
                                                        ifelse(grepl("tick_pathogen", NEON_2_taxon$class, ignore.case = T), "Other", 
                                                              ifelse(grepl("tick", NEON_2_taxon$class, ignore.case = F), "Tick*", 
                                                                    "Other"))))))))))


## Combine edi, neon taxon coverage
combined_taxon_cov <- dplyr::bind_rows(NEON_2_taxon, EDI_2_taxon)
# add column for source
combined_taxon_cov$Source <- 
  ifelse(grepl("DP1.", combined_taxon_cov$L1_id, ignore.case = F), "NEON", 
         ifelse(grepl("edi.", combined_taxon_cov$L1_id, ignore.case = F), "EDI", 
                ifelse(grepl("knb-lter", combined_taxon_cov$L1_id, ignore.case = F), "EDI", 
                       "Other")))


# plots --------------------------------

# geom plot with 2 axes:
# might work: https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
# also try split_axis, or something like that (so you can enlarge the lower portion)

# geographic coverage
plot_geoCov <- ggplot(combined_geo_temporal_cov, aes(x=log10(area_km2), fill=Source ) ) + 
 # geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
  geom_histogram( binwidth=.5) +
  # ggtitle("Bin size = 15") +
  theme_minimal_hgrid()  +
  theme(text = element_text(size=12) )+ 
  ylab("") + 
  ylim(0,150) +
  xlab("Log 10 (Area, km^2)") + 
  guides(fill=FALSE)


# temporal coverage 
plot_temCov <-   ggplot(combined_geo_temporal_cov, aes(x=n_years, fill=Source)) + 
#   geom_histogram( binwidth=1, fill="red", color="red", alpha=0.6) +
  geom_histogram( binwidth=3) +
  theme_minimal_hgrid()  +
  theme(text = element_text(size=12) )+ 
  ylab("Number of Datasets") + 
  ylim(0,250) +
  xlab("Duration, years")  + 
  guides(fill=FALSE) # removes legend
                        
                
# temporal evenness
# place holder.
plot_temEvenness <- ggplot(combined_geo_temporal_cov, aes(x=sd_year_interval, fill=Source)) + 
#   geom_histogram( binwidth=.5, fill="green", color="green", alpha=0.6) +
  geom_histogram( binwidth=.3) +
  theme_minimal_hgrid()  +
  theme(text = element_text(size=12) )+ 
  ylab("") + 
  ylim(0,400) +
  xlab("S.D. sampling interval") + 
  guides(fill=FALSE)
  

# taxonomic coverage
# with hints from:
# https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot/7267364
plot_taxonCov <- ggplot(data=combined_taxon_cov, aes(x=taxon_label, fill = Source)) +
  # geom_bar(stat="count", fill="blue", color="blue", alpha=0.5) +
  geom_bar(stat="count") +
  # scale_y_continuous(expand=c(0,0)) + # removes space between the bar and the tick mark
  theme_minimal_hgrid() + 
  theme(text = element_text(size=12) )+ 
  ylab("Number of Datasets") +
  # ylim(0,120) +
  xlab("Taxon Group")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) ) +  # hjust sets postition of text
  guides(fill=FALSE)



# arrange plots
# cowplot recommendation from: https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
# nested grid of plots. taxon on the bottom, wide, geo/temporal above, 1/3 width each.
bottom_row <- plot_grid(plot_taxonCov, labels = c("D"), label_size = 12)
top_row <- plot_grid(
  plot_temCov, plot_temEvenness, plot_geoCov,  
  labels = c("A","B","C"), label_size = 12,
  nrow = 1)

p <- plot_grid(top_row, bottom_row, nrow = 2, rel_heights = c(0.8,1))
ggsave("fig_x.png", p)
