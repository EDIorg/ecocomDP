# plot ds coverage for paper figure
# historgrams for 
# temporal coverage - num years
# temporal evenness (interval SD)
# geo coverage - square km
# taxon coverage - # taxa by group

# you could try stacking the bars, e.g., for each taxon group, NEON= color 1, EDI=color 2


# set up
setwd('/Users/mob/Desktop/EDI_files_at_msi/EDI/thematic_standardization/ecocomDP_pop_com/git_checkouts/ecocomDP/paper_2020')
library(ggplot2)
library(cowplot)
library(readr)
library(dplyr)


# load data:
NEON_dataset_coverage <- read_csv("NEON_dataset_coverage.csv")
EDI_dataset_coverage <- read_csv("EDI_dataset_coverage.csv") 
## edit this file manually to remove some extra fields in rows 42, 60, 61

#, col_types = ("ccnnnnnnnnnc"))


# manipulate data
# calc temporal cov for NEON
# NEON_dataset_coverage$num_years <- (NEON_dataset_coverage$max_year - NEON_dataset_coverage$min_year) +1

# combine eric, colin files
# grab, rename a subset of incoming files for temporal, geo coverage.
NEON_1 <- data.frame(NEON_dataset_coverage$dpid, NEON_dataset_coverage$site_id, NEON_dataset_coverage$n_years, NEON_dataset_coverage$std_dev_interval_betw_years, NEON_dataset_coverage$areaKm2 )
colnames(NEON_1) <- c("L1_id", "site_id", "n_years", "sd_year_interval", "area_km2" )
NEON_1$area_m2 = NA

EDI_1 <- data.frame(EDI_dataset_coverage$L1_id, EDI_dataset_coverage$L0_id, EDI_dataset_coverage$n_years, EDI_dataset_coverage$std_dev_interval_betw_years, EDI_dataset_coverage$geo_extent_bounding_box_m2)
colnames(EDI_1) <- c("L1_id", "site_id", "n_years", "sd_year_interval", "area_m2" )
EDI_1$area_km2 <- EDI_dataset_coverage$geo_extent_bounding_box_m2/1000000

combined_geo_temporal_cov <- dplyr::bind_rows(NEON_1, EDI_1)

# add colum for source
combined_geo_temporal_cov$Source <- ifelse(grepl("DP1.", combined_geo_temporal_cov$L1_id, ignore.case = F), "NEON", 
                ifelse(grepl("edi.", combined_geo_temporal_cov$L1_id, ignore.case = F), "EDI", 
                       ifelse(grepl("knb-lter", combined_geo_temporal_cov$L1_id, ignore.case = F), "EDI", 
                               "Other")))





# add column for taxonomic groups
combined_data$taxon_label <- ifelse(grepl("algae", combined_data$taxon_type, ignore.case = T), "Algae", 
                      ifelse(grepl("macroinvertebrate", combined_data$taxon_type, ignore.case = T), "Macroinvertebrate", 
                             ifelse(grepl("beetle", combined_data$taxon_type, ignore.case = T), "Beetle", 
                                    ifelse(grepl("bird", combined_data$taxon_type, ignore.case = T), "Bird", 
                                           ifelse(grepl("fish", combined_data$taxon_type, ignore.case = T), "Fish", 
                                                  ifelse(grepl("mosquito", combined_data$taxon_type, ignore.case = T), "Mosquito", 
                                                         ifelse(grepl("plant", combined_data$taxon_type, ignore.case = T), "Plant", 
                                                                ifelse(grepl("small_mammal", combined_data$taxon_type, ignore.case = T), "Small Mammal", 
                                                                       ifelse(grepl("Tick", combined_data$taxon_type, ignore.case = T), "Tick", 
                                                                              ifelse(grepl("tick_pathogen", combined_data$taxon_type, ignore.case = T), "Tick Pathogen", 
                             "Other"))))))))))



# plots

# geographic coverage
plot_geoCov <- ggplot(combined_geo_temporal_cov, aes(x=log10(area_km2), fill=Source ) ) + 
 # geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
  geom_histogram( binwidth=.5) +
  # ggtitle("Bin size = 15") +
  theme_minimal_hgrid()  +
  ylab("Number of Datasets") + 
  xlab("Log 10 (Area, km^2)")


# geo plot with 2 axes:
# might work: https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html









# temporal coverage IN PROGRESS
plot_temCov <-   ggplot(combined_geo_temporal_cov, aes(x=n_years, fill=Source)) + 
#   geom_histogram( binwidth=1, fill="red", color="red", alpha=0.6) +
  geom_histogram( binwidth=3) +
  theme_minimal_hgrid()  +
  ylab("Number of Datasets") + 
  xlab("Duration, years")      
                        
                
# temporal evenness
# place holder.
plot_temEvenness <- ggplot(combined_geo_temporal_cov, aes(x=sd_year_interval, fill=Source)) + 
#   geom_histogram( binwidth=.5, fill="green", color="green", alpha=0.6) +
  geom_histogram( binwidth=.3) +
  theme_minimal_hgrid()  +
  ylab("") + 
  xlab("S.D. interval between years")

# taxonomic coverage
# with hints from:
# https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot/7267364
plot_taxonCov <- ggplot(data=combined_data, aes(x=taxon_label)) +
  geom_bar(stat="count", fill="blue", color="blue", alpha=0.5) +
  # scale_y_continuous(expand=c(0,0)) + # removes space between the bar and the tick mark
  theme_minimal_hgrid() + 
  ylab("") + 
  xlab("Taxon Group")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )  # hjust sets postition of text
    


# arrange plots
# cowplot recommendation from: https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
p <- plot_grid(plot_temCov, plot_temEvenness, plot_geoCov, plot_taxonCov, labels = "AUTO")
p <- plot_grid(plot_temCov, plot_temEvenness, plot_geoCov, labels = "AUTO")

ggsave("fig_x.png", p)
