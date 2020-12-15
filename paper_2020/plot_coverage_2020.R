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


# load data:
NEON_dataset_coverage <- read_csv("NEON_dataset_coverage.csv")


# manipulate data
# calc temporal cov for NEON
NEON_dataset_coverage$num_years <- (NEON_dataset_coverage$max_year - NEON_dataset_coverage$min_year) +1

# combine eric, colin files
# TO DO, Tuesday
combined_data <- NEON_dataset_coverage


# add colum for source
combined_data$source <- ifelse(grepl("DP1.", combined_data$dpid, ignore.case = F), "NEON", 
                ifelse(grepl("edi.", combined_data$dpid, ignore.case = F), "EDI", "Other"))

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
plot_geoCov <- ggplot(combined_data, aes(x=areaKm2)) + 
  geom_histogram( binwidth=15, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
  # ggtitle("Bin size = 15") +
  theme_minimal_hgrid()  +
  ylab("Number of Datasets") + 
  xlab("Area, km^2")

# temporal coverage
plot_temCov <-   ggplot(combined_data, aes(x=num_years)) + 
  geom_histogram( binwidth=1, fill="red", color="red", alpha=0.6) +
  theme_minimal_hgrid()  +
  ylab("Number of Datasets") + 
  xlab("Duration, years")      
                        
                
# temporal evenness
# place holder.
plot_temEvenness <- plot_temCov

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
ggsave("fig_x.png", p)
