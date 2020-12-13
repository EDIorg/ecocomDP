# plot ds coverage for paper figure
# historgrams for 
# temporal coverage - num years
# geo coverage - square km
# taxon coverage - # taxa by group


# set up
setwd('/Users/mob/Desktop/EDI_files_at_msi/EDI/thematic_standardization/ecocomDP_pop_com/git_checkouts/ecocomDP/paper_2020')
library(ggplot2)
library(cowplot)
library(readr)


# load data:
NEON_dataset_coverage <- read_csv("NEON_dataset_coverage.csv")


# manipulate data
# combine eric, colin files
# calc temporal cov for NEON


# plots



plot_geoCov <- qplot(NEON_dataset_coverage$areaKm2, geom="histogram",
      binwidth = 10,  
      main = "Area", 
      xlab = "km^2",  
      fill=I("blue"), 
      col=I("black"), 
      alpha=I(.2) 
)



plot_temCov <- qplot(NEON_dataset_coverage$min_year, geom="histogram",
      binwidth = 1,  
      main = "Temporal", 
      xlab = "Year",  
      fill=I("red"), 
      col=I("black"), 
      alpha=I(.2) 
)

# arrange plots
# cowplot recommendation from: https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
p <- plot_grid(plot_geoCov, plot_temCov, labels = "AUTO")
ggsave("fig_x.png", p)
