#' Plot community metrics
#'
#' @param dataset (list) Data object returned by \code{read_data()} (? list of named datapackage$tables)
#' @param type (character) Type of plots to create. Options: "community"
#' @param path (character) Path to directory where plots will be written
#' @param alpha (numeric) Alpha-transparency scale between 0 and 1, where 1 is 100% opaque
#'
#' @return
#' \item{alpha_diversity.pdf}{Alpha diversity (species richness) over time and space}
#' \item{sampling_effort.pdf}{Spatiotemporal sampling effort}
#' \item{sp_accumulation_over_space.pdf}{Species accumulation curve over space}
#' \item{sp_accumulation_over_time.pdf}{Species accumulation curves over time (site-specific and total)}
#' \item{sp_shared_among_sites.pdf}{Species shared among sites}
#' 
#' @details LIST METHODS OF CALCULATION FOR EACH PLOT
#' 
#' @note LIST DATA AGGREGATION REQUIREMENTS FOR THIS FUNC TO WORK.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # one dataset
#' # - summarizing sometimes required
#' # more than one dataset
#' }
#' 
plot_data <- function(dataset, type, path, alpha = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {           # ggplot2 is a suggested package
    stop("Package 'ggplot2' is required but is not installed", call. = FALSE)
  }
  if (type == "community") {                                    # plot community
    id <- names(dataset)                                        # dataset id
    ds <- format_for_comm_plots(dataset, id)                    # intermediate format for plotting
    dslong <- ds %>%                                            # long form
      dplyr::mutate_at(dplyr::vars(c(VALUE)), as.numeric) %>%
      dplyr::mutate_at(dplyr::vars(SITE_ID), as.character)
    dswide <- dslong %>%                                        # wide form
      dplyr::select(-VARIABLE_UNITS) %>%
      tidyr::pivot_wider(names_from = VARIABLE_NAME, values_from = VALUE)
    vunit <- unique(dslong$VARIABLE_UNITS)                      # variable unit
    plot_alpha_diversity(dslong, id,  path, vunit, alpha)
    plot_sampling_effort(dslong, id, path, alpha)
    plot_sp_accumulation_over_space(dslong, id, path, vunit)
    plot_sp_accumulation_over_time(dslong, id, path, vunit, alpha)
    plot_sp_shared_among_sites(dswide, id, path, vunit)
  }
}








#' Plot alpha diversity (species richness) over time and space
#'
#' @param dslong (data.frame) Long form of return from \code{format_for_comm_plots()}
#' @param id (character) Dataset id
#' @param path (character) Path to where plots will be written
#' @param vunit (character) Unit of measurement variable
#' @param alpha (numeric) Alpha-transparency scale between 0 and 1, where 1 is 100% opaque
#'
#' @return
#' \item{alpha_diversity.pdf}{Alpha diversity (species richness) over time and space}
#' 
#' @import dplyr
#' @import ggplot2
#' 
plot_alpha_diversity <- function(dslong, id, path, vunit, alpha) {
  message("Plotting ", id, " alpha diversity")
  # Calculate num unique taxa at each site through time and num unique taxa among all sites through time
  calc_ntaxa <- function(ex) {
    ntaxa <- ex %>%
      filter(VALUE > 0) %>% 
      select(DATE, VARIABLE_NAME, SITE_ID) %>% 
      unique() %>% 
      mutate(ntaxa = 1) %>% 
      group_by(SITE_ID, DATE) %>% 
      summarize(ntaxa = sum(ntaxa))
    total_ntaxa <- ex %>%
      filter(VALUE > 0) %>%
      select(DATE, VARIABLE_NAME) %>%
      unique() %>%
      mutate(ntaxa = 1) %>%
      group_by(DATE) %>%
      summarize(ntaxa = sum(ntaxa))
    return(list(ntaxa = ntaxa, total_ntaxa = total_ntaxa))
  }
  ntaxa <- calc_ntaxa(dslong)
  # Plot
  ggplot() +
    geom_point(data = ntaxa$ntaxa, aes(x = DATE, y = ntaxa, group = SITE_ID, color = SITE_ID), alpha = alpha) + 
    geom_line(data = ntaxa$ntaxa, aes(x = DATE, y = ntaxa, group = SITE_ID, color = SITE_ID), alpha = alpha) + 
    geom_point(data = ntaxa$total_ntaxa, aes(x = DATE, y = ntaxa, fill = ""), color="black") + 
    geom_line(data = ntaxa$total_ntaxa, aes(x = DATE, y = ntaxa, group = 1), color="black") + 
    labs(title = "Alpha diversity (species richness) over time and space", subtitle = id) +
    xlab("Year") +
    ylab(paste0("Taxa observed (", vunit, ")")) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") + 
    guides(
      color = guide_legend(title = "Site", label.theme = element_text(size = 6)),
      fill = guide_legend(title = "All sites")) +
    ylim(c(0, max(ntaxa$total_ntaxa$ntaxa))) +
    theme_bw()
  ggsave(
    filename = paste(path, "/", id, '_alpha_diversity.pdf',sep=''), 
    width = 12, 
    height = 7, 
    units = "in")
}








#' Plot spatiotemporal sampling effort
#'
#' @param dslong (data.frame) Long form of return from \code{format_for_comm_plots()}
#' @param id (character) Dataset id
#' @param path (character) Path to where plots will be written
#' @param alpha (numeric) Alpha-transparency scale between 0 and 1, where 1 is 100% opaque
#' 
#' @return
#' \item{sampling_effort.pdf}{Spatiotemporal sampling effort}
#' 
#' @import ggplot2
#' 
plot_sampling_effort <- function(dslong, id, path, alpha) {
  message("Plotting ", id, " spatiotemporal sampling effort")
  # Scale font size
  uniy <- length(unique(dslong$SITE_ID))
  if (uniy < 30) {
    txty <- NULL
  } else if (uniy < 60) {
    txty <- 6
  } else if (uniy >= 60) {
    txty <- 4
  }
  # Plot
  ggplot() +
    geom_point(data = dslong, aes(x = DATE, y = SITE_ID), alpha = alpha) +
    theme_bw() +
    labs(title = "Spatiotemporal sampling effort", subtitle = id) +
    xlab("Year") +
    ylab("Site") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") +
    theme_bw() +
    theme(
      axis.text.y.left = element_text(size = txty),
      plot.margin = margin(0.1, 0.25, 0.1, 0.1, "in"))
  ggsave(
    file = paste(path, "/", id, '_sampling_effort.pdf',sep = ''),
    width = 12,
    height = 7,
    units = "in")
}








#' Plot species accumulation curve over space
#'
#' @param dslong (data.frame) Long form of return from \code{format_for_comm_plots()}
#' @param id (character) Dataset id
#' @param path (character) Path to where plots will be written
#' @param vunit (character) Unit of measurement variable
#'
#' @return
#' \item{sp_accumulation_over_space.pdf}{Species accumulation curve over space}
#' 
plot_sp_accumulation_over_space <- function(dslong, id, path, vunit) {
  message("Plotting ", id, " species accumulation over space")
  # Calculate cumulative number of taxa
  calc_cuml_taxa_space <- function(ex) {
    taxa.s.list <- list()
    sites <- unique(ex$SITE_ID)
    for(t in 1:length(unique(ex$SITE_ID))){                            # unique taxa found in each year
      tmp.dat <- subset(ex, ex$SITE_ID == sites[t])
      tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
      taxa.s.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
    }
    cuml.taxa.space <- list()                                          # cumulative list of taxa over space
    cuml.taxa.space[[1]] <- taxa.s.list[[1]]
    if (length(unique(ex$SITE_ID)) > 1) {
      for(t in 2:length(unique(ex$SITE_ID))){                            # list cumulative taxa, with duplicates
        cuml.taxa.space[[t]] <- c(cuml.taxa.space[[t - 1]], taxa.s.list[[t]])
      }
    }
    cuml.taxa.space <- lapply(cuml.taxa.space, function(x) {unique(x)})# rm duplicates
    cuml.no.taxa.space <- data.frame("site" = unique(ex$SITE_ID))      # total unique taxa over space
    cuml.no.taxa.space$no.taxa <- unlist(lapply(cuml.taxa.space, function(x) {length(x)}))
    return(cuml.no.taxa.space)
  }
  comm.dat <- dslong %>% arrange(SITE_ID)                              # order by site
  no.taxa.space <- calc_cuml_taxa_space(comm.dat)
  no.taxa.space$no.site <- as.numeric(rownames(no.taxa.space))
  # Plot
  ggplot(data = no.taxa.space, aes(x = no.site, y = no.taxa)) + 
    geom_point() +
    geom_line() +
    labs(title = "Species accumulation curve over space", subtitle = id) +
    xlab("Cumulative number of sites") +
    ylab(paste0("Cumulative taxa observed (", vunit, ")")) +
    theme_bw()
  ggsave(
    file = paste(path, "/", id,'_sp_accumulation_over_space.pdf', sep=''),
    width = 12,
    height = 7,
    units = "in")
}








#' Plot species accumulation curves over time (site-specific and total)
#'
#' @param dslong (data.frame) Long form of return from \code{format_for_comm_plots()}
#' @param id (character) Dataset id
#' @param path (character) Path to where plots will be written
#' @param vunit (character) Unit of measurement variable
#' @param alpha (numeric) Alpha-transparency scale between 0 and 1, where 1 is 100% opaque
#'
#' @return
#' \item{sp_accumulation_over_time.pdf}{Species accumulation curves over time (site-specific and total)}
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' 
plot_sp_accumulation_over_time <- function(dslong, id, path, vunit, alpha) {
  message("Plotting ", id, " species accumulation over time")
  # Calculate cumulative number of taxa 
  cuml.taxa.fun <- function(ex){
    taxa.t.list <- list()
    dates <- unique(ex$DATE)
    for(t in 1:length(unique(ex$DATE))) {                                          # unique taxa found in each year
      tmp.dat <- subset(ex, ex$DATE == dates[t])
      tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
      taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
    }
    cuml.taxa <- list()                                                            # cumulative taxa through time
    cuml.taxa[[1]] <- taxa.t.list[[1]]
    if (length(unique(ex$DATE)) > 1) {                                             # create list of the cumulative taxa, with duplicates
      for(t in 2:length(unique(ex$DATE))){ 
        cuml.taxa[[t]] <- c(cuml.taxa[[t - 1]], taxa.t.list[[t]])
      }
    }
    cuml.taxa <- lapply(cuml.taxa, function(x){unique(x)})                         # rm duplicates
    cuml.no.taxa <- data.frame("year" = unique(ex$DATE), stringsAsFactors = FALSE) # number of unique taxa through time
    cuml.no.taxa$no.taxa <- unlist(lapply(cuml.taxa, function(x){length(x)}))
    return(cuml.no.taxa)
  }
  cuml.taxa.all.sites <- cuml.taxa.fun(ex = dslong)   # species accumulation across all sites pooled together
  comm.dat <- dslong %>% arrange(SITE_ID)             # order by site
  X <- split(comm.dat, as.factor(comm.dat$SITE_ID))   # cumulative number of taxa for each site
  out <- lapply(X, cuml.taxa.fun)
  out[names(out) %in% "lo_2_115"]                     # list to dataframe
  output <- do.call("rbind", out)
  output$rnames <- row.names(output)                  # create SITE_ID column
  cuml.taxa.by.site <- suppressWarnings(              # Clean up the SITE_ID column. Warning sent when site_id only has one observation (non-issue), an artifact of do.call("rbind", out)
    output %>%
      tbl_df() %>%
      separate(rnames, c("SITE_ID", "todrop"), sep = "\\.") %>%
      select(-todrop))
  # Plot
  ggplot() +
    geom_point(data = cuml.taxa.by.site, aes(x = year, y = no.taxa, color = SITE_ID), alpha = alpha) +
    geom_line(data = cuml.taxa.by.site, aes(x = year, y = no.taxa, color = SITE_ID), alpha = alpha) +
    geom_point(data = cuml.taxa.all.sites, aes(x = year, y = no.taxa, fill = "")) +
    geom_line(data = cuml.taxa.all.sites, aes(x = year, y = no.taxa)) +
    labs(title = "Species accumulation curves over time (site-specific and total)", subtitle = id) +
    xlab("Year") +
    ylab(paste0("Cumulative taxa observed (", vunit, ")")) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") + 
    guides(color = guide_legend(title = "Site", label.theme = element_text(size = 6)),
           fill = guide_legend(title = "All sites")) +
    ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
    theme_bw()
  ggsave(
    file = paste(path, "/", id,'_sp_accumulation_over_time.pdf', sep = ''),
    width = 12,
    height = 7,
    units = "in")
}








#' Plot species shared among sites
#' 
#' @param dswide (data.frame) Wide form of return from \code{format_for_comm_plots()}
#' @param id (character) Dataset id
#' @param path (character) Path to where plots will be written
#' @param vunit (character) Unit of measurement variable
#'
#' @return
#' \item{sp_shared_among_sites.pdf}{Species shared among each site}
#' 
#' @import vegan
#' 
plot_sp_shared_among_sites <- function(dswide, id, path, vunit) {
  message("Plotting ", id, " species shared among sites")
  heat_pal_spectral <- colorRampPalette(rev( RColorBrewer::brewer.pal(11, "Spectral")))
  # Count species shared between sites in a site by species matrix
  shared.species <- function(comm, output = "matrix"){
    sites <- comm[, 1]
    share.mat <- matrix(NA, nrow = length(sites), ncol = length(sites), dimnames = list(sites, sites))
    site.pairs <- expand.grid(site1 = sites, site2 = sites)
    for(pair in 1:nrow(site.pairs)){
      site1 <- comm[site.pairs$site1[pair],][,-1] # pull out each site combo
      site2 <- comm[site.pairs$site2[pair],][,-1]
      if(output == "matrix"){                     # count shared species
        share.mat[site.pairs$site1[pair],site.pairs$site2[pair]] <- sum(site1 == 1 & site2 == 1)
      }
      if(output == "dataframe"){
        site.pairs[pair,"shared"] <- sum(site1 == 1 & site2 == 1)
      }
    }
    if(output == "matrix") return(share.mat)
    if(output == "dataframe") return(site.pairs)
  }
  
  comm.cumul <- dswide %>%                  # aggregate years by cumulative abundances
    group_by(SITE_ID) %>% 
    select(-OBSERVATION_TYPE, -DATE) %>%
    summarise_all(sum)
  comm.cumul[is.na(comm.cumul)] <- 0
  dswide.pa <- cbind(comm.cumul[,1], decostand(comm.cumul[,-1], method = "pa", na.rm = TRUE))
  shared.taxa <- shared.species(dswide.pa, output = "dataframe")
  uniy <- length(unique(shared.taxa$site1)) # scale font size
  if (uniy < 30) {
    txty <- NULL
  } else if (uniy < 60) {
    txty <- 6
  } else if (uniy >= 60) {
    txty <- 4
  }
  # Plot
  ggplot(shared.taxa, aes(x = site1, y = site2, fill = shared)) +
    geom_raster() +
    scale_fill_gradientn(colours = heat_pal_spectral(100), name = paste0("Taxa shared (", vunit, ")")) +
    theme_bw() +
    labs(title = "Plot species shared among each site", subtitle = id) +
    xlab("Site") +
    ylab("Site") +
    theme(
      aspect.ratio = 1, 
      axis.text.x.bottom = element_text(size = txty, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y.left = element_text(size = txty))
  ggsave(
    file = paste(path, "/", id,'_sp_shared_among_sites.pdf', sep=''),
    width = 12,
    height = 7,
    units = "in")
}








#' Format dataset for community plotting functions
#'
#' @param dataset (list) Data object returned by \code{read_data()} (? list of named datapackage$tables)
#' @param id (character) Dataset id
#' 
#' @details Downsteam plotting functions are based on \href{https://github.com/sokole/ltermetacommunities/tree/master/Group2-explore-data}{LTER Metacommunities code} and use their intermediate data input format.
#'
#' @return (data.frame) Tabular data of \code{id} in a format compatible with plotting functions
#' 
format_for_comm_plots <- function(dataset, id) {
  observation <- dataset[[1]]$tables$observation
  # Constraints
  varname <- unique(observation[c("variable_name", "unit")])                 # Can only handle one variable
  if (nrow(varname) > 1) {
    warning("The observation table of ", id, " has more than one variable ",
            "and unit combination:\n", paste0("  variable = ", 
                                              varname$variable_name, 
                                              ", unit = ", varname$unit, 
                                              collapse = "\n"), 
            "\n plot_data() can only handle one. Consider splitting this ",
            "dataset. Using the first set and dropping the others.", 
            call. = FALSE)
    observation <- dplyr::filter(observation, 
                                   variable_name == varname$variable_name[1] & 
                                     unit == varname$unit[1])
  }

  # Duplicate observation handling
  dups <- observation %>% dplyr::select(-observation_id, -value, -event_id) %>% duplicated()
  if (any(dups)) {                                                          # Only unique observations allowed
    warning("The observation table of ", id, " has ", sum(dups), " duplicate ",
            "observations. Consider aggregating these observations by date ",
            "and location before passing to plot_data(). Dropping duplicates ",
            "and continuing. ", call. = FALSE)
    observation <- observation[!dups, ]
  }
  
  # Use dates
  if (is.character(observation$observation_datetime)) {
    warning("Input datetimes are character strings. Accuracy may be improved",
            " if inputs are parsed before calling plot_data().", call. = FALSE)
  }
  observation$observation_datetime <- lubridate::date(observation$observation_datetime)
  
  obs <- observation
  taxon_count <- data.frame(
    OBSERVATION_TYPE = "TAXON_COUNT",
    SITE_ID = obs$location_id,
    DATE = obs$observation_datetime,
    VARIABLE_NAME = obs$taxon_id,
    VARIABLE_UNITS = obs$variable_name,
    VALUE = obs$value,
    stringsAsFactors = FALSE)
  return(taxon_count)
}
