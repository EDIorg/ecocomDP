#' Plot taxa accumulation by site accumulation
#'
#' @param dataset (list) An ecocomDP formatted dataset
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#' 
#' @examples
#' plot_taxa_accum_sites(
#'   observation = ants_L1[[1]]$tables$observation,
#'   id = names(ants_L1))
#' 
#' plot_taxa_accum_sites(
#'   dataset = ants_L1)
#'
plot_taxa_accum_sites <- function(
  dataset = NULL,
  observation = NULL, 
  id = NA_character_, 
  alpha = 1){
  
  

  if(is.na(id)) id <- names(dataset)
  if(is.null(observation)) observation <- dataset[[id]]$tables$observation
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)                    # intermediate format for plotting
  
  
  
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
  comm.dat <- ds$dslong %>% arrange(SITE_ID)                              # order by site
  no.taxa.space <- calc_cuml_taxa_space(comm.dat)
  no.taxa.space$no.site <- as.numeric(rownames(no.taxa.space))
  
  
  # Plot
  ggplot2::ggplot(data = no.taxa.space, ggplot2::aes(x = no.site, y = no.taxa)) + 
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Taxa accumulation by site accumulation", subtitle = ds$id) +
    ggplot2::xlab("Cumulative number of sites") +
    ggplot2::ylab(paste0("Cumulative number of taxa")) +
    ggplot2::theme_bw()
}








#' Plot taxa accumulation through time
#'
#' @param dataset (list) An ecocomDP formatted dataset
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' 
#' @export
#' 
#' @examples
#' plot_taxa_accum_time(
#'   observation = ants_L1[[1]]$tables$observation,
#'   id = names(ants_L1))
#' 
#' plot_taxa_accum_time(
#'   dataset = ants_L1)
#'   
plot_taxa_accum_time <- function(
  dataset = NULL,
  observation = NULL, 
  id = NA_character_, 
  alpha = 1){
  
  if(is.na(id)) id <- names(dataset)
  if(is.null(observation)) observation <- dataset[[id]]$tables$observation
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)                    # intermediate format for plotting
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
  cuml.taxa.all.sites <- cuml.taxa.fun(
    ex = ds$dslong %>% dplyr::arrange(DATE))   # taxa accumulation across all sites pooled together
  comm.dat <- ds$dslong %>% dplyr::arrange(SITE_ID)             # order by site
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
  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = cuml.taxa.by.site, 
      ggplot2::aes(x = year, y = no.taxa, color = SITE_ID), 
      alpha = alpha) +
    ggplot2::geom_line(
      data = cuml.taxa.by.site, 
      ggplot2::aes(x = year, y = no.taxa, color = SITE_ID), 
      alpha = alpha) +
    ggplot2::geom_point(
      data = cuml.taxa.all.sites, 
      ggplot2::aes(x = year, y = no.taxa, fill = "")) +
    ggplot2::geom_line(
      data = cuml.taxa.all.sites, 
      ggplot2::aes(x = year, y = no.taxa)) +
    ggplot2::labs(title = "Accumulation of taxa through time", subtitle = ds$id) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(paste0("Cumulative number of taxa")) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") + 
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = "Site", 
        label.theme = ggplot2::element_text(size = 6)),
      fill = ggplot2::guide_legend(title = "All sites")) +
    ggplot2::ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
    ggplot2::theme_bw()
}




#' Plot diversity (taxa richness) through time
#'
#' @param dataset (list) An ecocomDP formatted dataset
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' 
#' @export
#' 
#' @examples
#' observation <- ants_L1[[1]]$tables$observation
#' id <- names(ants_L1)
#' 
#' plot_taxa_richness(observation, id)
#' 
plot_taxa_richness <- function(
  dataset = NULL,
  observation = NULL, 
  id = NA_character_,   alpha = 1){
  
  if(is.na(id)) id <- names(dataset)
  if(is.null(observation)) observation <- dataset[[id]]$tables$observation
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)                    # intermediate format for plotting
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
  ntaxa <- calc_ntaxa(ds$dslong)
  # Plot
  ggplot2::ggplot() +
    ggplot2::geom_point(data = ntaxa$ntaxa, ggplot2::aes(x = DATE, y = ntaxa, group = SITE_ID, color = SITE_ID), alpha = alpha) +
    ggplot2::geom_line(data = ntaxa$ntaxa, ggplot2::aes(x = DATE, y = ntaxa, group = SITE_ID, color = SITE_ID), alpha = alpha) +
    ggplot2::geom_point(data = ntaxa$total_ntaxa, ggplot2::aes(x = DATE, y = ntaxa, fill = ""), color="black") +
    ggplot2::geom_line(data = ntaxa$total_ntaxa, ggplot2::aes(x = DATE, y = ntaxa, group = 1), color="black") +
    ggplot2::labs(title = "Richness through time", subtitle = ds$id) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(paste0("Richness")) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = "Site", label.theme = ggplot2::element_text(size = 6)),
      fill = ggplot2::guide_legend(title = "All sites")) +
    ggplot2::ylim(c(0, max(ntaxa$total_ntaxa$ntaxa))) +
    ggplot2::theme_bw()
}








#' Plot dates and times samples were taken
#'
#' @param dataset (list) An ecocomDP formatted dataset
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' 
#' @export
#' 
#' @examples
#' observation <- ants_L1[[1]]$tables$observation
#' id <- names(ants_L1)
#' 
#' plot_sample_space_time(observation, id)
#' 
plot_sample_space_time <- function(
  dataset = NULL,
  observation = NULL, 
  id = NA_character_, 
  alpha = 1){
  
  if(is.na(id)) id <- names(dataset)
  if(is.null(observation)) observation <- dataset[[id]]$tables$observation
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  
  # summarize data for plot
  obs_summary <- observation %>%
    # truncate time and just use dates
    dplyr::mutate(datetime = .data$datetime %>% lubridate::as_date() %>% lubridate::ymd()) %>%
    dplyr::select(.data$event_id, .data$location_id, .data$datetime) %>%
    dplyr::group_by(.data$location_id, .data$datetime) %>%
    dplyr::summarize(
      `Sampling\nevents` = .data$event_id %>% unique %>% length)
  
  
  # Scale font size
  uniy <- length(unique(obs_summary$location_id))
  if (uniy < 30) {
    txty <- NULL
  } else if (uniy < 60) {
    txty <- 6
  } else if (uniy >= 60) {
    txty <- 4
  }
  
 
  # Plot
  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = obs_summary, 
      ggplot2::aes(x = datetime, y = location_id, size = .data$`Sampling\nevents`), 
      alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sample times by location", subtitle = id) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Site") +
    ggplot2::scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      date_minor_breaks = "1 month"
      ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y.left = ggplot2::element_text(size = txty),
      plot.margin = ggplot2::margin(0.1, 0.25, 0.1, 0.1, "in"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
}








#' Plot number of unique taxa shared across sites
#' 
#' @param dataset (list) An ecocomDP formatted dataset
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' 
#' @export
#' 
#' @examples
#' observation <- ants_L1[[1]]$tables$observation
#' id <- names(ants_L1)
#' 
#' plot_taxa_shared_sites(observation, id)
#' 
plot_taxa_shared_sites <- function(
  dataset = NULL,
  observation = NULL, 
  id = NA_character_){
  
  if(is.na(id)) id <- names(dataset)
  if(is.null(observation)) observation <- dataset[[id]]$tables$observation
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)                  # intermediate format for plotting
  heat_pal_spectral <- grDevices::colorRampPalette(rev( RColorBrewer::brewer.pal(11, "Spectral")))
  # Count taxa shared between sites in a site by taxa matrix
  shared.species <- function(comm) {
    sites <- comm[, 1]
    share.mat <- matrix(NA, nrow = length(sites), ncol = length(sites), dimnames = list(sites, sites))
    site.pairs <- expand.grid(site1 = sites, site2 = sites)
    site.pairs$shared <- NA
    for(pair in 1:nrow(site.pairs)) {
      site1 <- comm[site.pairs$site1[pair], -1]
      site2 <- comm[site.pairs$site2[pair], -1]
      site.pairs$shared[pair] <- sum(site1 == 1 & site2 == 1, na.rm = TRUE)
    }
    return(site.pairs)
  }
  # aggregate years by cumulative abundances
  comm.cumul <- ds$dswide %>% 
    dplyr::group_by(SITE_ID) %>% 
    dplyr::select(-OBSERVATION_TYPE, -DATE) %>%
    dplyr::summarise_all(sum, na.rm = TRUE)
  # Convert sum abundance to presence absence
  vals <- comm.cumul %>% dplyr::select(-SITE_ID)
  vals[vals > 0] <- 1
  comm.cumul <- dplyr::bind_cols(comm.cumul[, 1], vals)
  shared.taxa <- shared.species(as.data.frame(comm.cumul))
  # scale font size
  uniy <- length(unique(shared.taxa$site1))
  if (uniy < 30) {
    txty <- NULL
  } else if (uniy < 60) {
    txty <- 6
  } else if (uniy >= 60) {
    txty <- 4
  }
  # Plot
  p <- ggplot2::ggplot(shared.taxa, ggplot2::aes(x = site1, y = site2, fill = shared)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colours = heat_pal_spectral(100), name = paste0("Taxa shared")) + 
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Number of taxa shared across sites", subtitle = ds$id) +
    ggplot2::xlab("Site") +
    ggplot2::ylab("Site") +
    ggplot2::theme(
      aspect.ratio = 1, 
      axis.text.x.bottom = ggplot2::element_text(size = txty, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y.left = ggplot2::element_text(size = txty))
  if (is.null(txty)) {
    p <- p + ggplot2::annotate("text", x = shared.taxa$site1, y = shared.taxa$site2, label = shared.taxa$shared)
  } else {
    p <- p + ggplot2::annotate("text", x = shared.taxa$site1, y = shared.taxa$site2, label = shared.taxa$shared, size = txty)
  }
  p
}








# Format dataset for community plotting functions
#
# @param observation (tbl_df, tbl, data.frame) The observation table of ecocomDP
# @param id (character) Identifier of dataset to be used in plot subtitles.
# 
# @details Downstream plotting functions are based on \href{https://github.com/sokole/ltermetacommunities/tree/master/Group2-explore-data}{LTER Metacommunities code} and use their intermediate data input format.
#
# @return (tbl_df, tbl, data.frame) Tabular data of \code{id} in a format compatible with plotting functions
# 
format_for_comm_plots <- function(observation, id) {
  id <- id
  # Constraints
  varname <- unique(observation[c("variable_name", "unit")])                 # Can only handle one variable
  if (nrow(varname) > 1) {
    warning("Input 'observation' has > 1 unique variable_name and unit ",
            "combination. Only the first will be used (", 
            varname$variable_name, ", ", varname$unit, ").", call. = FALSE)
    if (is.na(varname$unit[1])) {
      observation <- dplyr::filter(observation, 
                                   variable_name == varname$variable_name[1] & 
                                     is.na(unit))
    } else {
      observation <- dplyr::filter(observation, 
                                   variable_name == varname$variable_name[1] & 
                                     unit == varname$unit[1])
    }
  }
  
  # Methods used by this processing don't require aggregation of non-unique observations, so drop duplicates without warning
  dups <- observation %>% dplyr::select(-observation_id, -value, -event_id) %>% duplicated()
  if (any(dups)) {
    observation <- observation[!dups, ]
  }
  
  # Use dates
  if (is.character(observation$datetime)) {
    warning("Input datetimes are character strings. Accuracy may be improved",
            " if inputs are parsed before calling plot_data().", call. = FALSE)
  }
  observation$datetime <- lubridate::date(observation$datetime)
  
  obs <- observation
  taxon_count <- data.frame(
    OBSERVATION_TYPE = "TAXON_COUNT",
    SITE_ID = obs$location_id,
    DATE = obs$datetime,
    VARIABLE_NAME = obs$taxon_id,
    VARIABLE_UNITS = obs$variable_name,
    VALUE = obs$value,
    stringsAsFactors = FALSE)
  
  dslong <- taxon_count %>%                                   # long form
    dplyr::mutate_at(dplyr::vars(c(VALUE)), as.numeric) %>%
    dplyr::mutate_at(dplyr::vars(SITE_ID), as.character)
  dswide <- dslong %>%                                        # wide form
    dplyr::select(-VARIABLE_UNITS) %>%
    tidyr::pivot_wider(names_from = VARIABLE_NAME, values_from = VALUE)
  res <- list(
    id = id,
    dslong = dslong,
    dswide = dswide)
  return(res)
}








#' Plot taxa ranks
#'
#' @param dataset (list) An ecocomDP formatted dataset
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param taxon (tbl_df, tbl, data.frame) The taxon table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' 
#' @export
#' 
#' @examples
#' observation <- ants_L1[[1]]$tables$observation
#' taxon <- ants_L1[[1]]$tables$taxon
#' id <- names(ants_L1)
#' 
#' plot_taxa_rank(observation, taxon, id)
#' 
plot_taxa_rank <- function(
  dataset = NULL,
  facet_var = NA_character_, #e.g., "location_id", "taxon_id" must be a column name in observation or taxon table
  taxon = NULL, 
  observation = NULL, 
  id = NA_character_, 
  alpha = 1){
  
  if(is.na(id)) id <- names(dataset)
  if(is.null(observation)) observation <- dataset[[id]]$tables$observation
  if(is.null(taxon)) taxon <- dataset[[id]]$tables$taxon
  
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  # ds <- format_for_comm_plots(observation, id)                    # intermediate format for plotting   # not sure if this is needed/compatible for plots using taxon table
  

  data_long <- observation %>%
    left_join(taxon, by = "taxon_id")
  

  if(is.na(facet_var)){
    data_long %>%
      ggplot2::ggplot(
        ggplot2::aes(taxon_rank)) + 
      ggplot2::labs(title = "Taxa rank frequencies in the observation table", subtitle = id) +
      ggplot2::xlab("Taxon rank") +
      ggplot2::ylab(paste0("Number of occurrences")) +
      ggplot2::geom_bar() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust=1))
  }else{
    data_long %>%
      ggplot2::ggplot(
        ggplot2::aes(taxon_rank)) + 
      ggplot2::labs(title = "Taxa rank frequencies in the observation table", subtitle = id) +
      ggplot2::xlab("Taxon rank") +
      ggplot2::ylab(paste0("Number of occurrences")) +
      ggplot2::geom_bar() +
      facet_wrap(~as.factor(.data[[facet_var]])) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust=1))
  }

}










#' Plot stacked taxa by site
#'
#' @param dataset (list) An ecocomDP formatted dataset
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param rank (string) The rank of taxa to plot. Defaults to NA.
#' @param cutoff (numeric) Defaults to 0.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device.
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' 
#' @export
#' 
#' @examples
#' tables <- ants_L1[[1]]$tables
#' id <- names(ants_L1)
#' rank <- "species"
#' 
#' plot_stacked_taxa_by_site(tables, id, rank)
#' 
plot_taxa_occurrence_frequency <- function(
  dataset = NULL, 
  cutoff = 0, 
  rank = NA_character_,
  color_var = NA_character_, #e.g., "location_id"
  facet_var = NA_character_, #e.g., "location_id"
  alpha = 1){
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  flat <- flatten_data(dataset[[1]]$tables) 
  flat$unit %>% unique()
  flat %>%
    group_by(event_id, taxon_id) %>%
    summarize(n_obs = length(event_id)) %>%
    dplyr::filter(n_obs > 1)
  summed <- flat %>%
    group_by(event_id, taxon_id) %>%
    summarize(value = sum(value, na.rm = FALSE))
  cleaned <- flat %>%
    dplyr::select(
      event_id, location_id, datetime,
      taxon_id, taxon_rank, taxon_name) %>%
    distinct() %>%
    right_join(summed)
  
  ifelse(!is.na(rank),
         by_rank <- cleaned %>% filter(taxon_rank==rank) %>%
           group_by(event_id, location_id, datetime, taxon_id, taxon_rank, taxon_name, value) %>%
           summarize(counts = sum(value, na.rm=TRUE)),
         by_rank <- cleaned %>% group_by(event_id, location_id, datetime, taxon_id, taxon_rank, taxon_name, value) %>%
           summarize(counts = sum(value, na.rm=TRUE)))
  ifelse(!is.na(rank),
         title <- paste("Taxa frequencies by site: cutoff =", cutoff, ", taxon rank =", rank),
         title <- paste("Taxa frequencies by site: cutoff =", cutoff))
  
  by_rank %>%
    group_by(taxon_name, location_id) %>%
    summarize(
      occurrence = (counts > 0) %>% sum()) %>%
    filter(occurrence > cutoff) %>%               # create if/else statement for cutoff
    ggplot2::ggplot(aes(
      x = reorder(taxon_name, -occurrence),
      y = occurrence,
      color = location_id,
      fill = location_id)) +
    ggplot2::labs(title = title, subtitle = id) +
    ggplot2::xlab("Taxon name") +
    ggplot2::ylab(paste0("No. occurrences")) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}






#' Faceted densities plot
#'
#' @param tables (list of tbl_df, tbl, data.frame) A named list of ecocomDP tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param rank (string) The rank of taxa to plot. Defaults to NA
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' 
#' @export
#' 
#' @examples
#' tables <- ants_L1[[1]]$tables
#' id <- names(ants_L1)
#' rank <- "species"
#' 
#' plot_stacked_taxa_by_site(tables, id, rank)
#' 

#Faceted densities plot, shows averages of each taxa.
plot_faceted_densities <- function(tables, id, rank=NA, alpha=1) {
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(tables$observation, id)                    # intermediate format for plotting
  
  flat <- flatten_data(tables) 
  flat$unit %>% unique()
  flat %>%
    group_by(event_id, taxon_id) %>%
    summarize(n_obs = length(event_id)) %>%
    dplyr::filter(n_obs > 1)
  summed <- flat %>%
    group_by(event_id, taxon_id) %>%
    summarize(value = sum(value, na.rm = FALSE))
  cleaned <- flat %>%
    dplyr::select(
      event_id, location_id, datetime,
      taxon_id, taxon_rank, taxon_name) %>%
    distinct() %>%
    right_join(summed)
  
  ifelse(!is.na(rank),
         by_rank <- cleaned %>% filter(taxon_rank==rank) %>%
           group_by(event_id, location_id, datetime, taxon_id, taxon_rank, taxon_name, value) %>%
           summarize(counts = sum(value, na.rm=TRUE)),
         by_rank <- cleaned %>% group_by(event_id, location_id, datetime, taxon_id, taxon_rank, taxon_name, value) %>%
           summarize(counts = sum(value, na.rm=TRUE)))
  ifelse(!is.na(rank),
         title <- paste("Faceted densities: taxon rank =",rank),
         title <- "Faceted densities")
  
  by_rank %>%
    ggplot2::ggplot(aes(
      x = reorder(taxon_name, -counts),
      y = log10(counts),
      color = location_id,
      fill = location_id)) +
    ggplot2::labs(title = title, subtitle = ds$id) +
    ggplot2::xlab("Taxa name") +
    ggplot2::ylab(paste0("log10(counts)")) +
    geom_boxplot(alpha = 0.5) + # alpha = transparency
    facet_grid(location_id ~ .) +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
}


#' Plot sites on US map
#'
#' @param dataset (list) An ecocomDP formatted dataset
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' @param labels (boolean) Argument to show labels of each US state. Default is TRUE.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import usmap
#' @import ggrepel
#' 
#' @export
#' 
#' @examples
#' flat_table <- flatten_data(ants_L1[[1]]$tables)
#' id <- names(ants_L1)
#' 
#' plot_sites(flat_table, id)
#' 
plot_sites <- function(
  dataset = NULL,
  flat_table = NULL,
  id = NA_character_,
  observation = NULL,
  location = NULL,
  alpha = 1,
  labels = TRUE){
  
  if(is.na(id) && !is.null(dataset)) id <- names(dataset)
  
  if(is.null(flat_table)){
    # default to tables that are provided, otherwise, use what's 
    # available in the dataset. 
    if(is.null(observation)) observation <- dataset[[id]]$tables$observation
    if(is.null(location)) location <- dataset[[id]]$tables$location
    
    validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
    
    if(!grepl("(?i)neon",id)){
      #non-neon locations need flattening
      flat_location <- (ecocomDP::flatten_location(location))$location_flat
    }else if(grepl("(?i)neon",id)){
      flat_location <- location
    }
    
    # join tables
    # thils filters out locations that are not in the observation table
    flat_table <- observation %>%
      dplyr::left_join(flat_location, by = "location_id")
    # flat_table <- ecocomDP::flatten_data(dataset[[1]]$tables)
  }
  
  
  cleaned <- flat_table %>%
    dplyr::select(
      .data$longitude, 
      .data$latitude, 
      .data$location_name,
      .data$package_id) %>%
    dplyr::distinct()
  
  # library(usmap)
  # library(ggrepel)
  
  transformed_cleaned <- usmap::usmap_transform(cleaned)
  
  usmap::plot_usmap(color = "grey") + 
    ggplot2::geom_point(
      data = transformed_cleaned,
      ggplot2::aes(x = longitude.1, y = latitude.1, size = 20),
      color = "red", alpha = alpha) +
    ggrepel::geom_text_repel(
      data = transformed_cleaned,
      aes(x = longitude.1, y = latitude.1, label = location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::labs(title = "Site Locations on US Map"
                  #, subtitle = ds$id
    ) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab(paste0("Latitude")) +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "none")
  
}
