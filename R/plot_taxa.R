#' Plot taxa accumulation by site accumulation
#'
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
#' plot_taxa_accum_sites(observation, id)
#' 
plot_taxa_accum_sites <- function(observation, id, alpha = 1) {
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
#' plot_taxa_accum_time(observation, id)
#' 
plot_taxa_accum_time <- function(observation, id, alpha = 1) {
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
#' plot_taxa_diversity(observation, id)
#' 
plot_taxa_diversity <- function(observation, id, alpha = 1) {
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
    ggplot2::labs(title = "Diversity through time", subtitle = ds$id) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(paste0("Number of taxa observed")) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = "Site", label.theme = ggplot2::element_text(size = 6)),
      fill = ggplot2::guide_legend(title = "All sites")) +
    ggplot2::ylim(c(0, max(ntaxa$total_ntaxa$ntaxa))) +
    ggplot2::theme_bw()
}








#' Plot dates and times samples were taken
#'
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
#' plot_taxa_sample_time(observation, id)
#' 
plot_taxa_sample_time <- function(observation, id, alpha = 1) {
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)                    # intermediate format for plotting
  # Scale font size
  uniy <- length(unique(ds$dslong$SITE_ID))
  if (uniy < 30) {
    txty <- NULL
  } else if (uniy < 60) {
    txty <- 6
  } else if (uniy >= 60) {
    txty <- 4
  }
  # Plot
  ggplot2::ggplot() +
    ggplot2::geom_point(data = ds$dslong, ggplot2::aes(x = DATE, y = SITE_ID), alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sample times", subtitle = ds$id) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Site") +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y.left = ggplot2::element_text(size = txty),
      plot.margin = ggplot2::margin(0.1, 0.25, 0.1, 0.1, "in"))
}








#' Plot number of unique taxa shared across sites
#' 
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
plot_taxa_shared_sites <- function(observation, id) {
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
