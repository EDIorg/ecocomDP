#' Plot taxa accumulation by site accumulation
#'
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {           # ggplot2 is a suggested package
    stop("Package 'ggplot2' is required but is not installed", call. = FALSE)
  }
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
  ggplot(data = no.taxa.space, aes(x = no.site, y = no.taxa)) + 
    geom_point() +
    geom_line() +
    labs(title = "Taxa accumulation by site accumulation", subtitle = ds$id) +
    xlab("Cumulative number of sites") +
    ylab(paste0("Cumulative number of taxa")) +
    theme_bw()
}








#' Plot taxa accumulation through time
#'
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {           # ggplot2 is a suggested package
    stop("Package 'ggplot2' is required but is not installed", call. = FALSE)
  }
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
  cuml.taxa.all.sites <- cuml.taxa.fun(ex = ds$dslong)   # taxa accumulation across all sites pooled together
  comm.dat <- ds$dslong %>% arrange(SITE_ID)             # order by site
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
    labs(title = "Accumulation of taxa through time", subtitle = ds$id) +
    xlab("Year") +
    ylab(paste0("Cumulative number of taxa")) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") + 
    guides(color = guide_legend(title = "Site", label.theme = element_text(size = 6)),
           fill = guide_legend(title = "All sites")) +
    ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
    theme_bw()
}








#' Plot diversity (taxa richness) through time
#'
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {           # ggplot2 is a suggested package
    stop("Package 'ggplot2' is required but is not installed", call. = FALSE)
  }
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
  ggplot() +
    geom_point(data = ntaxa$ntaxa, aes(x = DATE, y = ntaxa, group = SITE_ID, color = SITE_ID), alpha = alpha) +
    geom_line(data = ntaxa$ntaxa, aes(x = DATE, y = ntaxa, group = SITE_ID, color = SITE_ID), alpha = alpha) +
    geom_point(data = ntaxa$total_ntaxa, aes(x = DATE, y = ntaxa, fill = ""), color="black") +
    geom_line(data = ntaxa$total_ntaxa, aes(x = DATE, y = ntaxa, group = 1), color="black") +
    labs(title = "Diversity through time", subtitle = ds$id) +
    xlab("Year") +
    ylab(paste0("Number of taxa observed")) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") +
    guides(
      color = guide_legend(title = "Site", label.theme = element_text(size = 6)),
      fill = guide_legend(title = "All sites")) +
    ylim(c(0, max(ntaxa$total_ntaxa$ntaxa))) +
    theme_bw()
}








#' Plot dates and times samples were taken
#'
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {           # ggplot2 is a suggested package
    stop("Package 'ggplot2' is required but is not installed", call. = FALSE)
  }
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
  ggplot() +
    geom_point(data = ds$dslong, aes(x = DATE, y = SITE_ID), alpha = alpha) +
    theme_bw() +
    labs(title = "Sample times", subtitle = ds$id) +
    xlab("Year") +
    ylab("Site") +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", date_minor_breaks = "1 month") +
    theme_bw() +
    theme(
      axis.text.y.left = element_text(size = txty),
      plot.margin = margin(0.1, 0.25, 0.1, 0.1, "in"))
}








#' Plot number of unique taxa shared across sites
#' 
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
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
  if (!requireNamespace("ggplot2", quietly = TRUE)) {           # ggplot2 is a suggested package
    stop("Package 'ggplot2' is required but is not installed", call. = FALSE)
  }
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)                  # intermediate format for plotting
  heat_pal_spectral <- colorRampPalette(rev( RColorBrewer::brewer.pal(11, "Spectral")))
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
    group_by(SITE_ID) %>% 
    select(-OBSERVATION_TYPE, -DATE) %>%
    summarise_all(sum, na.rm = TRUE)
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
  p <- ggplot(shared.taxa, aes(x = site1, y = site2, fill = shared)) +
    geom_raster() +
    scale_fill_gradient(colours = heat_pal_spectral(100), name = paste0("Taxa shared")) +
    theme_bw() +
    labs(title = "Number of taxa shared across sites", subtitle = ds$id) +
    xlab("Site") +
    ylab("Site") +
    theme(
      aspect.ratio = 1, 
      axis.text.x.bottom = element_text(size = txty, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y.left = element_text(size = txty))
  if (is.null(txty)) {
    p <- p + annotate("text", x = shared.taxa$site1, y = shared.taxa$site2, label = shared.taxa$shared)
  } else {
    p <- p + annotate("text", x = shared.taxa$site1, y = shared.taxa$site2, label = shared.taxa$shared, size = txty)
  }
  p
}








#' Format dataset for community plotting functions
#'
#' @param observation (tbl_df, tbl, data.frame) The observation table of ecocomDP
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' 
#' @details Downstream plotting functions are based on \href{https://github.com/sokole/ltermetacommunities/tree/master/Group2-explore-data}{LTER Metacommunities code} and use their intermediate data input format.
#'
#' @return (tbl_df, tbl, data.frame) Tabular data of \code{id} in a format compatible with plotting functions
#' 
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
