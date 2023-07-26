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
    tidyr::pivot_wider(
      names_from = VARIABLE_NAME,
      values_from = VALUE,
      values_fn = mean) #average across non-unique observations.
  res <- list(
    id = id,
    dslong = dslong,
    dswide = dswide)
  return(res)
}








#' Plot taxa accumulation by site accumulation
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' @param observation (tbl_df, tbl, data.frame) DEPRECATED: Use \code{data} instead.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 9 columns of the observation table.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a dataset of interest
#' dataset <- read_data("edi.193.5")
#'
#' # Plot the dataset
#' plot_taxa_accum_sites(dataset)
#'
#' # Flatten the dataset, manipulate, then plot
#' dataset %>%
#'   flatten_data() %>%
#'   dplyr::filter(lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_accum_sites()
#'
#' # Plot from the observation table directly
#' plot_taxa_accum_sites(dataset$tables$observation)
#' }
#'
#' # Plot the example dataset
#' plot_taxa_accum_sites(ants_L1)
#'
plot_taxa_accum_sites <- function(data,
                                  id = NA_character_,
                                  alpha = 1,
                                  observation = NULL) {
  # Warn about deprecated observation parameter (remove after 2022-10-18)
  if (!is.null(observation)) {
    data <- observation
    warning('Input argument "observation" is deprecated, use "data" instead',
            call. = FALSE)
    
    # get data package id from dataset object
    if (is.na(id)) {
      id <- get_id(data)
    }
  }
  
  # Get observation table and id from data input if possible
  observation <- get_observation_table(data)
  
  # if id is still missing, get it from observation table
  if (is.na(id)){
    id <- paste(unique(observation$package_id), collapse = "|")
  }
  
  
  # Validate inputs and construct intermediate format for plotting
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)
  # Calculate cumulative number of taxa
  calc_cuml_taxa_space <- function(ex) {
    taxa.s.list <- list()
    sites <- unique(ex$SITE_ID)
    for(t in 1:length(unique(ex$SITE_ID))){ # unique taxa found in each year
      tmp.dat <- subset(ex, ex$SITE_ID == sites[t])
      tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0)
      taxa.s.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
    }
    cuml.taxa.space <- list() # cumulative list of taxa over space
    cuml.taxa.space[[1]] <- taxa.s.list[[1]]
    if (length(unique(ex$SITE_ID)) > 1) {
      for(t in 2:length(unique(ex$SITE_ID))){ # list cumulative taxa, with duplicates
        cuml.taxa.space[[t]] <- c(cuml.taxa.space[[t - 1]], taxa.s.list[[t]])
      }
    }
    cuml.taxa.space <- lapply(cuml.taxa.space, function(x) {unique(x)}) # rm duplicates
    cuml.no.taxa.space <- data.frame("site" = unique(ex$SITE_ID)) # total unique taxa over space
    cuml.no.taxa.space$no.taxa <- unlist(lapply(cuml.taxa.space, function(x) {length(x)}))
    return(cuml.no.taxa.space)
  }
  comm.dat <- ds$dslong %>% arrange(SITE_ID) # order by site
  no.taxa.space <- calc_cuml_taxa_space(comm.dat)
  no.taxa.space$no.site <- as.numeric(rownames(no.taxa.space))
  
  # Plot
  p <- ggplot2::ggplot(data = no.taxa.space, ggplot2::aes(x = no.site, y = no.taxa)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Taxa accumulation by site accumulation", subtitle = ds$id) +
    ggplot2::xlab("Cumulative number of sites") +
    ggplot2::ylab(paste0("Cumulative number of taxa")) +
    ggplot2::theme_bw()
  
  return(p)
}








#' Plot taxa accumulation through time
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' @param observation (tbl_df, tbl, data.frame) DEPRECATED: Use \code{data} instead.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 9 columns of the observation table.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a dataset of interest
#' dataset <- read_data("edi.193.5")
#'
#' # Plot the dataset
#' plot_taxa_accum_time(dataset)
#'
#' # Flatten the dataset, manipulate, then plot
#' dataset %>%
#'   flatten_data() %>%
#'   dplyr::filter(lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_accum_time()
#'
#' # Plot from the observation table directly
#' plot_taxa_accum_time(dataset$tables$observation)
#' }
#'
#' # Plot the example dataset
#' plot_taxa_accum_time(ants_L1)
#'
plot_taxa_accum_time <- function(data,
                                 id = NA_character_,
                                 alpha = 1,
                                 observation = NULL){
  
  # Warn about deprecated observation parameter (remove after 2022-10-18)
  if (!is.null(observation)) {
    data <- observation
    warning('Input argument "observation" is deprecated, use "data" instead',
            call. = FALSE)
    
    # get data package id from dataset object
    if (is.na(id)) {
      id <- get_id(data)
    }
  }
  
  # Get observation table and id from data input if possible
  observation <- get_observation_table(data)
  
  # if id is still missing, get it from observation table
  if (is.na(id)){
    id <- paste(unique(observation$package_id), collapse = "|")
  }
  
  # Validate inputs and construct intermediate format for plotting
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)
  # Calculate cumulative number of taxa
  cuml.taxa.fun <- function(ex){
    taxa.t.list <- list()
    dates <- unique(ex$DATE)
    for(t in 1:length(unique(ex$DATE))) { # unique taxa found in each year
      tmp.dat <- subset(ex, ex$DATE == dates[t])
      tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0)
      taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
    }
    cuml.taxa <- list() # cumulative taxa through time
    cuml.taxa[[1]] <- taxa.t.list[[1]]
    if (length(unique(ex$DATE)) > 1) { # create list of the cumulative taxa, with duplicates
      for(t in 2:length(unique(ex$DATE))){
        cuml.taxa[[t]] <- c(cuml.taxa[[t - 1]], taxa.t.list[[t]])
      }
    }
    cuml.taxa <- lapply(cuml.taxa, function(x){unique(x)}) # rm duplicates
    cuml.no.taxa <- data.frame("year" = unique(ex$DATE), stringsAsFactors = FALSE) # number of unique taxa through time
    cuml.no.taxa$no.taxa <- unlist(lapply(cuml.taxa, function(x){length(x)}))
    return(cuml.no.taxa)
  }
  cuml.taxa.all.sites <- cuml.taxa.fun(
    ex = ds$dslong %>% dplyr::arrange(DATE))   # taxa accumulation across all sites pooled together
  # Plot
  p <- ggplot2::ggplot() +
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
    ggplot2::guides(fill = "none") +
    ggplot2::ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
    ggplot2::theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  return(p)
}








#' Plot diversity (taxa richness) through time
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param time_window_size (character) Define the time window over which to aggregate observations for calculating richness. Can be: "day" or "year"
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' @param observation (tbl_df, tbl, data.frame) DEPRECATED: Use \code{data} instead.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 9 columns of the observation table.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a dataset of interest
#' dataset <- read_data("edi.193.5")
#'
#' # Plot the dataset
#' plot_taxa_diversity(dataset)
#'
#' # Plot the dataset with observations aggregated by year
#' plot_taxa_diversity(dataset, time_window_size = "year")
#'
#' # Flatten the dataset, manipulate, then plot
#' dataset %>%
#'   flatten_data() %>%
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2007-01-01") %>%
#'   plot_taxa_diversity()
#'
#' # Plot from the observation table directly
#' plot_taxa_diversity(dataset$tables$observation)
#' }
#'
#' # Plot the example dataset
#' plot_taxa_diversity(ants_L1)
#'
plot_taxa_diversity <- function(data,
                                id = NA_character_,
                                time_window_size = "day",
                                observation = NULL,
                                alpha = 1){
  
  
  # Warn about deprecated observation parameter (remove after 2022-10-18)
  if (!is.null(observation)) {
    data <- observation
    warning('Input argument "observation" is deprecated, use "data" instead',
            call. = FALSE)
    
    # get data package id from dataset object
    if (is.na(id)) {
      id <- get_id(data)
    }
  }
  
  # Get observation table and id from data input if possible
  observation <- get_observation_table(data)
  
  # if id is still missing, get it from observation table
  if (is.na(id)){
    id <- paste(unique(observation$package_id), collapse = "|")
  }
  
  # Validate inputs and construct intermediate format for plotting
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  
  
  # Richness by location id
  richness_by_location <- observation %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(
      .data$location_id,
      .data$datetime,
      .data$taxon_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      datetime = dplyr::case_when(
        grepl("^(?i)d", time_window_size) ~ .data$datetime %>%
          lubridate::as_date() %>%
          as.character(),
        grepl("^(?i)m", time_window_size) ~ paste0(
          .data$datetime %>% lubridate::as_date() %>% lubridate::year(),"-",
          .data$datetime %>% lubridate::as_date() %>% lubridate::month(),"-01"),
        grepl("^(?i)y", time_window_size) ~ .data$datetime %>%
          lubridate::as_date() %>%
          lubridate::year() %>%
          as.character())) %>%
    dplyr::group_by(.data$location_id, .data$datetime) %>%
    dplyr::summarize(
      ntaxa = length(unique(taxon_id)))
  # Total richness
  total_richness <- observation %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(
      .data$datetime,
      .data$taxon_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      datetime = dplyr::case_when(
        grepl("^(?i)d", time_window_size) ~ .data$datetime %>%
          lubridate::as_date() %>%
          as.character(),
        grepl("^(?i)m", time_window_size) ~ paste0(
          .data$datetime %>% lubridate::as_date() %>% lubridate::year(),"-",
          .data$datetime %>% lubridate::as_date() %>% lubridate::month(),"-01"),
        grepl("^(?i)y", time_window_size) ~ .data$datetime %>%
          lubridate::as_date() %>%
          lubridate::year() %>%
          as.character())) %>%
    dplyr::group_by(.data$datetime) %>%
    dplyr::summarize(
      ntaxa = length(unique(taxon_id))) %>%
    dplyr::arrange(.data$datetime) %>%
    dplyr::mutate(
      location_id = "All sites")
  # Format datetime properly based on window size
  if(!grepl("^(?i)y", time_window_size)){
    richness_by_location$datetime <- richness_by_location$datetime %>%
      lubridate::as_date()
    total_richness$datetime <- total_richness$datetime %>%
      lubridate::as_date()
  }else if(grepl("^(?i)y", time_window_size)){
    richness_by_location <- richness_by_location %>%
      dplyr::mutate(
        datetime = paste0(.data$datetime, "-01-01") %>%
          lubridate::as_date())
    total_richness <- total_richness %>%
      dplyr::mutate(
        datetime = paste0(.data$datetime, "-01-01") %>%
          lubridate::as_date())
  }
  # Plot
  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = richness_by_location,
      ggplot2::aes(x = .data$datetime,
                   y = .data$ntaxa,
                   group = as.factor(.data$location_id),
                   color = as.factor(.data$location_id)),
      alpha = alpha) +
    ggplot2::geom_line(
      data = richness_by_location,
      ggplot2::aes(x = .data$datetime,
                   y = .data$ntaxa,
                   group = as.factor(.data$location_id),
                   color = as.factor(.data$location_id)),
      alpha = alpha) +
    ggplot2::geom_point(
      data = total_richness,
      ggplot2::aes(x = .data$datetime,
                   y = .data$ntaxa,
                   fill = ""),
      color = "black") +
    ggplot2::geom_line(
      data = total_richness,
      ggplot2::aes(x = .data$datetime,
                   y = .data$ntaxa,
                   group = 1),
      color = "black") +
    ggplot2::labs(title = "Richness through time", subtitle = id) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(paste0("Richness")) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = "Site",
        label.theme = ggplot2::element_text(size = 6)),
      fill = ggplot2::guide_legend(title = "All sites")) +
    ggplot2::ylim(c(0, max(total_richness$ntaxa))) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(
      date_labels = "%Y",
      date_breaks = "1 year",
      date_minor_breaks = "1 month") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}








#' Plot dates and times samples were collected or observations were made
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' @param color_var (character) Name of column to use to assign colors to the points on the plot
#' @param shape_var (character) Name of column to use to assign shapes to the points on the plot
#' @param observation (tbl_df, tbl, data.frame) DEPRECATED: Use \code{data} instead.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 9 columns of the observation table.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a dataset of interest
#' dataset <- read_data("edi.193.5")
#'
#' # Plot the dataset
#' plot_sample_space_time(dataset)
#'
#' # Flatten the dataset, manipulate, then plot
#' dataset %>%
#'   flatten_data() %>%
#'   dplyr::filter(lubridate::as_date(datetime) > "2003-07-01") %>%
#'   dplyr::filter(as.numeric(location_id) > 4) %>%
#'   plot_sample_space_time()
#' }
#'
#' # Plot the example dataset
#' plot_sample_space_time(ants_L1)
#'
plot_sample_space_time <- function(data,
                                   id = NA_character_,
                                   alpha = 1,
                                   color_var = "package_id",
                                   shape_var = "package_id",
                                   observation = NULL) {
  # Warn about deprecated observation parameter (remove after 2022-10-18)
  if (!is.null(observation)) {
    data <- observation
    warning('Input argument "observation" is deprecated, use "data" instead',
            call. = FALSE)
    
    # get data package id from dataset object
    if (is.na(id)) {
      id <- get_id(data)
    }
  }
  
  # # Get observation table and id from data input if possible
  # observation <- get_observation_table(data)
  
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "taxon_rank")
  
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  if(data_type == "table" && all(req_col_names %in% names(data))){
    data_long <- data %>% dplyr::distinct()
    # data_long <- data_long[,req_col_names]
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  }else if(data_type == "table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }else if(data_type == "dataset_old"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- names(data)
  }else if(data_type == "dataset"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }else{
    stop("No plotting method currently implemented for this data format")
  }
  
  
  # assing data_long to observation -- legacy of how old fxn was written
  observation <- data_long
  
  # Validate inputs
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  
  # if id is still missing, get it from observation table
  if (is.na(id)){
    id <- paste(unique(observation$package_id), collapse = "|")
  }
  
  
  
  # summarize data for plot
  obs_summary <- observation %>%
    # truncate time and just use dates
    dplyr::mutate(datetime = .data$datetime %>%
                    lubridate::as_date() %>%
                    lubridate::ymd()) %>%
    dplyr::select(
      all_of(c("event_id","location_id","datetime",
               color_var, shape_var))) %>%
    dplyr::group_by(
      across(
        all_of(unique(
        c("location_id","datetime", 
          color_var, shape_var))))) %>%
    dplyr::summarize(
      `Sampling\nevents` = .data$event_id %>% 
        unique %>% 
        length %>%
        as.integer)
  
  

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
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = obs_summary,
      ggplot2::aes(x = datetime, 
                   y = location_id, 
                   size = .data$`Sampling\nevents`,
                   color = .data[[color_var]], 
                   shape = .data[[shape_var]]),
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
  

  # do not plot legend for variables that only have one level
  if(length(unlist(unique(obs_summary[,color_var]))) == 1){
    p <- p + ggplot2::guides(color = "none")}
  if(length(unlist(unique(obs_summary[,shape_var]))) == 1){
    p <- p + ggplot2::guides(shape = "none")}
  if(length(unlist(unique(obs_summary[,"Sampling\nevents"]))) == 1){ 
    p <- p + ggplot2::guides(size = "none")}
  
  # return ggplot object
  return(p)
}








#' Plot dates and times samples were taken (DEPRECATED)
#'
#' @description This function has been deprecated. Use \code{plot_sample_space_time()} instead.
#'
#' @param observation (tbl_df, tbl, data.frame) The observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @export
#' @keywords internal
#'
plot_taxa_sample_time <- function(observation, id = NA_character_, alpha = 1) {
  .Deprecated("plot_sample_space_time")
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  plot_sample_space_time(data = observation, id = id, alpha = alpha)
}








#' Plot number of unique taxa shared across sites
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation table, or a flat table containing columns of the observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param observation (tbl_df, tbl, data.frame) DEPRECATED: Use \code{data} instead.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 9 columns of the observation table.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a dataset of interest
#' dataset <- read_data("edi.193.5")
#'
#' # Plot the dataset
#' plot_taxa_shared_sites(dataset)
#'
#' # Flatten the dataset, manipulate, then plot
#' dataset %>%
#'   flatten_data() %>%
#'   dplyr::filter(lubridate::as_date(datetime) > "2003-07-01") %>%
#'   dplyr::filter(as.numeric(location_id) > 4) %>%
#'   plot_taxa_shared_sites()
#'
#' # Plot from the observation table directly
#' plot_taxa_shared_sites(dataset$tables$observation)
#' }
#'
#' # Plot the example dataset
#' plot_taxa_shared_sites(ants_L1)
#'
plot_taxa_shared_sites <- function(data,
                                   id = NA_character_,
                                   observation = NULL){
  
  
  # Warn about deprecated observation parameter (remove after 2022-10-18)
  if (!is.null(observation)) {
    data <- observation
    warning('Input argument "observation" is deprecated, use "data" instead',
            call. = FALSE)
    
    # get data package id from dataset object
    if (is.na(id)) {
      id <- get_id(data)
    }
  }
  
  # Get observation table and id from data input if possible
  observation <- get_observation_table(data)
  
  # if id is still missing, get it from observation table
  if (is.na(id)){
    id <- paste(unique(observation$package_id), collapse = "|")
  }
  
  
  # Validate inputs and construct intermediate format for plotting
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  ds <- format_for_comm_plots(observation, id)
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
  
  return(p)
}








#' Plot taxa ranks
#'
#' @description Plot the number of observations that use each taxonomic rank in the dataset.
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation and taxon tables, or a flat table containing columns of the observation and taxon tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param facet_var (character) Name of column to use for faceting. Must be a column of the observation or taxon table.
#' @param facet_scales (character) Should scales be free ("free", default value), fixed ("fixed"), or free in one dimension ("free_x", "free_y")?
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 13 columns of the combined observation and taxon tables.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a dataset of interest
#' dataset <- read_data(
#'   id = "neon.ecocomdp.20120.001.001",
#'   site= c('COMO','LECO'),
#'   startdate = "2017-06",
#'   enddate = "2019-09",
#'   check.size = FALSE)
#'
#' # Plot the dataset
#' plot_taxa_rank(dataset)
#'
#' # Plot with facet by location
#' plot_taxa_rank(dataset, facet_var = "location_id")
#'
#' # Flatten the dataset, manipulate, then plot
#' dataset %>%
#'   flatten_data() %>%
#'   dplyr::filter(lubridate::as_date(datetime) > "2003-07-01") %>%
#'   dplyr::filter(grepl("COMO",location_id)) %>%
#'   plot_taxa_rank()
#' }
#'
#' # Plot the example dataset
#' plot_taxa_rank(ants_L1)
#'
plot_taxa_rank <- function(data,
                           id = NA_character_,
                           facet_var = NA_character_,
                           facet_scales = "free_x",
                           alpha = 1) {
  
  # TODO: Convert this unreadable block of code to a function like get_observation_table()
  # TODO: Call get_id()
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "taxon_rank")
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  if(data_type == "table" && all(req_col_names %in% names(data))){
    data_long <- data %>% dplyr::distinct()
    data_long <- data_long[,req_col_names]
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  }else if(data_type == "table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }else if(data_type == "dataset_old"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- names(data)
  }else if(data_type == "dataset"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }else{
    stop("No plotting method currently implemented for this data format")
  }
  
  # Validate inputs
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  # ordered taxon ranks
  rank_list_ordered <- c(
    "kingdom", "subkingdom", "infrakingdom",
    "superphylum", "phylum", "subphylum", "infraphylum",
    "superdivision", "division", "subdivision", "infradivision", "parvdivision", 
    "superclass", "class", "subclass", "infraclass", 
    "superorder", "order", "suborder", "infraorder", 
    "section", "subsection", 
    "superfamily", "family", "subfamily", 
    "tribe", "subtribe", 
    "genus", "subgenus", "species", "subspecies", 
    "variety", "subvariety", "form", "subform", 
    "stirp", "morph", "aberration", "race") %>% 
    rev()
  
  # Plot
  p <- data_long %>%
    dplyr::mutate(
      taxon_rank = tolower(taxon_rank) %>%
        factor(levels = rank_list_ordered, ordered = TRUE)) %>%
    ggplot2::ggplot(
      ggplot2::aes(taxon_rank)) +
    ggplot2::labs(
      title = "Taxa rank frequencies in the observation table", 
      subtitle = id) +
    ggplot2::xlab("Taxon rank") +
    ggplot2::ylab(paste0("Number of observations")) +
    ggplot2::geom_bar() +
    ggplot2::theme_bw() +
    ggplot2::coord_flip()
  if(!is.na(facet_var)){
    p <- p +
      ggplot2::facet_wrap(
        ~ as.factor(.data[[facet_var]]),
        scales = facet_scales)
  }
  return(p)
}








#' Plot taxon occurrence frequencies
#'
#' @description Plot taxon occurrence frequences as the number of 'event_id' by 'location_id' combinations in which a taxon is observed.
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation and taxon tables, or a flat table containing columns of the observation and taxon tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param min_occurrence (numeric) Minimum number of occurrences allowed for taxa included in the plot.
#' @param facet_var (character) Name of column to use for faceting. Must be a column of the observation or taxon table.
#' @param facet_scales (character) Should scales be free ("free", default value), fixed ("fixed"), or free in one dimension ("free_x", "free_y")?
#' @param color_var (character) Name of column to use for plot colors.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device.
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 13 columns of the combined observation and taxon tables.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a dataset of interest
#' dataset <- read_data("edi.193.5")
#'
#' # Plot the dataset
#' plot_taxa_occur_freq(dataset)
#'
#' # Facet by location and color by taxon_rank
#' plot_taxa_occur_freq(
#'   data = dataset,
#'   facet_var = "location_id",
#'   color_var = "taxon_rank")
#'
#' # Color by location and only include taxa with >= 5 occurrences
#' plot_taxa_occur_freq(
#'   data = dataset,
#'   color_var = "location_id",
#'   min_occurrence = 5)
#'
#' # Flatten, filter using a time cutoff, then plot
#' dataset %>%
#'   flatten_data() %>%
#'   dplyr::filter(lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_occur_freq()
#' }
#' # Plot the example dataset
#' plot_taxa_occur_freq(ants_L1)
#'
plot_taxa_occur_freq <- function(data,
                                 id = NA_character_,
                                 min_occurrence = 0,
                                 facet_var = NA_character_,
                                 color_var = NA_character_,
                                 facet_scales = "free",
                                 alpha = 1) {
  
  # TODO: Convert this unreadable block of code to a function like get_observation_table()
  # TODO: Call get_id()
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "taxon_name")
  
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  if(data_type == "table" && all(req_col_names %in% names(data))){
    data_long <- data %>% dplyr::distinct()
    # data_long <- data_long[,req_col_names]
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  }else if(data_type == "table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }else if(data_type == "dataset_old"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- names(data)
  }else if(data_type == "dataset"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }else{
    stop("No plotting method currently implemented for this data format")
  }
  
  
  
  # Validate inputs
  # TODO min_occurrence, color_var, facet_var
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  
  # plot title and subtitle text
  plot_title = "Taxa occurrence frequencies"
  plot_subtitle <- id
  if(!is.na(min_occurrence) && min_occurrence > 0) plot_subtitle <- paste0(plot_subtitle,
                                                                           "\ntaxa with >= ", min_occurrence, " occurrences")
  # calculate occurrences
  data_working <- data_long %>%
    dplyr::filter(.data$value > 0) %>%
    dplyr::mutate(occurrence = 1)
  
  col_select_list <- c("event_id","location_id","taxon_name","occurrence",
                       color_var, facet_var) %>%
    stats::na.omit() %>%
    unique()
  
  data_working <- data_working[,col_select_list] %>%
    dplyr::distinct()
  
  
  # detemine which taxa meet minimum occurrence threshold in dataset
  data_occurrence_total_filtered <- data_working %>%
    dplyr::group_by(.data$taxon_name) %>%
    dplyr::summarize(
      n_occurrences = length(.data$occurrence)) %>%
    dplyr::filter(.data$n_occurrences >= min_occurrence)
  
  
  # calculate occurrence by location_id and event_id for plotting
  data_occurrence_by_group <- data_working %>%
    dplyr::distinct() %>%
    dplyr::filter(
      .data$taxon_name %in% data_occurrence_total_filtered$taxon_name) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          unique(stats::na.omit(c("taxon_name", color_var, facet_var)))
        )
      )
    ) %>%
    dplyr::summarize(
      n_occurrences = length(.data$occurrence))
  
  
  data_occurrence <- data_occurrence_by_group
  
  # Scale font size
  uniy <- length(unique(data_occurrence$taxon_name))
  if (uniy < 30) {
    txty <- NULL
  } else if (uniy < 60) {
    txty <- 6
  } else if (uniy >= 60) {
    txty <- 4
  }
  # make initial plot object
  if(!is.na(color_var)){
    p <- data_occurrence %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = stats::reorder(.data$taxon_name, -.data$n_occurrences),
          y = .data$n_occurrences,
          color = .data[[color_var]],
          fill = .data[[color_var]]))
  }else{
    p <- data_occurrence %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = stats::reorder(.data$taxon_name, -.data$n_occurrences),
          y = .data$n_occurrences))
  }
  # add facets if provided
  if(!is.na(facet_var)){
    p <- p +
      ggplot2::facet_wrap(~ as.factor(.data[[facet_var]]),
                          scales = facet_scales)
  }
  # add title, labels and axis formatting
  p <- p +
    ggplot2::labs(title = plot_title, subtitle = plot_subtitle) +
    ggplot2::xlab("Taxon name") +
    ggplot2::ylab(paste0("Occurrences\n(unique 'event_id' by 'location_id' occurrences)")) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y.left = ggplot2::element_text(size = txty))
  # return plot
  return(p)
}








#' Plot mean taxa abundances per 'observation_id'
#'
#' @description Plot taxon abundances averaged across observation records for each taxon. Abundances are reported using the units provided in the dataset. In some cases, these counts are not standardized to sampling effort.
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation and taxon tables, or a flat table containing columns of the observation and taxon tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param min_relative_abundance (numeric) Minimum relative abundance allowed for taxa included in the plot; a value between 0 and 1, inclusive.
#' @param trans (character) Define the transform applied to the response variable; "identity" is default, "log1p" is x+1 transform. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".
#' @param facet_var (character) Name of column to use for faceting. Must be a column of the observation or taxon table.
#' @param facet_scales (character) Should scales be free ("free", default value), fixed ("fixed"), or free in one dimension ("free_x", "free_y")?
#' @param color_var (character) Name of column to use for plot colors.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 13 columns of the combined observation and taxon tables.
#'
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a dataset of interest
#' dataset <- read_data("edi.193.5")
#'
#' # plot ecocomDP formatted dataset
#' plot_taxa_abund(dataset)
#'
#' # plot flattened ecocomDP dataset, log(x+1) transform abundances
#' plot_taxa_abund(
#'   data = flatten_data(dataset),
#'   trans = "log1p")
#'
#' # facet by location color by taxon_rank, log 10 transform
#' plot_taxa_abund(
#'   data = dataset,
#'   facet_var = "location_id",
#'   color_var = "taxon_rank",
#'   trans = "log10")
#'
#' # facet by location, minimum rel. abund = 0.05, log 10 transform
#' plot_taxa_abund(
#'   data = dataset,
#'   facet_var = "location_id",
#'   min_relative_abundance = 0.05,
#'   trans = "log1p")
#'
#' # color by location, log 10 transform
#' plot_taxa_abund(
#'   data = dataset,
#'   color_var = "location_id",
#'   trans = "log10")
#'
#' # tidy syntax, flatten then filter data by date
#' dataset %>%
#'   flatten_data() %>%
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_abund(
#'     trans = "log1p",
#'     min_relative_abundance = 0.01)
#' }
#'
#' # Plot the example dataset
#' plot_taxa_abund(ants_L1)
#'
plot_taxa_abund <- function(data,
                            id = NA_character_,
                            min_relative_abundance = 0,
                            trans = "identity",
                            facet_var = NA_character_,
                            color_var = NA_character_,
                            facet_scales = "free",
                            alpha = 1) {
  # TODO: Convert this unreadable block of code to a function like get_observation_table()
  # TODO: Call get_id()
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "taxon_name")
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  if(data_type == "table" && all(req_col_names %in% names(data))){
    flat_data <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  }else if(data_type == "table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }else if(data_type == "dataset_old"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- names(data)
  }else if(data_type == "dataset"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }else{
    stop("No plotting method currently implemented for this data format")
  }
  # Validate inputs
  # TODO min_relative_abundance, color_var, facet_var
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  # filter based on min relative abund
  if(min_relative_abundance > 0){
    
    RA_data_filtered <- flat_data %>%
      dplyr::select(.data$taxon_id, .data$value, .data$unit) %>%
      dplyr::group_by(.data$taxon_id, .data$unit) %>%
      dplyr::summarize(
        sum_value = sum(.data$value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        total = sum(.data$sum_value),
        RA = .data$sum_value / .data$total) %>%
      dplyr::filter(.data$RA >= min_relative_abundance)
    
    flat_data <- flat_data %>%
      dplyr::filter(.data$taxon_id %in% RA_data_filtered$taxon_id)
  }
  
  # plot title and subtitle text
  plot_title = "Taxa abundances per 'observation_id'"
  plot_subtitle <- id
  if(!is.na(min_relative_abundance) && min_relative_abundance > 0) plot_subtitle <- paste0(plot_subtitle,
                                                                                           "\nrel. abundance >= ", min_relative_abundance)
  # Scale font size
  uniy <- length(unique(flat_data$taxon_name))
  if (uniy < 30) {
    txty <- NULL
  } else if (uniy < 60) {
    txty <- 6
  } else if (uniy >= 60) {
    txty <- 4
  }
  # make initial plot object
  if(!is.na(color_var)){
    p <- flat_data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = stats::reorder(.data$taxon_name, -.data$value),
          y = .data$value,
          color = .data[[color_var]],
          fill = .data[[color_var]]))
  }else{
    p <- flat_data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = stats::reorder(.data$taxon_name, -.data$value),
          y = .data$value))
  }
  # add facets if provided
  if(!is.na(facet_var)){
    if(length(unique(flat_data$unit)) > 1){
      p <- p + ggplot2::facet_wrap(~ as.factor(.data[[facet_var]]) +
                                     as.factor(.data$unit),
                                   scales = facet_scales)
    }else{
      p <- p + ggplot2::facet_wrap(~ as.factor(.data[[facet_var]]),
                                   scales = facet_scales)
    }
  }
  # add title, labels and axis formatting
  p <- p +
    ggplot2::labs(title = plot_title, subtitle = plot_subtitle) +
    ggplot2::xlab("Taxon name") +
    ggplot2::ylab(paste0("Abundance (",paste(unique(flat_data$unit), collapse = " or "), ")")) +
    geom_boxplot(alpha = 0.5) + # alpha = transparency
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::scale_y_continuous(trans = trans) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y.left = ggplot2::element_text(size = txty),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  return(p)
}




#' Plot sites on US map
#'
#' @param data (list or tbl_df, tbl, data.frame) The dataset object returned by \code{read_data()}, a named list of tables containing the observation and taxon tables, or a flat table containing columns of the observation and location tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles. Is automatically assigned when \code{data} is a dataset object containing the \code{id} field, or is a table containing the package_id column.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' @param color_var (character) Name of column to use to assign colors to the points on the plot
#' @param shape_var (character) Name of column to use to assign shapes to the points on the plot
#' @param labels (logical) Argument to show labels of each US state. Default is TRUE.
#'
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#'
#' @details The \code{data} parameter accepts a range of input types but ultimately requires the 14 columns of the combined observation and location tables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # Read a dataset of interest
#' dataset <- read_data("edi.193.5")
#' 
#' # Plot the dataset
#' plot_sites(dataset)
#' 
#' # Flatten dataset then plot
#' dataset %>%
#'  flatten_data() %>%
#'  plot_sites()
#' 
#' # Download a NEON dataset
#' dataset2 <- read_data(
#'  id = "neon.ecocomdp.20120.001.001",
#'  site= c('COMO','LECO'), 
#'  startdate = "2017-06",
#'  enddate = "2021-03",
#'  token = Sys.getenv("NEON_TOKEN"), # option to use a NEON token
#'  check.size = FALSE)
#' 
#' # Combine the two datasets and plot. This requires the datasets be first
#' # flattened and then stacked.
#' flattened_data1 <- dataset %>% flatten_data()
#' flattened_data2 <- dataset2 %>% flatten_data()
#' stacked_data <- bind_rows(flattened_data1,flattened_data2)
#' plot_sites(stacked_data)
#' }
#'
#' # Plot the example dataset
#' plot_sites(ants_L1)
#'
plot_sites <- function(
    data,
    id = NA_character_,
    alpha = 1,
    labels = TRUE,
    color_var = "package_id",
    shape_var = "package_id"){
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  # check for suggested packages that are required for this function to work
  suggs <- c("ggrepel", "usmap", "maps", "sf")
  suggsmissing <- !unlist(lapply(suggs, requireNamespace, quietly = TRUE))
  if (any(suggsmissing)) {
    stop("Packages ", paste(suggs, collapse = ", "), " are required for ",
         "running plot_sites(). Packages ",
         paste(suggs[suggsmissing], collapse = ", "), " are not installed.",
         call. = FALSE)
  }
  
  # TODO: Convert this unreadable block of code to a function like get_observation_table()
  # TODO: Call get_id()
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "location_id","location_name","longitude","latitude")
  
  # detect data type, extract observation table
  
  data_type <- detect_data_type(data)
  if(data_type == "table" && all(req_col_names %in% names(data))){
    flat_data <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  }else if(data_type == "table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
  }else if(data_type == "dataset_old"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- names(data)
  }else if(data_type == "dataset"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }else{
    stop("No plotting method currently implemented for this data format")
  }
  
  cleaned <- flat_data %>%
    dplyr::select(
      .data$longitude,
      .data$latitude,
      .data$location_name,
      .data$package_id
    ) %>%
    dplyr::distinct()
  
  cleaned$location_abbrv = substr(cleaned$location_name, 1, 4)
  
  world <- map_data("world")
  
  # Set legend position
  legend_position <- "right"
  if(length(unlist(unique(flat_data[,color_var]))) == 1 & 
     length(unlist(unique(flat_data[,shape_var]))) == 1){
    legend_position <- "none"
  }
  
  
  # make plot object
  p <- ggplot() +
    geom_polygon(data = world, aes(x = .data$long, y = .data$lat, group = .data$group), fill = "grey") +
    geom_point(data = flat_data, aes(x = .data$longitude, y = .data$latitude, 
                                     color = .data[[color_var]], 
                                     shape = .data[[shape_var]]),
               size = 3) +
    labs(x = "Longitude", y = "Latitude", color = color_var) +
    ggtitle("US Map with Coordinates") +
    theme_bw() +
    ggrepel::geom_text_repel(
      data = cleaned,
      aes(x = .data$longitude, 
          y = .data$latitude, 
          label = .data$location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = legend_position) +
    coord_cartesian(xlim = c(-165, -40), ylim = c(15, 75))
  
  return(p)
}