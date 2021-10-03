#' Plot taxa accumulation by site accumulation
#'
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, an ecocomDP observation table, or any table containing the columns present in an ecocomDP observation table.
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
#' \dontrun{
#' # plot ecocomDP dataset
#' plot_taxa_accum_sites(ants_L1)
#' 
#' # plot flattened ecocomDP data
#' plot_taxa_accum_sites(flatten_data(ants_L1))
#' 
#' # plot an ecocomDP observation table
#' plot_taxa_accum_sites(
#'   data = ants_L1[[1]]$tables$observation)
#' 
#' # tidy syntax
#' ants_L1 %>% plot_taxa_accum_sites()
#' 
#' # tidy syntax, filter data by date
#' ants_L1 %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_accum_sites()
#' }
#' 
plot_taxa_accum_sites <- function(
  data,
  id = NA_character_,
  alpha = 1){
  
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value","unit")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    observation <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
  
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or table that includes columns from an ecocomDP 'observation' table")
  
  }else if(data_type == "dataset_old"){
    observation <- data[[1]]$tables$observation
    if(is.na(id)) id <- names(data)
  
  }else if(data_type == "dataset"){
    observation <- data$tables$observation
    if(is.na(id)) id <- data$id
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
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, an ecocomDP observation table, or any table containing the columns present in an ecocomDP observation table.
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
#' \dontrun{
#' # plot ecocomDP formatted dataset
#' plot_taxa_accum_time(ants_L1)
#' 
#' # plot flattened ecocomDP dataset
#' plot_taxa_accum_time(flatten_data(ants_L1))
#' 
#' # plot ecocomDP observation table
#' plot_taxa_accum_time(ants_L1[[1]]$tables$observation)
#' 
#' # tidy syntax, filter data by date
#' ants_L1 %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_accum_time()
#' }
#' 
plot_taxa_accum_time <- function(
  data,
  id = NA_character_, 
  alpha = 1){
  
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value","unit")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    observation <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
    
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or table that includes columns from an ecocomDP 'observation' table")
    
  }else if(data_type == "dataset_old"){
    observation <- data[[1]]$tables$observation
    if(is.na(id)) id <- names(data)
    
  }else if(data_type == "dataset"){
    observation <- data$tables$observation
    if(is.na(id)) id <- data$id
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
  cuml.taxa.all.sites <- cuml.taxa.fun(
    ex = ds$dslong %>% dplyr::arrange(DATE))   # taxa accumulation across all sites pooled together
  # comm.dat <- ds$dslong %>% dplyr::arrange(SITE_ID)             # order by site
  # X <- split(comm.dat, as.factor(comm.dat$SITE_ID))   # cumulative number of taxa for each site
  # out <- lapply(X, cuml.taxa.fun)
  # # out[names(out) %in% "lo_2_115"]                     # list to dataframe
  # output <- do.call("rbind", out)
  # output$rnames <- row.names(output)                  # create SITE_ID column
  # 
  # cuml.taxa.by.site <- suppressWarnings(              # Clean up the SITE_ID column. Warning sent when site_id only has one observation (non-issue), an artifact of do.call("rbind", out)
  #   output %>%
  #     dplyr::tbl_df() %>%
  #     dplyr::mutate(SITE_ID = rnames)
  #     # separate(rnames, c("SITE_ID", "todrop"), sep = "\\.") %>%
  #     # select(-todrop)
  #   )
  
  # Plot
  ggplot2::ggplot() +
    # ggplot2::geom_point(
    #   data = cuml.taxa.by.site, 
    #   ggplot2::aes(x = year, y = no.taxa, color = SITE_ID), 
    #   alpha = alpha) +
    # ggplot2::geom_line(
    #   data = cuml.taxa.by.site, 
    #   ggplot2::aes(x = year, y = no.taxa, color = SITE_ID), 
    #   alpha = alpha) +
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
    # ggplot2::guides(
    #   # color = ggplot2::guide_legend(
    #   #   title = "Site", 
    #   #   label.theme = ggplot2::element_text(size = 6)),
    #   fill = ggplot2::guide_legend(title = "All sites")) +
    ggplot2::guides(fill = "none") +
    ggplot2::ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
    ggplot2::theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
}




#' Plot diversity (taxa richness) through time
#'
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, an ecocomDP observation table, or any table containing the columns present in an ecocomDP observation table.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param time_window_size (character) Define the time window over which to aggregate observations for calculating richness
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
#' \dontrun{
#' # plot richness through time for ecocomDP formatted dataset by 
#' # observation date
#' plot_taxa_diversity(ants_L1)
#' 
#' # plot richness through time for ecocomDP formatted dataset by 
#' # aggregating observations within a year
#' plot_taxa_diversity(
#'   data = ants_L1,
#'   time_window_size = "year")
#' 
#' # plot richness through time for ecocomDP observation table
#' plot_taxa_diversity(ants_L1[[1]]$tables$observation)
#' 
#' # plot richness through time for flattened ecocomDP dataset 
#' plot_taxa_diversity(flatten_data(ants_L1))
#' 
#' 
#' # Using Tidy syntax:
#' # plot ecocomDP formatted dataset richness through time by 
#' # observation date
#' ants_L1 %>% plot_taxa_diversity()
#' 
#' ants_L1 %>% plot_taxa_diversity(time_window_size = "day")
#' 
#' # plot ecocomDP formatted dataset richness through time 
#' # aggregating observations within a month
#' ants_L1 %>% plot_taxa_diversity(time_window_size = "month")
#' 
#' # plot ecocomDP formatted dataset richness through time 
#' # aggregating observations within a year
#' ants_L1 %>% plot_taxa_diversity(time_window_size = "year")
#' 
#' # tidy syntax, filter data by date
#' ants_L1 %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2007-01-01") %>%
#'   plot_taxa_diversity(
#'     time_window_size = "year")
#' }
#' 
plot_taxa_diversity <- function(
  data,
  id = NA_character_,
  time_window_size = "day", #day or year
  alpha = 1){
  
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value","unit")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    observation <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
    
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or table that includes columns from an ecocomDP 'observation' table")
    
  }else if(data_type == "dataset_old"){
    observation <- data[[1]]$tables$observation
    if(is.na(id)) id <- names(data)
    
  }else if(data_type == "dataset"){
    observation <- data$tables$observation
    if(is.na(id)) id <- data$id
  }
  
  
  
  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  

  # richness by location id
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
  
  # total richness
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
  
  # format datetime properly based on window size
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
   
  # plot 
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
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, an ecocomDP observation table, or any table containing the columns present in an ecocomDP observation table.
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
#' \dontrun{
#' # plot ecocomDP formatted dataset
#' plot_sample_space_time(ants_L1)
#' 
#' # plot flattened ecocomDP dataset
#' plot_sample_space_time(flatten_data(ants_L1))
#' 
#' # plot ecocomDP observation table
#' plot_sample_space_time(ants_L1[[1]]$tables$observation)
#' 
#' # tidy syntax, filter data by date
#' ants_L1 %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_sample_space_time()
#' 
#' # tidy syntax, filter data by site ID
#' ants_L1 %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     as.numeric(location_id) > 4) %>%
#'   plot_sample_space_time()
#' }
#' 
plot_sample_space_time <- function(
  data,
  id = NA_character_, 
  alpha = 1){
  
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value","unit")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    observation <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
    
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or table that includes columns from an ecocomDP 'observation' table")
    
  }else if(data_type == "dataset_old"){
    observation <- data[[1]]$tables$observation
    if(is.na(id)) id <- names(data)
    
  }else if(data_type == "dataset"){
    observation <- data$tables$observation
    if(is.na(id)) id <- data$id
  }
  
  

  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))

  
  # summarize data for plot
  obs_summary <- observation %>%
    # truncate time and just use dates
    dplyr::mutate(datetime = .data$datetime %>% 
                    lubridate::as_date() %>% 
                    lubridate::ymd()) %>%
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
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, an ecocomDP observation table, or any table containing the columns present in an ecocomDP observation table.
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
#' \dontrun{
#' # plot ecocomDP formatted dataset
#' plot_taxa_shared_sites(ants_L1)
#' 
#' # plot flattened ecocomDP dataset
#' plot_taxa_shared_sites(flatten_data(ants_L1))
#' 
#' # plot ecocomDP observation table
#' plot_taxa_shared_sites(ants_L1[[1]]$tables$observation)
#' 
#' # tidy syntax, filter data by date
#' ants_L1 %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_shared_sites()
#' 
#' # tidy syntax, filter data by site ID
#' ants_L1 %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     as.numeric(location_id) > 4) %>%
#'   plot_taxa_shared_sites()
#' }
#' 
plot_taxa_shared_sites <- function(
  data,
  id = NA_character_){
  
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value","unit")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    observation <- data %>% dplyr::distinct()
    observation <- observation[,req_col_names]
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
    
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or table that includes columns from an ecocomDP 'observation' table")
    
  }else if(data_type == "dataset_old"){
    observation <- data[[1]]$tables$observation
    if(is.na(id)) id <- names(data)
    
  }else if(data_type == "dataset"){
    observation <- data$tables$observation
    if(is.na(id)) id <- data$id
  }
  
  
  
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
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, a flattened ecocomDP dataset, or any flat table containing the columns present in both the ecocomDP observation and taxon tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param facet_var (character) Name of column to use for faceting.
#' @param facet_scales (character) Should scales be free ("free", default value), fixed ("fixed"), or free in one dimension ("free_x", "free_y")?
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
#' \dontrun{
#' # plot ecocomDP formatted dataset
#' plot_taxa_rank(ants_L1)
#' 
#' # download and plot NEON macroinvertebrate data
#' my_dataset <- read_data(
#'   id = "neon.ecocomdp.20120.001.001",
#'   site= c('COMO','LECO'), 
#'   startdate = "2017-06",
#'   enddate = "2019-09",
#'   check.size = FALSE)
#' 
#' plot_taxa_rank(my_dataset)
#' 
#' # facet by location
#' plot_taxa_rank(
#'   data = my_dataset,
#'   facet_var = "location_id")
#' 
#' # plot flattened ecocomDP dataset
#' plot_taxa_rank(
#'   data = flatten_data(my_dataset),
#'   facet_var = "location_id")
#' 
#' 
#' # tidy syntax, filter data by date
#' my_dataset %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_rank()
#' 
#' # tidy syntax, filter data by site ID
#' my_dataset %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     grepl("COMO",location_id)) %>%
#'   plot_taxa_rank()
#' }
#' 
plot_taxa_rank <- function(
  data,
  id = NA_character_, 
  facet_var = NA_character_, #e.g., "location_id", "taxon_id" must be a column name in observation or taxon table
  facet_scales = "free", #Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
  alpha = 1){
  

  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "taxon_rank")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    data_long <- data %>% dplyr::distinct()
    data_long <- data_long[,req_col_names]
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
    
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
    
  }else if(data_type == "dataset_old"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- names(data)
    
  }else if(data_type == "dataset"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }
  

  validate_arguments(fun.name = "plot", fun.args = as.list(environment()))
  
  p <- data_long %>%
    ggplot2::ggplot(
      ggplot2::aes(taxon_rank)) + 
    ggplot2::labs(title = "Taxa rank frequencies in the observation table", subtitle = id) +
    ggplot2::xlab("Taxon rank") +
    ggplot2::ylab(paste0("Number of occurrences")) +
    ggplot2::geom_bar() +
    ggplot2::theme_bw() +
    ggplot2::coord_flip() 
    # ggplot2::theme(
    #   axis.text.x = ggplot2::element_text(angle = 45, hjust=1))
  
  if(!is.na(facet_var)){
    p <- p +
      ggplot2::facet_wrap(
        ~ as.factor(.data[[facet_var]]),
        scales = facet_scales)
  }
  
  return(p)
}










#' Plot stacked taxa by site
#'
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, a flattened ecocomDP dataset, or any flat table containing the columns present in both the ecocomDP observation and taxon tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param min_occurrence (numeric) Minimum number of occurrences allowed for taxa included in the plot.
#' @param facet_var (character) Name of column to use for faceting.
#' @param facet_scales (character) Should scales be free ("free", default value), fixed ("fixed"), or free in one dimension ("free_x", "free_y")?
#' @param color_var (character) Name of column to use for plot colors.
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
#' \dontrun{
#' # plot ecocomDP formatted dataset
#' plot_taxa_occurrence_frequency(ants_L1)
#' 
#' # plot flattened ecocomDP dataset
#' plot_taxa_occurrence_frequency(flatten_data(ants_L1))
#' 
#' # facet by location color by taxon_rank
#' plot_taxa_occurrence_frequency(
#'   data = ants_L1,
#'   facet_var = "location_id",
#'   color_var = "taxon_rank")
#' 
#' # color by location, only include taxa with > 10 occurrences
#' plot_taxa_occurrence_frequency(
#'   data = ants_L1,
#'   color_var = "location_id",
#'   min_occurrence = 5)
#' 
#' # tidy syntax, filter data by date
#' ants_L1 %>% 
#'   flatten_data() %>% 
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_occurrence_frequency()
#' }
#' 
plot_taxa_occurrence_frequency <- function(
  data, 
  id = NA_character_,
  min_occurrence = 0, 
  facet_var = NA_character_, #e.g., "location_id"
  color_var = NA_character_, #e.g., "location_id"
  facet_scales = "free", #Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
  alpha = 1){
  

  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "taxon_name")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    data_long <- data %>% dplyr::distinct()
    # data_long <- data_long[,req_col_names]
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
    
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
    
  }else if(data_type == "dataset_old"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- names(data)
    
  }else if(data_type == "dataset"){
    data_long <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }
  
  
  
  # plot title and subtitle text
  plot_title = "Taxa occurrence frequencies"
  
  plot_subtitle <- paste0("data package id: ",id)
  if(!is.na(min_occurrence) && min_occurrence > 0) plot_subtitle <- paste0(plot_subtitle,
                                             "\ntaxa with >= ", min_occurrence, " occurrences")
  # if(!is.na(rank)) plot_subtitle <- paste0(plot_subtitle,
  #                                          "\ntaxon rank: ", rank)
  # 
  
  # make plot data
  data_working <- data_long %>%
    dplyr::filter(.data$value > 0) %>%
    dplyr::mutate(occurrence = 1)
    
  
  # if(!is.na(rank)) data_working <- data_working %>%
  #   dplyr::filter(.data$taxon_rank == rank)
  
  col_select_list <- c("event_id","taxon_name","occurrence",
                       color_var, facet_var) %>% stats::na.omit()
  
  data_working <- data_working[,col_select_list] %>% 
    dplyr::distinct()
  
  data_occurrence <- data_working %>%
    dplyr::group_by(
      dplyr::across(
        -c(.data$event_id, .data$occurrence))) %>%
    dplyr::summarize(
        n_occurrences = length(.data$occurrence)) %>%
    dplyr::filter(.data$n_occurrences >= min_occurrence)
  

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
    ggplot2::ylab(paste0("Occurrences\n(no. 'event_ids' in which the taxon is observed)")) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y.left = ggplot2::element_text(size = txty))
  
  # return plot
  return(p)
}






#' Plot abundances by event_id
#'
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, a flattened ecocomDP dataset, or any flat table containing the columns present in both the ecocomDP observation and taxon tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param min_abundance (numeric) Minimum abundance allowed for observations of taxa included in the plot.
#' @param trans (character, "identity" is default, "log1p" is x+1 transform) For continuous scales, the name of a transformation object or the object itself. Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt" and "time".
#' @param facet_var (character) Name of column to use for faceting.
#' @param facet_scales (character) Should scales be free ("free", default value), fixed ("fixed"), or free in one dimension ("free_x", "free_y")?
#' @param color_var (character) Name of column to use for plot colors.
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
#' \dontrun{
#' # plot ecocomDP formatted dataset
#' plot_taxa_abundance(
#'   dataset = ants_L1)
#' 
#' # plot flattened ecocomDP dataset, log(x+1) transform abundances
#' plot_taxa_abundance(
#'   flat_data = flatten_dataset(ants_L1),
#'   trans = "log1p")
#' 
#' # facet by location color by taxon_rank, log 10 transformed
#' plot_taxa_abundance(
#'   dataset = ants_L1,
#'   facet_var = "location_id",
#'   color_var = "taxon_rank",
#'   trans = "log10")
#' 
#' # facet by location, only plot taxa of rank = "species"
#' plot_taxa_abundance(
#'   dataset = ants_L1,
#'   facet_var = "location_id",
#'   rank = "Species", min_occurrence = 5,
#'   trans = "log1p")
#' 
#' # color by location, only include taxa with > 10 occurrences
#' plot_taxa_abundance(
#'   dataset = ants_L1,
#'   color_var = "location_id",
#'   trans = "log10")
#' 
#' # tidy syntax, filter data by date
#' ants_L1 %>% 
#'   flatten_dataset() %>% 
#'   dplyr::filter(
#'     lubridate::as_date(datetime) > "2003-07-01") %>%
#'   plot_taxa_abundance(flat_data = .,
#'                       trans = "log1p")
#' }
plot_taxa_abundance <- function(
  data = NULL, 
  id = NA_character_,
  min_abundance = 0, 
  trans = "identity",
  facet_var = NA_character_, #e.g., "location_id"
  color_var = NA_character_, #e.g., "location_id"
  facet_scales = "free", #Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
  alpha = 1) {
  
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "taxon_name")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    flat_data <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
    
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
    
  }else if(data_type == "dataset_old"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- names(data)
    
  }else if(data_type == "dataset"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }
  
  
  
  if(min_abundance > 0) flat_data <- flat_data %>%
      dplyr::filter(.data$value >= min_abundance)
  
  
  # plot title and subtitle text
  plot_title = "Taxa abundances per 'event_id'"
  
  plot_subtitle <- paste0("data package id: ",id)
  if(!is.na(min_abundance) && min_abundance > 0) plot_subtitle <- paste0(plot_subtitle,
                                                           "\nabundance >= ", min_abundance)
  
  
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
  
  # return plot
  return(p)
}





#' Plot sites on US map
#'
#' @param data (list or tbl_df, tbl, data.frame) An ecocomDP formatted dataset, a flattened ecocomDP dataset, or any flat table containing the columns present in both the ecocomDP observation and location tables.
#' @param id (character) Identifier of dataset to be used in plot subtitles.
#' @param alpha (numeric) Alpha-transparency scale of data points. Useful when many data points overlap. Allowed values are between 0 and 1, where 1 is 100\% opaque. Default is 1.
#' @param labels (boolean) Argument to show labels of each US state. Default is TRUE.
#' 
#' @return (gg, ggplot) A gg, ggplot object if assigned to a variable, otherwise a plot to your active graphics device
#' 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # plot map of sites
#' plot_sites(ants_L1)
#' 
#' plot_sites(flatten_data(ants_L1))
#' 
#' 
#' # download and plot NEON macroinvertebrate data
#' my_dataset <- read_data(
#'   id = "neon.ecocomdp.20120.001.001",
#'   site= c('COMO','LECO'), 
#'   startdate = "2017-06",
#'   enddate = "2019-09",
#'   check.size = FALSE)
#' 
#' plot_sites(my_dataset)
#' }
#' 
plot_sites <- function(
  data,
  id = NA_character_,
  alpha = 1,
  labels = TRUE){
  
  # check for suggested packages that are required for this function to work
  if(!"ggrepel" %in% base::rownames(utils::installed.packages())) stop("Please install 'ggrepel' to use this function")
  if(!"usmap" %in% base::rownames(utils::installed.packages())) stop("Please install 'usmap' to use this function")
  if(!"maptools" %in% base::rownames(utils::installed.packages())) stop("Please install 'maptools' to use this function")
  if(!"rgdal" %in% base::rownames(utils::installed.packages())) stop("Please install 'rgdal' to use this function")
  
  
  
  # required col names in flat data
  req_col_names <- c("observation_id","event_id","package_id","location_id",
                     "datetime","taxon_id","variable_name","value",
                     "location_id","location_name","longitude","latitude")
  
  # detect data type, extract observation table
  data_type <- detect_data_type(data)
  
  if(data_type == "flat_table" && all(req_col_names %in% names(data))){
    flat_data <- data %>% dplyr::distinct()
    if(is.na(id)) id <- paste(unique(data$package_id), collapse = " | ")
    
  }else if(data_type == "flat_table" && !all(req_col_names %in% names(data))){
    stop("please provide a valid ecocomDP dataset or a table that includes the columns present in the ecocomDP 'observation' and 'taxon' tables")
    
  }else if(data_type == "dataset_old"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- names(data)
    
  }else if(data_type == "dataset"){
    flat_data <- flatten_data(data)
    if(is.na(id)) id <- data$id
  }
  
  
  
  
  cleaned <- flat_data %>%
    dplyr::select(
      .data$longitude, 
      .data$latitude, 
      .data$location_name,
      .data$package_id) %>%
    dplyr::distinct()
  
  transformed_cleaned <- usmap::usmap_transform(cleaned)
  
  usmap::plot_usmap(color = "grey") + 
    ggplot2::geom_point(
      data = transformed_cleaned,
      ggplot2::aes(x = .data$longitude.1, y = .data$latitude.1, size = 20),
      color = "red", alpha = alpha) +
    ggrepel::geom_text_repel(
      data = transformed_cleaned,
      aes(x = .data$longitude.1, y = .data$latitude.1, label = .data$location_name),
      size = 3, max.overlaps = Inf) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab(paste0("Latitude")) +
    ggplot2::labs(title = "Map of sites", subtitle = id) +
    ggplot2::theme(legend.position = "none")
  
}
