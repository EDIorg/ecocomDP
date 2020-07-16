#' Join observation, location, and taxon tables in a wide format
#'
#' @param dt_obs
#'     (data.frame) The observation table
#' @param dt_loc 
#'     (data.frame) The location table
#' @param dt_tax 
#'     (data.frame) The taxon table
#' @return
#'     (data.frame) Joined observation, location, and taxon tables in a wide 
#'     format, where the location table has been unnested for latitude, 
#'     longitude, and site names.
#' @details 
#'     TODO: "Denormalizd" + describe the unnesting logic.
#'     TODO: Intended to be used within a function that has already read the 
#'     data tables.
#' @export
#'
#' @examples
#' \dontrun{
#' long2wide_obs_loc_tax(
#'   dt_obs = my_package_observation,
#'   dt_loc = my_package_location,
#'   dt_tax = my_package_taxon)
#' }
#' 
long2wide_obs_loc_tax <- function(dt_obs, dt_loc, dt_tax) {
  
  # Expand location table -----------------------------------------------------
  
  # Rewrite names of locations to indicate their nested arrangements and expand
  # corresponding latitude, longitude, and elevation
  loc_name_combined <- rep(NA_character_, nrow(dt_loc))
  for (i in 1:length(dt_loc$location_id)) {
    if (!is.na(dt_loc$parent_location_id[i])) {
      id_out <- dt_loc$location_id[i]
      id <- dt_loc$location_id[i]
      cont <- TRUE
      while (isTRUE(cont)) {
        if (!is.na(dt_loc$parent_location_id[id == dt_loc$location_id])) {
          id_out[length(id_out) + 1] <- dt_loc$parent_location_id[id == dt_loc$location_id]
          id <- dt_loc$parent_location_id[id == dt_loc$location_id]
        } else {
          cont <- FALSE
        }
      }
      loc_name_combined[i] <- paste(
        dt_loc$location_name[
          dt_loc$location_id %in% rev(id_out)], 
        collapse = ".")
      lat_i <- max(
        which(
          !is.na(
            dt_loc$latitude[
              dt_loc$location_id %in% rev(id_out)])))
      lon_i <- max(
        which(
          !is.na(
            dt_loc$longitude[
              dt_loc$location_id %in% rev(id_out)])))
      elv_i <- max(
        which(
          !is.na(
            dt_loc$elevation[
              dt_loc$location_id %in% rev(id_out)])))
      if (!is.na(lat_i) & !is.na(lon_i)) {
        if (lat_i == lon_i) {
          fill <- dt_loc$latitude[dt_loc$location_id %in% rev(id_out)]
          fill[is.na(fill)] <- dt_loc$latitude[dt_loc$location_id %in% rev(id_out)][lat_i]
          dt_loc$latitude[dt_loc$location_id %in% rev(id_out)] <- fill
          fill <- dt_loc$longitude[dt_loc$location_id %in% rev(id_out)]
          fill[is.na(fill)] <- dt_loc$longitude[dt_loc$location_id %in% rev(id_out)][lat_i]
          dt_loc$longitude[dt_loc$location_id %in% rev(id_out)] <- fill
          if (elv_i == lat_i) {
            fill <- dt_loc$elevation[dt_loc$location_id %in% rev(id_out)]
            fill[is.na(fill)] <- dt_loc$elevation[dt_loc$location_id %in% rev(id_out)][lat_i]
            dt_loc$elevation[dt_loc$location_id %in% rev(id_out)] <- fill
          }
        }
      }
    }
  }
  
  # Create the expanded location table
  loc_name_combined[is.na(dt_loc$parent_location_id)] <- dt_loc$location_name[is.na(dt_loc$parent_location_id)]
  dt_loc_expanded <- dt_loc
  dt_loc_expanded$location_name <- loc_name_combined
  dt_loc_expanded$parent_location_id <- NULL
  
  # Left join observation, location (expanded), and taxon tables
  # TO DO: ask Colin for his naming convention for temporary objects
    temp <- dplyr::left_join(
    dplyr::left_join(
      dt_obs, dt_loc_expanded, 
      by = "location_id"), 
    dt_tax, by = "taxon_id")
  
  # Pivot the joined df to spread the non-unique columns
  join_wide <-tidyr::pivot_wider(temp, names_from = variable_name, values_from=c(observation_id, value, unit))
  # TO DO: QC - handle errors and warnings. incoming tables with duplicate rows contain a list (see edi.323.1)


  
  
  return(join_wide)
}

