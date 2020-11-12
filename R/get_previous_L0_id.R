#' Get previous L0 data package identifier and URL link to conversion script
#'
#' @description Get the most recent L0 data package identifier containing a recognizable L0 to L1 conversion script and get the URL to download the script as well.
#'
#' @param package.id (character) Newest L0 data package identifier
#' @param repository (character) Data repository in which \code{package.id} resides and associated with \code{environment}. Currently supported repositories are: "EDI" (Environmental Data Initiative). Requests for support of other repositories can be made via \href{https://github.com/EDIorg/ecocomDP}{ecocomDP GitHub} issues. Default is "EDI".
#' @param evironment (character) Repository environment in which \code{package.id} exists. Some repositories have development, staging, and production environments which are distinct from one another. This argument allows reading of EML from different environments. Default is "production".
#' 
#' @details For the most recent L0 data package, which may not have a corresponding L1, follow all provenance traces to data packages containing the "ecocomDP" keyword. If more than one, select the most recent L0 identifier, read the corresponding EML, look for a valid L0 to L1 conversion script (convert_ecocomDP.R), and break if found. Otherwise try this process again for the next oldest L0 data package and so on.
#'
#' @return (list) A list containing:
#' \item{id}{Identifier of previous L0 data package}
#' \item{script}{Download URL of the script used to convert the L0 to L1}
#' Otherwise NULL
#' 
#' @export
#'
get_previous_L0_id_and_L1_conversion_script <- function(package.id,
                                                        repository = "EDI",
                                                        environment = "production") {
  
  # Repository specific methods
  
  if (repository == "EDI") {
    
    # List all previous versions
    
    package_id_components <- stringr::str_split(package.id, "\\.")[[1]][1:2]

    versions <- EDIutils::api_list_data_package_revisions(
      scope = package_id_components[1],
      identifier = package_id_components[2], 
      environment = environment)
    
    previous_versions <- rev(
      paste0(
        paste(package_id_components[1:2], collapse = "."),
        ".",
        versions[1:length(versions) - 1]))
    
    # Include newest version incase the L1 was updated using a different method
    
    all_versions <- c(package.id, previous_versions)
    
    # Find the most recent linking to an L1 and a recognizable conversion script name
    
    r <- lapply(
      all_versions,
      function(version) {
        
        # TODO: stop update_L1() if there already exists an L1 for the newest L0
        
        descendants <- xml2::xml_text(
          xml2::xml_find_all(
            EDIutils::api_list_data_descendants(version, environment),
            ".//packageId"))
        
        r <- NULL
        if (length(descendants) != 0) {
          # Get all descendants with scripts
          r <- lapply(
            descendants,
            function(descendant) {
              eml <- EDIutils::api_read_metadata(descendant, environment)
              if ("ecocomDP" %in% 
                  xml2::xml_text(xml2::xml_find_all(eml, ".//keyword"))) {
                other_entities <- xml2::xml_text(
                  xml2::xml_find_all(eml, ".//otherEntity/physical/objectName"))
                # script <- stringr::str_detect(
                #     tolower(other_entities), "^create_ecocomdp.r$") # Exact match
                script <- stringr::str_detect(
                    tolower(other_entities), "_ecocomdp.r$") # Fuzzy match
                urls <- xml2::xml_text(
                  xml2::xml_find_all(eml, ".//otherEntity/physical/distribution/online/url"))
                if (any(script)) {
                  pubdate <- xml2::xml_text(
                    xml2::xml_find_all(eml, ".//pubDate"))
                  return(list(id = version, script_url = urls[script], pubdate = pubdate))
                } else {
                  return(NULL)
                }
              } else {
                return(NULL)
              }
            })
        }

        if (!is.null(unlist(r))) {
          # Get newest pubdate and if tie then revision number
          r <- data.table::rbindlist(r)
          r$pubdate <- lubridate::ymd(r$pubdate)
          i <- which(r$pubdate == min(r$pubdate))
          if (sum(i) == 1) {
            return(r[i, ])
          } else {
            pkg_nums <- stringr::str_remove_all(
              r$id, "^.*\\.(?=[:digit:]*\\.[:digit:]*)")
            i <- pkg_nums == pkg_nums[
              as.numeric(pkg_nums) == max(as.numeric(pkg_nums))]
            return(r[i, ])
          }
        } else {
          return(NULL)
        }
        
      })
    
    if (!is.null(unlist(r))) {
      r <- data.table::rbindlist(r)
      r <- list(
        id = r[1, "id"],
        script = r[1, "script_url"])
      return(r)
    } else {
      return(NULL)
    }
    
  }
  
}