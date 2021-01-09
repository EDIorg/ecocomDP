#' Does a data package have a ecocomDP or DwC-A child?
#'
#' @param keyword (character) Specifies the child type. Can be: "ecocomDP" or "Darwin Core Archive (DwC-A) Event Core"
#' @param package.id (character) Data package identifier
#'
#' @return (named logical) Named after the NEWEST child data package identifier
#' descending from \code{package.id}. It's possible to have multiple children
#' REVISIONS originating from a \code{package.id}. This may occur when there 
#' are issues with the first child of \code{package.id} that are fixed by a 
#' follow on revision.
#' 
has_child <- function(keyword, package.id) {
  
  # Load Global Environment config --------------------------------------------
  
  if (exists("config.repository", envir = .GlobalEnv)) {
    repository <- get("config.repository", envir = .GlobalEnv)
  } else {
    repository <- "EDI"
  }
  
  if (exists("config.environment", envir = .GlobalEnv)) {
    environment <- get("config.environment", envir = .GlobalEnv)
  } else {
    environment <- "production"
  }
  
  # Repository specific methods -----------------------------------------------
  
  # EDI
  
  if (repository == "EDI") {
    
    children <- xml2::xml_text(
      xml2::xml_find_all(
        suppressMessages(
          EDIutils::api_list_data_descendants(package.id, environment)),
        ".//packageId"))
    
    if (length(children) != 0) {
      
      # It's possible for a package.id to have multiple versions of a child 
      # resembling one of the target data types. Get child specific attributes 
      # for analysis.
      
      r <- sapply(
        children,
        function(child) {
          
          eml <- suppressMessages(
            EDIutils::api_read_metadata(child, environment))
          pubdate <- get_report_datetime(child, environment)
          
          if (keyword == "ecocomDP") {
            
            ischild <- "ecocomDP" %in% 
              xml2::xml_text(xml2::xml_find_all(eml, ".//keyword"))
            
            script <- "create_ecocomdp.r" %in% 
              tolower(
                xml2::xml_text(
                  xml2::xml_find_all(
                    eml, ".//otherEntity/physical/objectName")))
            
            res <- list(
              id = child, 
              pubdate = pubdate, 
              ischild = ischild, 
              script = script)
            
            return(res)
            
          } else if (keyword == "Darwin Core Archive (DwC-A) Event Core") {
            
            ischild <- "Darwin Core Archive (DwC-A) Event Core" %in% 
              xml2::xml_text(xml2::xml_find_all(eml, ".//keyword"))
            
            res <- list(
              id = child, 
              pubdate = pubdate, 
              ischild = ischild)
            
            return(res)
            
          }
        }, 
        simplify = FALSE)
      
      r <- data.table::rbindlist(r)
      
      # Analyze attributes to find an accurate match. I.e. the most recently
      # published and (in the case of ecocomDP) having a conversion script of 
      # the correct format.
      
      if (keyword == "ecocomDP" & any(r$ischild)) {
        
        has_script <- r$script
        r <- r[has_script, ]
        
        newest <- lubridate::ymd_hms(r$pubdate) %in% 
          max(lubridate::ymd_hms(r$pubdate))
        r <- r[newest, ]
        
        res <- r$ischild
        names(res) <- r$id
        
        return(res)
        
      } else if (keyword == "Darwin Core Archive (DwC-A) Event Core" & 
                 any(r$ischild)) {
        
        newest <- lubridate::ymd_hms(r$pubdate) %in% 
          max(lubridate::ymd_hms(r$pubdate))
        r <- r[newest, ]
        
        res <- r$ischild
        names(res) <- r$id
        
        return(res)
        
      } else {
        
        return(FALSE)
        
      }
      
    } else {
      
      return(FALSE)
      
    }
    
  }
  
}








#' Get datetime of data package quality report from the EDI Data Repository
#'
#' @param package.id (character) Data package identifier
#' @param environment (character) Data repository environment
#'
#' @return (character) Datetime in the form YYYY-MM-DDThh:mm:ss
#'
get_report_datetime <- function(package.id, environment) {
  report <- suppressMessages(
    EDIutils::api_read_data_package_report(package.id, environment))
  datetime <- xml2::xml_text(xml2::xml_find_all(report, ".//qr:creationDate"))
  return(datetime)
}