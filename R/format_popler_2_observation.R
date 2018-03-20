#' Reformat popler data to ecocomDP observation table
#'
#' @description  
#'     Load datasets from the popler database 
#'     (https://github.com/AldoCompagnoni/popler) and reformat to the 
#'     observation table of the ecocomDP project
#'     (https://github.com/EDIorg/ecocomDP/tree/master).
#'
#' @usage format_popler_2_observation(path.in = "", data.list = c("data.list.1", 
#'     "data.list.2", "etc."), path.out = "")
#' 
#' @param path.in
#'     A character string specifying the full path to the tab delimited table
#'     containing the list of popler datasets to be reformatted. This table
#'     must contain at least 1 of the following fields:
#'         \itemize{
#'             \item \strong{title} The full title of the popler dataset (as it
#'             appears in the popler database) to be formatted into the 
#'             ecocomDP.
#'             \item \strong(proj_metadata_key) The 
#'             }
#'         }
#' @param path.out
#'     A character string specifying a path to the directory to which the 
#'     reformatted popler datasets will be exported to.
#' @param data.list
#'     A list of character strings specifying the names of the data list files. 
#' 
#' @return 
#'     Data tables (.csv) in the ecocomDP format to the directory specified
#'     by the path.out argument.
#'
#' @export
#'

format_popler_2_observation <- function(path.in, path.out, data.list){
  
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(path.in)){
    stop('Input argument "path.in" is missing! Specify the path to your data list.')
  }
  if (missing(path.out)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }
  if (missing(data.list)){
    stop('Input argument "data.files" is missing! Specify the names of all the data files in your dataset.')
  }

  # Validate path
  
  validate_path(path)
  
  # Validate data.files
  
  data_files <- validate_file_names(path, data.files)
  
  # Validate os
  
  if (isTRUE((os != "win") & (os != "mac"))){
    stop('The value of input argument "os" is invalid.')
  }
  
  # Testing -------------------------------------------------------------------
  
  # Make list of target datasets, w/these fields:
  # - Dataset names
  # - Popler ID
  
  # Load list
  
  # Load dataset(s)
  
  # Format
  # Create package_id (Eoes popler or linked metadata contain this?)
  # Create location_id (Requires location table.)
  # Create observation_datetime
  # Create taxon_id (Requires taxon table)
  # Create variable_name
  # Create value
  # Create unit
  
  # Write
  
  

  
  # Detect field delimiters ---------------------------------------------------
  
  delim_guess <- c()
  data_path <- c()
  for (i in 1:length(data_files)){
    
    data_path[i] <- paste(path,
                          "/",
                          data_files[i],
                          sep = "")
    
    nlines <- length(readLines(data_path[i],
                               warn = F))
    
    if (os == "mac"){
      
      delim_guess[i] <- suppressWarnings(get.delim(data_path[i],
                                                   n = 2,
                                                   delims = c("\t",
                                                              ",",
                                                              ";",
                                                              "|")))
    } else if (os == "win"){
      
      delim_guess[i] <- get.delim(data_path[i],
                                  n = 1,
                                  delims = c("\t",
                                             ",",
                                             ";",
                                             "|"))
    }
    
    # Secondary check on delimeter detection with manual override
    
    detect_delimeter_2 <- function(data.file, delim.guess){
      file_ext <- substr(data.file, 
                         nchar(data.file)-3,
                         nchar(data.file))
      if (is.null(delim.guess)){
        message(paste("I'm having trouble identifying the field delimeter of ", data.file,
                      ". Enter the field delimeter of this file.",
                      ' Valid options are:  ,  \\t  ;  |', sep = ""))
        answer <- readline('ENTER here: ')
        if (answer == "\\t"){
          answer <- "\t"
        }
      } else if ((delim.guess == ",") & (file_ext == ".txt")){
        message(paste("I'm having trouble identifying the field delimeter of ", data.file,
                      ". Enter the field delimeter of this file.",
                      ' Valid options are:  ,  \\t  ;  |', sep = ""))
        answer <- readline('ENTER here: ')
        if (answer == "\\t"){
          answer <- "\t"
        }
      } else if ((delim.guess == "\t") & (file_ext == ".csv")){
        message(paste("I'm having trouble identifying the field delimeter of ", data.file,
                      ". Enter the field delimeter of this file.",
                      ' Valid options are:  ,  \\t  ;  |', sep = ""))
        answer <- readline('ENTER here: ')
        if (answer == "\\t"){
          answer <- "\t"
        }
      } else if ((delim.guess == "|") & (file_ext == ".csv")){
        message(paste("I'm having trouble identifying the field delimeter of ", data.file,
                      ". Enter the field delimeter of this file.",
                      ' Valid options are:  ,  \\t  ;  |', sep = ""))
        answer <- readline('ENTER here: ')
        if (answer == "\\t"){
          answer <- "\t"
        }
      } else if ((delim.guess == ";") & (file_ext == ".csv")){
        message(paste("I'm having trouble identifying the field delimeter of ", data.file,
                      ". Enter the field delimeter of this file.",
                      ' Valid options are:  ,  \\t  ;  |', sep = ""))
        answer <- readline('ENTER here: ')
        if (answer == "\\t"){
          answer <- "\t"
        }
      } else {
        answer <- delim.guess
      }
      answer
    }
    
    delim_guess[i] <- detect_delimeter_2(data.file = data_files[i],
                                         delim.guess = delim_guess[i])
    
  }
  
  delim_guess
  
}
