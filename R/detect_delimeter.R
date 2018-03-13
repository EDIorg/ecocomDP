#' Detect field delimeter
#'
#' @description  
#'     Detect field delimiter of input files and return character string 
#'     representation.
#'
#' @usage detect_delimeter(path = "", data.files = c("data.file.1", 
#'     "data.file.2", "etc.") , os = "")
#' 
#' @param path 
#'     A character string specifying a path to the dataset working directory
#'     containing the data files from which to detect the field delimeters.
#' @param data.files
#'     A list of character strings specifying the names of the data files
#'     from which to detect the field delimeters of. 
#' @param os
#'     A character string specifying the operating system in which this 
#'     function is to be called. Valid options are generated from 
#'     \code{detect_os}.
#' 
#' @return 
#'     A character string representation of the field delimeters listed in 
#'     order of files listed in the data.files argument.
#'     \item{"\\t"}{tab}
#'     \item{","}{comma}
#'     \item{";"}{semi-colon}
#'     \item{"|"}{pipe}
#'
#' @export
#'

detect_delimeter <- function(path, data.files, os){
  
  # Check arguments -----------------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }
  if (missing(data.files)){
    stop('Input argument "data.files" is missing! Specify the names of all the data files in your dataset.')
  }
  if (missing(os)){
    stop('Input argument "os" is missing! Specify your operating system.')
  }
  
  # Validate path
  
  validate_path(path)
  
  # Validate data.files
  
  data_files <- validate_file_names(path, data.files)
  
  # Validate os
  
  if (isTRUE((os != "win") & (os != "mac"))){
    stop('The value of input argument "os" is invalid.')
  }
  
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
