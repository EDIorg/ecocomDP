#' Validate file names
#'
#' @description  
#'     Identify whether input data file names exist in the specified directory.
#'
#' @usage validate_file_names(path = "", data.files = data.files = c("data.file.1", 
#'     "data.file.2", "etc.")).
#' 
#' @param path 
#'     A character string specifying a path to the dataset working directory.
#' @param data.files
#'     A list of character strings specifying the names of the data files
#'     of your dataset. It is not necessary to include the file extension.
#' 
#' @return 
#'     A warning message if the data files don't exist at path, and which of
#'     the input data files are missing.
#'     
#'     The full names of files listed in the data.files argument.
#'
#' @export
#'

validate_file_names <- function(path, data.files){
  
  # Check arguments -----------------------------------------------------------

  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }
  if (missing(data.files)){
    stop('Input argument "data.files" is missing! Specify the names of all the data files in your dataset.')
  }
  
  # Validate path
  
  validate_path(path)
  
  # Validate names ------------------------------------------------------------
  
  files <- list.files(path)
  files <- c(files, str_replace(files, "\\.[:alnum:]*$", replacement = ""))
  use_i <- str_detect(string = files,
                      pattern = str_c("^", data.files, "$", collapse = "|"))
  if (!sum(use_i) == length(data.files)){
    if(sum(use_i) == 0){
      stop(paste("Invalid data.files entered: ", paste(data.files, collapse = ", "), sep = ""))
    } else {
      name_issues <- data.files[!files[use_i] == data.files]
      stop(paste("Invalid data.files entered: ", paste(name_issues, collapse = ", "), sep = ""))
    }
  }
  
  # Get file names ------------------------------------------------------------

  files <- list.files(path)
  use_i <- str_detect(string = files,
                      pattern = str_c("^", data.files, collapse = "|"))
  data_files <- files[use_i]
  
  # Reorder file names to match input ordering --------------------------------
  
  data_files_out <- c()
  for (i in 1:length(data.files)){
    use_i <- str_detect(string = data_files,
                        pattern = str_c("^", data.files[i], collapse = "|"))
    data_files_out[i] <- data_files[use_i]
  }
  
  data_files_out

}
