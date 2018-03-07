#' Validate table fields
#'
#' @description  
#'     This function checks for congruence in the number of fields among rows 
#'     of a data table.
#'
#' @usage 
#'     validate_fields(path = "", data.files = c("data.file.1", "data.file.2", "etc."))
#'
#' @param path 
#'     A character string specifying a path to the dataset working directory.
#' @param data.files
#'     A list of character strings specifying the names of the data files
#'     of your dataset. It is not necessary to include the file extension.
#'
#' @return 
#'     If incongruence is found an error is returned.
#'     
#' @export     
#'   

validate_fields <- function(path, data.files){
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }
  if (missing(data.files)){
    stop('Input argument "data.files" is missing! Specify the names of all the data files in your dataset.')
  }
  
  # Validate path
  
  validate_path(path)
  
  # Validate data.files
  
  data_files <- validate_file_names(path, data.files)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Detect delimeter
  
  delim_guess <- detect_delimeter(path, data.files = data_files, os)
  
  # Function to detect and report errors in field delimiters
  # delim.counts = Row sums of field delimiters detected by the count.fields
  # function
  
  detect_errors <- function(delim.counts){
    counts <- delim.counts[!is.na(delim.counts)]
    counts_uni <- unique(counts)
    if (length(counts_uni) > 1){
      counts_tbl <- as.data.frame(table(counts))
      guess_correct <- counts_tbl$counts[counts_tbl$Freq %in% max(counts_tbl$Freq)]
      guess_incorrect <- counts_tbl$counts[!counts_tbl$counts %in% guess_correct]
      locations_incorrect <- sort(seq(1, length(counts))[counts %in% guess_incorrect])
      stop(paste0(data_files[i], " contains an inconsistent number of field delimeters. ",
                  "The correct number of field delimiters for this table appears to be ", guess_correct, ". ",
                  "Deviation from this occurs at rows: ", paste(locations_incorrect, collapse = ", "),
                  " ... Check the number of field delimiters in these rows. All rows of your table must contain a consistent number of fields."))
    }
  }
  
  # Detect inconsistent field lengths -----------------------------------------
  
  data_path <- c()
  for (i in 1:length(data_files)){
    
    data_path[i] <- paste(path,
                          "/",
                          data_files[i],
                          sep = "")
    
    # Use quote character that produces the fewest NAs, then break ties with 
    # with quote character producing the fewest unique field delimiters.
    
    
    count_quote <- count.fields(file = data_path[i],
                                sep = delim_guess[i],
                                quote = "\"")
    
    count_appos <- count.fields(file = data_path[i],
                                sep = delim_guess[i],
                                quote = "\'")

    if (sum(is.na(count_quote)) < sum(is.na(count_appos))){
      detect_errors(count_quote)
    } else if (sum(is.na(count_appos)) < sum(is.na(count_quote))){
      detect_errors(count_appos)  
    } else if (sum(is.na(count_appos)) == sum(is.na(count_quote))){
      count_quote_uni <- unique(count_quote[!is.na(count_quote)])
      count_appos_uni <- unique(count_appos[!is.na(count_appos)])
      if (length(count_quote_uni) < length(count_appos_uni)){
        detect_errors(count_quote)
      } else if (length(count_quote_uni) > length(count_appos_uni)){
        detect_errors(count_appos)
      } else if (length(count_quote_uni) == length(count_appos_uni)){
        detect_errors(count_appos)
      }
    }
  }
  
}
