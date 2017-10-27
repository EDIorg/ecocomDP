#' Clean taxon (WARNING: this function is not yet functional)
#'
#' @description  
#'     Clean taxonomy data of level-0 dataset and export to file.
#'
#' @usage clean_taxon(path, parent.package.id, table.name, taxon.col)
#' 
#'     Run this function after making ecocomDP tables, which includes a draft
#'     version of the taxon table.
#'
#' @param path 
#'     A path to the dataset working directory where the clean taxon table will
#'     be exported.
#'     
#' @param delimiter
#'     Delimiter of taxon table in the working directory. Can be comma (",") or
#'     tab ("\\t").
#'     
#' @param cleaning.step
#'     Step in the data cleaning process. Can be L0, L1, L2, L3, L4.
#'
#' @return 
#'     A tab delimited UTF-8 file in the ecocomDP working directory appended
#'     with a title corresponding with the cleaning step level.
#'     
#' @details 
#'     This function fails out if tables created with this workflow already
#'     exist in the working director. Remove these files or rename to allow 
#'     the function to continue.
#'
#' @export
#'


clean_taxon <- function(path, delimiter, cleaning.step) {
  
  # Check arguments
  
  if (missing(path)){
    stop("Specify path to dataset working directory.")
  }
  if (missing(delimiter)){
    stop("Specify a delimiter to the ecocomDP taxon table in the working directory.")
  }
  if (missing(cleaning.step)){
    stop("Specify a cleaning step.")
  }
  
  # Parameters ----------------------------------------------------------------
  
  dir_files <- list.files(path)
  valid_table_names <- c("taxon\\b",
                         "taxon_L1\\b",
                         "taxon_L2\\b",
                         "taxon_L3\\b",
                         "taxon_L4\\b")
  input_table_names <- dir_files[attr(regexpr(paste(valid_table_names, 
                                                    collapse = "|"), 
                                              dir_files),
                                      "match.length")
                                 != -1]
  
  # Step 1 --------------------------------------------------------------------
  # Get the taxon name from the raw dataset (in this case the taxon_name field
  # of the ecocomDP taxon table) and append a column for an information manager
  # to pass judgement on whether the listed taxon name has any taxonomic value.
  # For example an entity listed as "stone" or "unsorted biomass" will not be 
  # listed in a taxonomic authority and thus has no taxonomic value.

  if (cleaning.step == "1"){
    if (identical(input_table_names[attr(regexpr("taxon_L1\\b", input_table_names), "match.length") != -1], character(0))){
      stop("A taxon_L1 table already exists in this directory. Remove this file
           or rename and try again.")
    } else {
      input_file_name <- input_table_names[attr(regexpr("taxon\\b", input_table_names), "match.length") != -1]
      clean_taxon_step1 <- function(path, input_file_name, delimiter){
        data_in <- read.table(paste(path,
                                    "/",
                                    input_file_name,
                                    sep = ""),
                              header = T,
                              sep = delimiter,
                              as.is = T,
                              na.strings = "NA")
        data_out <- data.frame(taxon_name = data_in[["taxon_name"]],
                               meaningful = character(length(data_in[["taxon_name"]])))
        if (delimiter == ","){
          write.table(data_out,
                      file = paste(path,
                                   "/",
                                   "taxon_L0.csv",
                                   sep = ""),
                      col.names = T,
                      row.names = F,
                      sep = ",",
                      eol = "\r\n",
                      quote = F)
        } else if (delimiter == "\t"){
          write.table(data_out,
                      file = paste(path,
                                   "/",
                                   "taxon_L1.txt",
                                   sep = ""),
                      col.names = T,
                      row.names = F,
                      sep = "\t",
                      eol = "\r\n",
                      quote = F)
        }
        # Send table to taxize or IM for evaluation of meaningful entities?
        
        
        
      }
    }
  }
}

