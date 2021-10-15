#' Save a dataset
#'
#' @param dataset (list) One or more datasets of the structure returned by \code{read_data()}. Name of the \code{dataset} object will become the file name if \code{name} is not used.
#' @param path (character) Path to the directory in which \code{dataset} will be written.
#' @param type (character) Type of file to save the \code{dataset} as. Default is ".rds" but can also be ".csv". Note: metadata and validation_issues are lost when using ".csv".
#' @param name (character) An optional argument for setting the saved file name (for .rds) if you'd like it to be different than \code{dataset}'s object name.
#'     
#' @note Subsequent calls won't overwrite files or directories
#'
#' @return
#'     \item{.rds}{If \code{type = ".rds"}, then an .rds representation of \code{dataset} is returned.}
#'     \item{.csv}{If \code{type = ".csv"}, then an set of .csv files are written to a sub-directory of \code{path} named after the data package/product ID.}
#'     
#' @export
#'
#' @examples
#' # Create directory for the data
#' mypath <- paste0(tempdir(), "/data")
#' dir.create(mypath)
#' 
#' # Save as .rds
#' save_data(ants_L1, mypath)
#' dir(mypath)
#' 
#' # Save as .rds with the name "mydata"
#' save_data(ants_L1, mypath, name = "mydata")
#' dir(mypath)
#' 
#' # Save as .csv
#' save_data(ants_L1, mypath, type = ".csv")
#' dir(mypath)
#' 
#' \dontrun{
#' # Save multiple datasets
#' ids <- c("edi.193.5", "edi.303.2", "edi.290.2")
#' datasets <- lapply(ids, read_data)
#' save_data(datasets, mypath)
#' dir(mypath)
#' }
#' 
#' # Clean up
#' unlink(mypath, recursive = TRUE)
#' 
save_data <- function(dataset, path, type = ".rds", name = NULL) {
  
  validate_arguments(fun.name = "save_data", fun.args = as.list(environment()))
  if (is.null(name)) {
    name <- deparse(substitute(dataset))
  }
  
  # For 1 dataset
  if (detect_data_type(dataset) == "dataset") {
    if (type == ".rds") {
      name <- paste0(name, ".rds")
      message("Writing ", name, " to ", path)
      if (file.exists(paste0(path, "/", name))) { # don't overwrite .rds file
        warning("'", paste0(path, "/", name), "'", " already exists", call. = FALSE)
      } else {
        saveRDS(dataset, file = paste0(path, "/", name))
      }
    } else if (type == ".csv") {
      id <- dataset$id
      message("Writing ", paste(id, collapse = ", "), " as .csv to ", path)
      subdir <- paste0(path, "/", id)
      if (dir.exists(subdir)) { # don't overwrite subdirs and tables
        warning("'", subdir, "'", " already exists", call. = FALSE)
      } else {
        dir.create(subdir)
        for (j in 1:length(dataset$tables)) {
          tblname <- paste0(names(dataset$tables)[j], ".csv")
          if (file.exists(paste0(subdir, "/", tblname))) {
            warning("'", subdir, "/", tblname, "'", " already exists", call. = FALSE)
          } else {
            data.table::fwrite(
              dataset$tables[[j]], file = paste0(subdir, "/", tblname), sep = ",", na = "NA")
          }
        }
      }
    }
  }
  
  # For > 1 dataset
  if (detect_data_type(dataset) == "list_of_datasets") {
    if (type == ".rds") {
      name <- paste0(name, ".rds")
      message("Writing ", name, " to ", path)
      if (file.exists(paste0(path, "/", name))) { # don't overwrite .rds file
        warning("'", paste0(path, "/", name), "'", " already exists", call. = FALSE)
      } else {
        saveRDS(dataset, file = paste0(path, "/", name))
      }
    } else if (type == ".csv") { # as .csv
      ids <- unlist(lapply(dataset, function(x) {x$id}))
      if (is.null(ids)) {
        ids <- seq(length(dataset))
      }
      message("Writing ", paste(ids, collapse = ", "), " as .csv to ", path)
      subdirs <- paste0(path, "/", ids)
      for (i in 1:length(subdirs)) {
        if (dir.exists(subdirs[i])) { # don't overwrite subdirs and tables
          warning("'", subdirs[i], "'", " already exists", call. = FALSE)
        } else {
          dir.create(subdirs[i])
          for (j in 1:length(dataset[[i]]$tables)) {
            tblname <- paste0(names(dataset[[i]]$tables)[j], ".csv")
            if (file.exists(paste0(subdirs[i], "/", tblname))) {
              warning("'", subdirs[i], "/", tblname, "'", " already exists", call. = FALSE)
            } else {
              data.table::fwrite(
                dataset[[i]]$tables[[j]], file = paste0(subdirs[i], "/", tblname), sep = ",", na = "NA")
            }
          }
        }
      }
    }
  }
  
}
