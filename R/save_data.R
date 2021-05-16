#' Save ecocomDP data
#'
#' @param data 
#'     (list) Data as a list object created by \code{read()}. Name of object will become file name if \code{file.name} is not used. Can be a list of these objects, i.e. more than one.
#' @param path
#'     (character) Path to the directory in which the data will be written.
#' @param file.type
#'     (character) Type of file to save the data to. Default is ".rds" but can also be ".csv". Note: metadata and validation_issues are lost when ".csv". Recommended to use .rds is using R because, all objects in one place, don't have to handle directory naming
#' @param file.name
#'     (character) Use this to set the file name (for .rds), or dir name (for .csv) if you'd like to be different than \code{data}.
#'     
#' @note Subsequent calls won't overwrite dirs or files
#'
#' @return
#'     \item{.rda}{If \code{file.type} = ".rda", then an .rda representation 
#'     of \code{data} is returned.}
#'     \item{.csv}{If \code{file.type} = ".csv", then an set of .csv files are
#'     written to a sub-directory of \code{path} named after the data 
#'     package/product ID.}
#'     
#' @export
#'
#' @examples
#' \dontrun{
#' # Read from source
#' datasets <- c(
#'   read("edi.193.3"),
#'   read("edi.262.1"),
#'   read("edi.359.1"))
#' 
#' # Save as .rds
#' save_data(datasets, "/Users/me/documents/data")
#' 
#' # Save as .csv
#' save_data(datasets, "/Users/me/documents/data", file.type = ".csv")
#' }
#' 
save_data <- function(data, path, file.type = ".rds", file.name = NULL) {
  validate_arguments(fun.name = "save_data", fun.args = as.list(environment()))
  if (is.null(file.name)) {
    file.name <- deparse(substitute(data))
  }
  if (file.type == ".rds") {                                                        # as .rds
    file.name <- paste0(file.name, ".rds")
    message("Writing ", file.name, " to ", path)
    if (file.exists(paste0(path, "/", file.name))) {                                # don't overwrite .rds file
      warning("'", paste0(path, "/", file.name), "'", " already exists", call. = FALSE)
    } else {
      saveRDS(data, file = paste0(path, "/", file.name))
    }
  } else if (file.type == ".csv") {                                                 # as .csv
    if (is.null(file.name)) {
      file.name <- deparse(substitute(data))
    }
    parentdir <- paste0(path, "/", file.name)
    message("Writing ", paste(names(data), collapse = ", "), " as .csv to ", parentdir)
    if (dir.exists(parentdir)) {                                                    # don't overwrite parent dir
      warning("'", parentdir, "'", " already exists", call. = FALSE)
    } else {
      dir.create(parentdir)
    }
    for (i in 1:length(data)) {
      id <- names(data)[[i]]
      subdir <- paste0(parentdir, "/", id)
      if (dir.exists(subdir)) {                                                     # don't overwrite subdir
        warning("'", subdir, "'", " already exists", call. = FALSE)
      } else {
        dir.create(subdir)
      }
      for (j in 1:length(data[[i]]$tables)) {
        tblname <- paste0(names(data[[i]]$tables)[j], ".csv")
        if (file.exists(paste0(subdir, "/", tblname))) {
          warning("'", subdir, "/", tblname, "'", " already exists", call. = FALSE) # don't overwrite table
        } else {
          data.table::fwrite(
            data[[i]]$tables[[j]], file = paste0(subdir, "/", tblname), sep = ",")
        }
      }
    }
  }
  
}
