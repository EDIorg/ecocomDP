#' Save an ecocomDP dataset
#'
#' @param dataset (list) One or more datasets returned by \code{read_data()}. Name of the \code{dataset} object will become the file name if \code{name} is not used.
#' @param path (character) Path to the directory in which \code{dataset} will be written.
#' @param type (character) Type of file to save the \code{dataset} as. Default is ".rds" but can also be ".csv". Note: metadata and validation_issues are lost when using ".csv".
#' @param name (character) An optional argument for setting the saved file name (for .rds), or dir name (for .csv) if you'd like it to be different than \code{dataset}'s object name.
#'     
#' @note Subsequent calls won't overwrite dirs or files
#'
#' @return
#'     \item{.rda}{If \code{type} = ".rda", then an .rda representation of \code{dataset} is returned.}
#'     \item{.csv}{If \code{type} = ".csv", then an set of .csv files are written to a sub-directory of \code{path} named after the data package/product ID.}
#'     
#' @export
#'
#' @examples
#' \dontrun{
#' # Read from source
#' datasets <- c(
#'   read_data("edi.193.3"),
#'   read_data("edi.262.1"),
#'   read_data("edi.359.1"))
#' 
#' # Save as .rds
#' save_data(datasets, "/Users/me/documents/data")
#' 
#' # Save as .csv
#' save_data(datasets, "/Users/me/documents/data", type = ".csv")
#' }
#' 
save_data <- function(dataset, path, type = ".rds", name = NULL) {
  validate_arguments(fun.name = "save_data", fun.args = as.list(environment()))
  if (is.null(name)) {
    name <- deparse(substitute(dataset))
  }
  if (type == ".rds") {                                                        # as .rds
    name <- paste0(name, ".rds")
    message("Writing ", name, " to ", path)
    if (file.exists(paste0(path, "/", name))) {                                # don't overwrite .rds file
      warning("'", paste0(path, "/", name), "'", " already exists", call. = FALSE)
    } else {
      saveRDS(dataset, file = paste0(path, "/", name))
    }
  } else if (type == ".csv") {                                                 # as .csv
    if (is.null(name)) {
      name <- deparse(substitute(dataset))
    }
    parentdir <- paste0(path, "/", name)
    message("Writing ", paste(names(dataset), collapse = ", "), " as .csv to ", parentdir)
    if (dir.exists(parentdir)) {                                                    # don't overwrite parent dir
      warning("'", parentdir, "'", " already exists", call. = FALSE)
    } else {
      dir.create(parentdir)
    }
    for (i in 1:length(dataset)) {
      id <- names(dataset)[[i]]
      subdir <- paste0(parentdir, "/", id)
      if (dir.exists(subdir)) {                                                     # don't overwrite subdir
        warning("'", subdir, "'", " already exists", call. = FALSE)
      } else {
        dir.create(subdir)
      }
      for (j in 1:length(dataset[[i]]$tables)) {
        tblname <- paste0(names(dataset[[i]]$tables)[j], ".csv")
        if (file.exists(paste0(subdir, "/", tblname))) {
          warning("'", subdir, "/", tblname, "'", " already exists", call. = FALSE) # don't overwrite table
        } else {
          data.table::fwrite(
            dataset[[i]]$tables[[j]], file = paste0(subdir, "/", tblname), sep = ",")
        }
      }
    }
  }
  
}
