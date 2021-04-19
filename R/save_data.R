#' Save ecocomDP data
#'
#' @param data 
#'     (list) Data as a list object created by \code{read_data()}. Name of object will become file name if \code{file.name} is not used
#' @param path
#'     (character) Path to the directory in which the data will be written.
#' @param file.type
#'     (character) Type of file to save the data to. Default is ".rds" but can also be ".csv". Note: metadata and validation_issues are lost when ".csv". 
#' @param file.name
#'     (character) Use this to set the file name (for .rds), or dir name (for .csv) if you'd like to be different than \code{data}.
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
#' d <- read_data("edi.193.3")
#' #' save_data(d, tempdir(), ".rda")
#' save_data(d, tempdir(), ".csv")
#' 
save_data <- function(data, path, file.type = ".rds", file.name = NULL) {
  if (is.null(file.name)) {
    file.name <- deparse(substitute(data))
  }
  if (file.type == ".rds") {
    file.name <- paste0(file.name, ".rds")
    message("Writing ", file.name, " to ", path)
    saveRDS(data, file = paste0(path, "/", file.name))
  } else if (file.type == ".csv") {
    for (i in 1:length(data)) {
      if (is.null(file.name)) {
        dir.name <- names(data)[[i]]
      } else {
        dir.name <- file.name
      }
      message("Writing ", dir.name, " as .csv to ", path)
      dir.create(paste0(path, "/", dir.name))
      for (j in 1:length(data[[i]]$tables)) {
        file.name <- paste0(names(data[[i]]$tables)[j], ".csv")
        data.table::fwrite(
          data[[i]]$tables[[j]], file = paste0(path, "/", dir.name, "/", file.name), sep = ",")
      }
    }
  }
  
}
