#' Save ecocomDP data
#'
#' @param data 
#'     (list) Data as a list object created by \code{read_data()}. Name of object will become file name.
#' @param path
#'     (character) Path to the directory in which the data will be written.
#' @param file.name
#'     (character) Name of file. Only for .rds. When file.type = .csv dir name for id and table names receive ecocomDP table names
#' @param file.type
#'     (character) Type of file to save the data to. Options are: ".rda", 
#'     ".csv"
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
save_data <- function(data, path, file.name, file.type) {
  message("Saving data")
  if (file.type == ".rds") {
    file.name <- paste0(file.name, ".rds")
    message("Writing ", file.name)
    saveRDS(data, file = paste0(path, "/", file.name))
  } else if (file.type == ".csv") {
    for (i in 1:length(data)) {
      message("Writing ", names(data)[[i]])
      dirname <- paste0(path, "/", names(data)[[i]])
      dir.create(dirname)
      for (j in 1:length(data[[i]]$tables)) {
        file.name <- paste0(names(data[[i]]$tables)[j], ".csv")
        message("  ", file.name)
        data.table::fwrite(
          data[[i]]$tables[[j]], file = paste0(dirname, "/", file.name))
      }
    }
  }
  
}