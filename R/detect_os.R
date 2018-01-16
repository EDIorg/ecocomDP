#' Detect operating system
#'
#' @description  
#'     This function uses \code{Sys.info} to detect the user's operating system 
#'     and outputs an abbreviated character string to be used as inputs to OS
#'     specific function calls.
#'
#' @usage detect_os()
#' 
#' @return 
#'     \item{win}{Windows OS}
#'     \item{mac}{Mac OS}
#'
#' @export
#'

detect_os <- function(){
  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }
  os
}