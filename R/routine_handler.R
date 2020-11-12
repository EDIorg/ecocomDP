#' Event notification handler
#'
#' @description Checks for unprocessed items in SQlite and pops the longest waiting item into \code{update_L1()} or \code{update_L2_dwca()}.
#'
#' @export
#'
routine_handler <- function(workflow) {
  
  # TODO: This is the function the chron process should call with package.id and environment, which is parsed here to decide whether to call L1 or L2-DwC-A
  
  
}