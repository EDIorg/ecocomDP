#' Summarize variable name with function
#'
#' @param tables (list) Data object returned by \code{read_data()[[1]]$tables}
#' @param varfun (named character) Variable to summarize (as name) and function to summarize with (as character; can be: "sum", "mean")
#'
#' @return
#' \item{list}{\code{tables} with \code{names(varfun)} summarized by \code{varfun}}
#' 
#' @note Beware of replicate observations! These may have to be handled separately than by a blanket \code{sum()} or \code{mean()} call.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Read dataset
#' d <- read_data("edi.124.4")
#' # Summarize the "Mass..g.m2." variable by mean()
#' summarize_variable_name(d[[1]]$tables, c(`Mass..g.m2.` = "mean"))
#' }
#' 
summarize_variable_name <- function(tables, varfun) {
  # TODO: Do summarizations propagate to other tables? 
  # TODO: When do heuristics fail?
  # TODO: Are groups properly assigned
  message("Summarizing:\n", paste0(names(varfun), " with ", varfun, "()\n"))
  i <- hasvars(tables, names(varfun))
  tbls_summarized <- lapply(
    tables[i],
    function(tbl) {
      vtbls <- split(tbl, f = as.factor(tbl$variable_name))                     # split table by variable names
      vtbls_summarized <- lapply(                                               # iterate over variable tabes
        vtbls,
        function(vtbl) {
          cols <- names(vtbl)
          func <- unname(varfun[names(varfun) %in% unique(vtbl$variable_name)]) # function to summarize by
          nogrp <- c("observation_id", "observation_ancillary_id",              # cols not to group by
                     "location_ancillary_id", "taxon_ancillary_id", "value")
          pkey <- cols[(cols %in% nogrp) & (cols != "value")]                   # primary key
          grps <- cols[!(cols %in% nogrp)]                                      # cols to group by
          vtbl <- vtbl %>% dplyr::group_by_at(grps)
          if (func == "sum") {                                                  # summarize
            vtbl <- vtbl %>% summarize(value = sum(value, na.rm = TRUE))
          } else if (func == "mean") {
            vtbl <- vtbl %>% summarize(value = mean(value, na.rm = TRUE))
          }
          vtbl[pkey] <- seq_len(nrow(vtbl))                                     # replace primary key
          vtbl <- dplyr::select(vtbl, cols)                                     # reorder
          return(vtbl)
        })
      res <- dplyr::bind_rows(vtbls_summarized)
      return(res)
    })
  tables[names(tables) %in% names(tbls_summarized)] <- tbls_summarized          # assign summarized tables
  return(tables)
}








#' Identify tables with variable names
#'
#' @param tables (list) Data object returned by \code{read_data()[[1]]$tables}
#' @param vnames (character) Variable name
#'
#' @return (logical) TRUE for \code{tables} containing \code{vars}
#' 
#' @note Does not return TRUE for the variable_mapping table.
#' 
hasvars <- function(tables, vname) {
  res <- lapply(
    tables, 
    function(tbl) {
      hascol <- "variable_name" %in% colnames(tbl)
      if (hascol) {
        if ("variable_mapping_id" %in% colnames(tbl)) {
          return(FALSE)
        }
        return(any(vname %in% tbl$variable_name))
      } else {
        return(FALSE)
      }
    })
  return(unlist(res))
}