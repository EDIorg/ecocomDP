# Get taxonomic authority
#
# @description
#     Use fuzzy searching in the Global Names Resolver to correct spelling
#     and locate appropriate authorities.
#
# @param taxon
#     A character string representation of the taxon to search on.
# @param data.source
#     A numeric ID corresponding to the data source (i.e. taxonomic authority)
#     you'd like to query. Run `view_authorities` to get valid data source
#     options and ID's.
#
# @return
#     \itemize{
#         \item resolved_name - Resolved taxon name.
#         \item authority - Name of the authority searched.
#         \item score - Relative match score provided by the authority.
#     }
#
txcl_get_authority <- function(taxon, data.source){
  
  # Tell user what authority is being queried
  gnr_list <- txcl_load_gnr_datasources()
  use_i <- gnr_list[ , 'id'] == data.source
  message('Searching ', gnr_list[use_i, 'title'],' for "',taxon,'"')
  
  # User Global Names Resolver to see if the taxon can be found in data.source
  resp <- suppressWarnings(
    taxize::gnr_resolve(sci = taxon,
                        data_source_ids = as.character(data.source),
                        resolve_once = TRUE,
                        canonical = TRUE,
                        best_match_only = TRUE))
  # Parse response and return and first item found
  if (nrow(resp) != 0) {
    res <- list(resolved_name = resp$matched_name2[1],
                authority = gnr_list$title[use_i],
                score = resp$score[1])
    return(res)
  } else {
    res <- list(resolved_name =  NA_character_,
                authority = NA_character_,
                score = NA_character_)
    return(res)
  }
}








# Get the taxonomic classification hierarchy for taxa resolved to supported authorities
#
# @param taxa.clean
#     (character) Taxa names
# @param authority
#     (character) Authority \code{taxa.clean} have been resolved to, otherwise \code{NA}. Supported authorities include: "ITIS", "WORMS", "GBIF".
# @param authority.id
#     (character) ID of \code{taxa.clean} within the \code{authority}, otherwise \code{NA}
# @param rank
#     (character) Rank (e.g. "Genus", "Species") of \code{taxa.clean}, otherwise \code{NA}. This is useful when \code{taxa.clean} can't be resolved to an \code{authority} and the rank must be manually defined.
# @param path
#     (character) Path of the directory containing taxa_map.csv.
#
# @return
#     (list) For \code{taxa.clean} resolved to a supported authority, each item in the list is a classification hierarchy (also a list), including one or more common names 
#     (only when \code{authority} is ITIS or WORMS) and authority IDs for each rank-value pair. For \code{taxa.clean} not resolved to a supported authority, each item is listed as defined in
#      the \code{taxa.clean}, \code{authority}, and \code{authority.id} arguments.
#
# @details
#     Only taxa resolved to supported authorities can be expanded into a full taxonomic classification with common names. Taxa resolved to unsupported authorities, or not resolved at all, will be listed as 
#     is defined in the \code{taxa.clean}, \code{authority}, and \code{authority.id} arguments.
#
#     Supported authorities are recognized by a controlled set of representations.
#     \itemize{
#     \item ITIS can be: "ITIS", "itis", "Integrated Taxonomic Information System", or "https://www.itis.gov/".
#     \item WORMS can be: "WORMS", "worms", "World Register of Marine Species", or "https://www.marinespecies.org/".
#     \item GBIF can be: "GBIF", "gbif", "GBIF Backbone Taxonomy", or "https://gbif.org".
#     }
#
txcl_get_classification <- function(taxa.clean,
                               authority = NA,
                               authority.id = NA,
                               rank = NA,
                               path = NULL) {
  
  # Parameterize --------------------------------------------------------------
  
  cw <- data.frame(
    human.readable = c(
      'Catalogue of Life',
      'ITIS',
      'Integrated Taxonomic Information System',
      'https://www.itis.gov/',
      'itis',
      'World Register of Marine Species',
      'WORMS',
      'https://www.marinespecies.org/',
      'http://www.marinespecies.org/',
      'worms',
      'GBIF Backbone Taxonomy',
      'GBIF',
      'gbif',
      'https://gbif.org',
      'Tropicos - Missouri Botanical Garden'),
    machine.readable = c(
      'col',
      'itis',
      'itis',
      'itis',
      'itis',
      'worms',
      'worms',
      'worms',
      'worms',
      'worms',
      'gbif',
      'gbif',
      'gbif',
      'gbif',
      'tropicos'),
    stringsAsFactors = F)
  
  supported <- unique.data.frame(
    data.frame(
      taxa.clean = taxa.clean[authority %in% cw$human.readable],
      authority = cw$machine.readable[
        match(authority[authority %in% cw$human.readable], cw$human.readable)],
      authority.id = authority.id[authority %in% cw$human.readable],
      rank = rank[authority %in% cw$human.readable],
      stringsAsFactors = FALSE))
  
  unsupported <- unique.data.frame(
    data.frame(
      taxa.clean = taxa.clean[!(authority %in% cw$human.readable)],
      authority = authority[!(authority %in% cw$human.readable)],
      authority.id = authority.id[!(authority %in% cw$human.readable)],
      rank = rank[!(authority %in% cw$human.readable)],
      stringsAsFactors = FALSE))
  
  # Supported authorities -----------------------------------------------------
  
  if (nrow(supported) != 0) {
    
    message("Retrieving classifications")
    classifications <- suppressMessages(
      unname(
        mapply(
          taxize::classification,
          sci_id = supported$authority.id,
          db = supported$authority)))
    
    # Move "false supported" to "unsupported" so taxa won't be lost from the
    # returned object. This can happen when the authority + ID pair doesn't
    # resolve due to inaccuracies
    
    i <- is.na(classifications)
    
    unsupported <- rbind(
      unsupported,
      data.frame(
        taxa.clean = supported$taxa.clean[i],
        authority = supported$authority[i],
        authority.id = supported$authority.id[i],
        rank = supported$rank[i],
        stringsAsFactors = FALSE))
    
    supported <- supported[!i, ]
    classifications <- classifications[!i]
    
    # Get all common names and restructure for annotation. Add the authority
    # system identifier and common names (there may be more than one) to each
    # name + rank pair. This will be used by txcl_make_taxonomicCoverage() to
    # annotate the output EML metadata.
    
    output_supported <- mapply(
      FUN = function(classification, authority) {
        
        restructured_classification <- apply(
          classification,
          1,
          function(row) {
            
            # Set Authority system identifier (i.e. provider)
            if (authority == "itis") {
              provider <- "https://itis.gov"
            } else if (authority == "worms") {
              provider <- "https://marinespecies.org"
            } else if (authority == "gbif") {
              provider <- "https://gbif.org"
            }  else {
              provider <- NA_character_
            }
            
            # Get common name(s). Only English language is supported (currently).
            # GBIF doesn't support common name fetching (currently).
            if (authority == "itis") {
              names_common <- tryCatch({
                r <- ritis::common_names(row[["id"]])
                suppressWarnings(
                  r$commonName[r$language == "English"])
              }, error = function(e) {
                NULL
              })
            } else if (authority == "worms") {
              names_common <- tryCatch({
                r <- worrms::wm_common_id(as.numeric(row[["id"]]))
                suppressWarnings(
                  r$vernacular[r$language == "English"])
              }, error = function(e) {
                NULL
              })
            } else {
              names_common <- NULL
            }
            
            # Restructure
            classification <- list(
              taxonRankName = row[["rank"]],
              taxonRankValue = row[["name"]],
              commonName = as.list(names_common),
              taxonId = list(
                provider = provider,
                taxonId = trimws(row[["id"]])))
            
            return(classification)
            
          })
        
        return(restructured_classification)
        
      },
      classification = classifications,
      authority = supported$authority,
      SIMPLIFY = FALSE)
    
    output_supported <- unname(output_supported)
    
  } else {
    output_supported <- NULL
  }
  
  # Unsupported authorities ---------------------------------------------------
  
  if (nrow(unsupported) != 0) {
    
    classifications <- unname(
      split(
        data.frame(
          name = unsupported$taxa.clean,
          rank = unsupported$rank,
          id = unsupported$authority.id,
          stringsAsFactors = FALSE),
        seq(nrow(unsupported))))
    
    # List names and ranks "as is" and restructure for annotation. Add the
    # authority system identifier to each name + rank pair. This will be used
    # by txcl_make_taxonomicCoverage() to annotate the output EML metadata.
    
    output_unsupported <- mapply(
      FUN = function(classification, authority) {
        
        restructured_classification <- apply(
          classification,
          1,
          function(row) {
            
            # Set Authority system identifier (i.e. provider)
            if (!is.na(authority) | ("" %in% authority)) {
              provider <- authority
            } else {
              provider <- NA_character_
            }
            
            # Restructure
            classification <- list(
              taxonRankName = row[["rank"]],
              taxonRankValue = row[["name"]],
              taxonId = list(
                provider = provider,
                taxonId = trimws(row[["id"]])))
            
            return(classification)
            
          })
        
        return(restructured_classification)
        
      },
      classification = classifications,
      authority = unsupported$authority,
      SIMPLIFY = FALSE)
    
  } else {
    output_unsupported <- NULL
  }
  
  # Combine -------------------------------------------------------------------
  
  return(c(output_supported, output_unsupported))
  
}








# Get taxonomic identifiers
#
# @description
#     Get a taxonomic identifier for a taxon name and corresponding authority.
#
# @param taxon
#     A character string specifying taxon to get the ID for.
# @param authority
#     A character string specifying the authority from which to get the ID.
#
# @return
#     \itemize{
#         \item taxon_id - An authority ID for the taxon.
#         \item rank - The taxonomic rank of the taxon.
#     }
#
txcl_get_id <- function(taxon, authority){
  
  taxon_id <- NA_character_
  taxon_rank <- NA_character_
  
  # Get ID and rank from taxon and authority ----------------------------------
  
  # Get authority and query for ID and rank
  
  # # Catalogue of Life
  # if ((!is.na(authority)) & (authority == 'Catalogue of Life')){
  #   response <- taxize::get_ids_(
  #     taxon,
  #     'col'
  #   )
  #   if (nrow(response[[1]][[1]]) > 0){
  #     response <- as.data.frame(response$col)
  #     use_i <- response[ , 2] == taxon
  #     response <- response[use_i, ]
  #     if (nrow(response) > 0){
  #       taxon_id <- as.character(response[1, 1])
  #       taxon_rank <- response[1, 3]
  #     } else {
  #       taxon_id <- NA_character_
  #       taxon_rank <- NA_character_
  #     }
  #   } else {
  #     taxon_id <- NA_character_
  #     taxon_rank <- NA_character_
  #   }
  # }
  
  # ITIS
  if ((!is.na(authority)) & (authority == 'ITIS')) {
    response <- as.data.frame(
      taxize::itis_terms(taxon))
    if (nrow(response) > 0) {
      use_i <- response[ , 'scientificName'] == taxon
      response <- response[use_i, ]
      if (nrow(response) > 0) {
        taxon_id <- as.character(response[1, 'tsn'])
        taxon_rank <- taxize::itis_taxrank(as.numeric(taxon_id))
      } else {
        taxon_id <- NA_character_
        taxon_rank <- NA_character_
      }
    } else {
      taxon_id <- NA_character_
      taxon_rank <- NA_character_
    }
  }
  
  # World Register of Marine Species
  if ((!is.na(authority)) & (authority == 'World Register of Marine Species')){
    response <- taxize::get_wormsid_(
      taxon,
      searchtype = 'scientific',
      accepted = F,
      ask = F,
      messages = F)
    if (!is.null(response[[1]])) {
      response <- as.data.frame(response[[1]])
      use_i <- response[ , 2] == taxon
      response <- response[use_i, ]
      if (nrow(response) > 0) {
        taxon_id <- as.character(response[ , 'AphiaID'])
        response <- taxize::classification(taxon_id, db = 'worms')
        response <- as.data.frame(response[[1]])
        taxon_rank <- response[nrow(response), 2]
      } else {
        taxon_id <- NA_character_
        taxon_rank <- NA_character_
      }
    } else {
      taxon_id <- NA_character_
      taxon_rank <- NA_character_
    }
  }
  
  # GBIF Backbone Taxonomy
  if ((!is.na(authority)) & (authority == 'GBIF Backbone Taxonomy')){
    
    response <- taxize::get_ids_(
      taxon,
      'gbif'
    )
    if (nrow(response[[1]][[1]]) > 0){
      response <- as.data.frame(response[[1]][[1]])
      use_i <- response[ , 6] == taxon
      response <- response[use_i, ]
      if (nrow(response) > 0){
        taxon_id <- as.character(response[1, 'usagekey'])
        response <- taxize::classification(taxon_id, db = 'gbif')
        response <- as.data.frame(response[[1]])
        taxon_rank <- response[nrow(response), 2]
      } else {
        taxon_id <- NA_character_
        taxon_rank <- NA_character_
      }
    } else {
      taxon_id <- NA_character_
      taxon_rank <- NA_character_
    }
  }
  
  # Tropicos - Missouri Botanical Garden
  if ((!is.na(authority)) & (authority == 'Tropicos - Missouri Botanical Garden')){
    response <- taxize::get_ids_(
      taxon,
      'tropicos'
    )
    if (nrow(response[[1]][[1]]) > 0){
      response <- as.data.frame(response[[1]][[1]])
      use_i <- response[ , 2] == taxon
      response <- response[use_i, ]
      if (nrow(response) > 0){
        taxon_id <- as.character(response[1, 'nameid'])
        response <- taxize::tax_rank(taxon_id, db = 'tropicos')
        taxon_rank <- response[[1]]
      } else {
        taxon_id <- NA_character_
        taxon_rank <- NA_character_
      }
    } else {
      taxon_id <- NA_character_
      taxon_rank <- NA_character_
    }
  }
  
  # Return --------------------------------------------------------------------
  
  if (!exists('taxon_id')){
    taxon_id <- NA_character_
  }
  if (!exists('taxon_rank')){
    taxon_rank <- NA_character_
  }
  if (is.null(taxon_id)){
    taxon_id <- NA_character_
  }
  if (is.null(taxon_rank)){
    taxon_rank <- NA_character_
  }
  
  list('taxon_id' = taxon_id,
       'taxon_rank' = taxon_rank)
  
}








# Get taxonomic identifiers
#
# @description
#     Get a taxonomic identifier for a taxon name and corresponding authority.
#
# @param taxon
#     A character string specifying taxon to get the ID for.
# @param authority
#     A character string specifying the authority from which to get the ID.
#
# @return
#     \itemize{
#         \item taxon_id - An authority ID for the taxon.
#         \item rank - The taxonomic rank of the taxon.
#     }
#
txcl_get_id_common <- function(taxon, authority){
  
  taxon_id <- NA_character_
  taxon_rank <- NA_character_
  taxon_authority <- NA_character_
  taxon_clean <- NA_character_
  
  # Match authority -----------------------------------------------------------
  
  gnr_list <- txcl_load_gnr_datasources()
  use_i <- authority == gnr_list[ , 'id']
  authority <- gnr_list[use_i, 'title']
  
  # Get ID and rank from taxon and authority ----------------------------------
  
  # Get authority and query for ID and rank
  
  # # ITIS
  if ((!is.na(authority)) & (authority == 'ITIS')){
    response <- as.data.frame(
      taxize::get_tsn_(
        sci_com = taxon,
        searchtype = 'common',
        ask = FALSE))
    if (nrow(response) > 0){
      use_i <- tolower(response[ , 3]) == tolower(taxon)
      response <- response[use_i, ]
      if (nrow(response) > 0){
        taxon_id <- as.character(response[1, 1])
        taxon_rank <- 'Common'
        taxon_authority <- authority
        taxon_clean <- taxon
      } else {
        taxon_id <- NA_character_
        taxon_rank <- NA_character_
        taxon_authority <- NA_character_
        taxon_clean <- NA_character_
      }
    } else {
      taxon_id <- NA_character_
      taxon_rank <- NA_character_
      taxon_authority <- NA_character_
      taxon_clean <- NA_character_
    }
  }
  
  # # World Register of Marine Species
  # if ((!is.na(authority)) & (authority == 'World Register of Marine Species')){
  #   response <- as.data.frame(
  #     get_wormsid_(
  #       query = taxon,
  #       searchtype = 'common',
  #       ask = F
  #       )
  #     )
  #   if (!is.null(response[[1]])){
  #     response <- as.data.frame(response[[1]])
  #     use_i <- response[ , 2] == taxon
  #     response <- response[use_i, ]
  #     if (nrow(response) > 0){
  #       taxon_id <- as.character(response[ , 'AphiaID'])
  #       response <- taxize::classification(taxon_id, db = 'worms')
  #       response <- as.data.frame(response[[1]])
  #       taxon_rank <- response[nrow(response), 2]
  #     } else {
  #       taxon_id <- NA_character_
  #       taxon_rank <- NA_character_
  #     }
  #   } else {
  #     taxon_id <- NA_character_
  #     taxon_rank <- NA_character_
  #   }
  # }
  
  # # Tropicos - Missouri Botanical Garden
  # if ((!is.na(authority)) & (authority == 'Tropicos - Missouri Botanical Garden')){
  #     response <- as.data.frame(
  #       tp_search(
  #         commonname = taxon
  #       )
  #       )
  #   if (nrow(response[[1]][[1]]) > 0){
  #     response <- as.data.frame(response[[1]][[1]])
  #     use_i <- response[ , 2] == taxon
  #     response <- response[use_i, ]
  #     if (nrow(response) > 0){
  #       taxon_id <- as.character(response[1, 'nameid'])
  #       response <- taxize::tax_rank(taxon_id, db = 'tropicos')
  #       taxon_rank <- response[[1]]
  #     } else {
  #       taxon_id <- NA_character_
  #       taxon_rank <- NA_character_
  #     }
  #   } else {
  #     taxon_id <- NA_character_
  #     taxon_rank <- NA_character_
  #   }
  # }
  
  # # Encyclopedia of life
  # if ((!is.na(authority)) & (authority == 'EOL')){
  #       response <- suppressMessages(as.data.frame(
  #         taxize::eol_search(
  #           terms = taxon,
  #           exact = T
  #           )
  #         ))
  #   if (nrow(response) > 0){
  #     if (nrow(response) > 0){
  #       taxon_id <- as.character(response[1, 'pageid'])
  #       taxon_rank <- 'common'
  #       taxon_authority <- authority
  #       taxon_clean <- taxon
  #     } else {
  #       taxon_id <- NA_character_
  #       taxon_rank <- NA_character_
  #       taxon_authority <- NA_character_
  #       taxon_clean <- NA_character_
  #     }
  #   } else {
  #     taxon_id <- NA_character_
  #     taxon_rank <- NA_character_
  #     taxon_authority <- NA_character_
  #     taxon_clean <- NA_character_
  #   }
  # }
  
  # Return --------------------------------------------------------------------
  
  if (!exists('taxon_id')){
    taxon_id <- NA_character_
  }
  if (!exists('taxon_rank')){
    taxon_rank <- NA_character_
  }
  if (!exists('taxon_authority')){
    taxon_id <- NA_character_
  }
  if (!exists('taxon_clean')){
    taxon_rank <- NA_character_
  }
  if (is.null(taxon_id)){
    taxon_id <- NA_character_
  }
  if (is.null(taxon_rank)){
    taxon_rank <- NA_character_
  }
  if (is.null(taxon_authority)){
    taxon_authority <- NA_character_
  }
  if (is.null(taxon_clean)){
    taxon_clean <- NA_character_
  }
  
  list('taxon_id' = taxon_id,
       'taxon_rank' = taxon_rank,
       'taxon_authority' = taxon_authority,
       'taxon_clean' = taxon_clean)
  
}








# Load and fix GNR Datasources
#
# @return (data.frame) GNR datasources from \code{taxize::gnr_datasources()}
#
# @details This fixes bugs in taxize which otherwise produce inconsistent datasource names (e.g. "Integrated Taxonomic Information SystemITIS" rather than expected "ITIS")
#
txcl_load_gnr_datasources <- function() {
  gnr_list <- as.data.frame(taxize::gnr_datasources())
  gnr_list$title[gnr_list$id == "3"] <- "ITIS"
  return(gnr_list)
}








# Make taxonomicCoverage EML node
#
# @param taxa.clean
#     (character) Taxa names as they appear in your dataset
# @param authority
#     (character) Authority \code{taxa.clean} have been resolved to. Supported authorities include: "ITIS", "WORMS", "GBIF". For unsupported authorities, list the home page URL. For unresolved taxa use \code{NA}.
# @param authority.id
#     (character) ID of \code{taxa.clean} within the \code{authority}, otherwise \code{NA}
# @param rank
#     (character) Rank (e.g. "Genus", "Species") of \code{taxa.clean}, otherwise \code{NA}. This is useful when \code{taxa.clean} can't be resolved to an \code{authority} and the rank must be manually defined.
# @param path
#     (character) Path of the directory to which taxonomicCoverage.xml will be written. Can also be the path of the directory containing taxa_map.csv, if using as inputs to this function.
# @param write.file
#     (logical) Whether taxonomicCoverage.xml should be written to file. Default is \code{TRUE}.
#
# @return
# \item{emld list}{The taxonomicClassification EML node for use in constructing EML metadata with the EML R library.}
# \item{taxonomicCoverage.xml}{If \code{write.file = TRUE}.}
#
# @details This function uses \code{txcl_get_classification()} to expand taxa, resolved to supported authorities, into full taxonomic classification. Each level of classification is
#  accompanied by an annotation (listing the \code{authority} and \code{authority.id}) and common names (only when \code{authority} is ITIS or WORMS). Taxa resolved to unsupported authorities, or not resolved at 
#  all, will be listed as is defined in the \code{taxa.clean}, \code{authority}, and \code{authority.id} arguments.
#
# @note The name of this function is a bit misleading. The return value is actually a list of taxonomicClassification nodes, which occur immediately below taxonomicCoverage 
# (i.e. ../taxonomicCoverage/taxonomicClassification).
#
txcl_make_taxonomicCoverage <- function(
  taxa.clean,
  authority = NA,
  authority.id = NA,
  rank = NA,
  path,
  write.file = TRUE){
  
  message('Creating <taxonomicCoverage>')
  
  # Not all taxonomic authority systems are supported by this function.
  # Testing and reporting of supported authorities in the function
  # documentation is needed. Additionally, the valid inputs to authority and
  # authority ID need to be definied in the function documentation so users can
  # manually supply this information if necessary.
  
  # Validate arguments --------------------------------------------------------
  
  # A path is required when writing to file
  if (missing(path) & isTRUE(write.file)){
    stop('Input argument "path" is required when writing data to file.')
  }
  
  # The path must be valid
  if (!missing(path)){
    validate_path(path)
  }
  
  # Load data -----------------------------------------------------------------
  
  # Remove any blank or missing taxa otherwise txcl_get_classification() will throw
  # errors
  missing_names <- is.na(taxa.clean) | taxa.clean == ""
  taxa.clean <- taxa.clean[!missing_names]
  authority <- authority[!missing_names]
  authority.id <- authority.id[!missing_names]
  rank <- rank[!missing_names]
  
  # Create taxonomicCoverage --------------------------------------------------
  
  # This method supports EML annotation and more than one common name per taxon
  # rank and thus requires a nested list structure not currently supported by
  # EML::set_taxonomicCoverage().
  
  # Retrieve taxonomic hierarchy and common names when possible
  classifications <- txcl_get_classification(
    taxa.clean = taxa.clean,
    authority = authority,
    authority.id = authority.id,
    rank = rank,
    path = path)
  
  # Recursively convert classifications into the nested structure expected by
  # EML::write_eml().
  taxonomic_coverage <- txcl_set_taxonomic_coverage(classifications)
  
  # Write to file -------------------------------------------------------------
  
  if (isTRUE(write.file)){
    message('Writing taxonomicCoverage.xml')
    emld::eml_version("eml-2.2.0")
    EML::write_eml(
      eml = taxonomic_coverage,
      file = paste0(path, '/taxonomicCoverage.xml'))
  }
  
  # Return object -------------------------------------------------------------
  
  message('Done.')
  return(taxonomic_coverage)
  
}








# Optimize match
#
# @description
#     Optimize the taxon match to an authority based on completeness of
#     returned information. A complete return contains both an authority
#     name and an authority ID for a taxon.
#
# @param x
#     (character) A character string specifying the taxon.
# @param data.sources
#     (numeric) A numeric vector of values specifying the authorities to search across.
#     Run `view_authorities` to get valid data source options and ID's.
#
# @return
#     \itemize{
#         \item taxa_clean - Resolved name for input taxon.
#         \item rank - Rank of the input taxon.
#         \item authority - Best authority match for input taxon.
#         \item authority_id - Corresponding authority ID for the input taxon.
#         \item score - Authority match score for input taxon.
#     }
#
txcl_optimize_match <- function(x, data.sources){
  
  # Initialize output
  output <- data.frame(
    taxa_clean = rep(NA_character_, length(data.sources)),
    rank = rep(NA_character_, length(data.sources)),
    authority = rep(NA_character_, length(data.sources)),
    authority_id = rep(NA_character_, length(data.sources)),
    score = rep(NA_character_, length(data.sources)),
    stringsAsFactors = FALSE)
  
  # Iterate overall sources and stop on the first match
  j <- 1
  while (j != (length(data.sources)+1)) {
    
    # Does taxon resolve to data.sources[j]?
    gnrr <- try(
      txcl_get_authority(taxon = x,
                         data.source = as.character(data.sources[j])),
      silent = TRUE)
    
    # Try for ID and rank if GNR didn't error, then add results
    if (!("try-error" %in% class(gnrr))) {
      id <- try(
        suppressWarnings(
          txcl_get_id(taxon = x, authority = gnrr$authority)),
        silent = TRUE)
      if (!("try-error" %in% class(id))) {
        output$authority_id[j] <- id$taxon_id
        output$rank[j] <- id$taxon_rank
        output$taxa_clean[j] <- x
        output$authority[j] <- gnrr$authority
        output$score[j] <- gnrr$score
      }
    }
    
    # Continue with next data.source if no results were found in this one
    if (!is.na(output$authority_id[j])) {
      j <- length(data.sources) + 1
    } else {
      j <- j + 1
    }
  }
  
  # Return the first match otherwise NAs
  resolved <- !is.na(output$authority_id)
  if (any(resolved)) {
    output <- output[resolved, ]
  } else {
    output <- output[1, ]
  }
  return(output)
  
}








# Optimize match common
#
# @description
#     Optimize the common taxon match to an authority based on completeness of
#     returned information. A complete return contains both an authority
#     name and an authority ID for a taxon.
#
# @param x
#     (character) A character string specifying the taxon.
# @param data.sources
#     (Numeric) A numeric vector of values specifying the authorities to search across.
#     Run `view_authorities` to get valid data source options and ID's.
#
# @return
#     \itemize{
#         \item taxa_clean - Resolved name for input taxon.
#         \item rank - Rank of the input taxon.
#         \item authority - Best authority match for input taxon.
#         \item authority_id - Corresponding authority ID for the input taxon.
#         \item score - Authority match score for input taxon.
#
#     }
#
txcl_optimize_match_common <- function(x, data.sources){
  
  output <- data.frame(
    taxa_clean = rep(NA_character_, length(data.sources)),
    rank = rep(NA_character_, length(data.sources)),
    authority = rep(NA_character_, length(data.sources)),
    authority_id = rep(NA_character_, length(data.sources)),
    stringsAsFactors = F)
  
  j <- 1
  
  while (j != (length(data.sources)+1)){
    
    # Resolve ID, and rank
    
    out_id <- try(
      suppressWarnings(
        txcl_get_id_common(
          taxon = x,
          authority = data.sources[j])),
      silent = TRUE)
    
    if (methods::is(out_id, "try-error")) {
      out_id <- list(
        'taxon_id' = NA_character_,
        'taxon_rank' = NA_character_,
        'taxon_authority' = NA_character_,
        'taxon_clean' = NA_character_)
    }
    
    # Parse results into output data frame
    
    output[j, 'taxa_clean'] <- out_id[['taxon_clean']]
    output[j, 'rank'] <- out_id[['taxon_rank']]
    output[j, 'authority'] <- out_id[['taxon_authority']]
    output[j, 'authority_id'] <- out_id[['taxon_id']]
    
    # Stop if a successful match has been made to save redundant effort
    
    if (!is.na(output[j, 'authority_id'])) {
      j <- length(data.sources) + 1
    } else {
      j <- j + 1
    }
    
  }
  
  # Get best match
  
  if (sum(is.na(output[ , 'authority_id'])) == nrow(output)){
    if (sum(is.na(output[ , 'authority'])) != nrow(output)){
      output <- output[!is.na(output[ , 'authority']), ]
      output <- output[1, ]
    } else {
      output <- output[1, ]
    }
  } else {
    output <- output[!is.na(output[ , 'authority_id']), ]
  }
  
  # Return
  
  list(
    output[1, 'taxa_clean'],
    output[1, 'rank'],
    output[1, 'authority'],
    output[1, 'authority_id']
  )
  
}










# Resolve common names to an authority
#
# @description
#     Resolve taxa to preferred authorities and get associated IDs.
#
# @param x
#     (character) A vector of taxa names.
# @param data.sources
#     (numeric) An ordered vector of authority IDs to be queried (
#     \code{view_taxa_authorities()} lists currently supported authorities).
#     Taxa are resolved to authorities in the order listed. If an authority
#     and ID match cannot be made, then the next will be queried.
# @param path
#     (character) Path to directory containing taxa_map.csv. This tracks
#     provenance throughout the data cleaning process. Create this file with
#     \code{create_taxa_map()}.
#
# @return
#     (data frame; taxa_map.csv) If using \code{x}, then a data frame
#     containing the input taxa, accepted taxa name, rank, authority,
#     authority ID, and score are returned. If using \code{path}, then an
#     updated version of taxa_map.csv will be returned to \code{path} and
#     a data frame of taxa_map.csv to the R environment.
#
txcl_resolve_comm_taxa <- function(x = NULL, data.sources, path = NULL){
  
  # Check arguments ---------------------------------------------------------
  
  if (!is.null(x) & !is.null(path)) {
    stop('Both "path" and "x" arguments are not allowed. Select one or the other.')
  }
  
  if (is.null(x)) {
    if (is.null(path)) {
      stop('Input argument "path" is missing!')
    }
    use_i <- file.exists(
      paste0(
        path,
        '/taxa_map.csv'
      )
    )
    if (!isTRUE(use_i)) {
      stop('taxa_map.csv is missing! Create it with initialize_taxa_map.R.')
    }
  }
  
  if (missing(data.sources)) {
    stop('Input argument "data.sources" is missing!')
  }
  
  authorities <- view_taxa_authorities()
  authorities <- authorities[authorities$resolve_comm_taxa == 'supported', ]
  use_i <- as.character(data.sources) %in% as.character(authorities$id)
  if (sum(use_i) != length(use_i)){
    stop('Input argument "data.sources" contains unsupported data source IDs!')
  }
  
  # Create taxa list ----------------------------------------------------------
  
  if (!is.null(path)) {
    
    taxa_list <- data.frame(
      index = seq(nrow(taxa_map)),
      taxa = rep(NA_character_, nrow(taxa_map)),
      stringsAsFactors = F)
    taxa_list$taxa <- taxa_map$taxa_raw
    use_i <- !is.na(taxa_map$taxa_trimmed)
    taxa_list$taxa[use_i] <- taxa_map$taxa_trimmed[use_i]
    use_i <- !is.na(taxa_map$taxa_replacement)
    taxa_list$taxa[use_i] <- taxa_map$taxa_replacement[use_i]
    use_i <- is.na(taxa_map$taxa_clean)
    taxa_list$taxa[!use_i] <- NA
    use_i <- is.na(taxa_map$authority_id) & is.na(taxa_map$taxa_removed)
    taxa_list <- taxa_list[use_i, ]
    
  } else {
    
    taxa_list <- data.frame(
      index = seq(length(x)),
      taxa = rep(NA_character_, length(x)),
      stringsAsFactors = F)
    taxa_list$taxa <- x
    
  }
  
  # Optimize match ------------------------------------------------------------
  
  r <- lapply(
    unique(taxa_list$taxa),
    txcl_optimize_match_common,
    data.sources = data.sources)
  
  # Update taxa_map.csv -------------------------------------------------------
  
  if (!is.null(path)) {
    
    r <- data.frame(
      matrix(
        unlist(r),
        nrow = length(r),
        byrow = T),
      stringsAsFactors = F)
    colnames(r) <- c(
      'taxa_clean',
      'rank',
      'authority',
      'authority_id')
    r$taxa <- unique(taxa_list$taxa)
    rj <- dplyr::full_join(r, taxa_list, by = "taxa")
    taxa_map[rj$index, c("taxa_clean", "rank", "authority", "authority_id", "score")] <-
      dplyr::select(rj, -taxa, -index)
    taxa_map$rank <- stringr::str_to_title(taxa_map$rank)
    
  } else {
    
    r <- data.frame(
      matrix(
        unlist(r),
        nrow = length(r),
        byrow = T),
      stringsAsFactors = F)
    colnames(r) <- c(
      'taxa_clean',
      'rank',
      'authority',
      'authority_id')
    taxa_map <- cbind(taxa_list, r)
    
  }
  
  # Return --------------------------------------------------------------------
  
  taxa_map
  
}









# Resolve scientific names to an authority
#
# @description
#     Resolve taxa to preferred authorities and get associated IDs.
#
# @param x
#     (character) A vector of taxa names.
# @param data.sources
#     (numeric) An ordered vector of authority IDs to be queried (
#     \code{view_taxa_authorities()} lists currently supported authorities).
#     Taxa are resolved to authorities in the order listed. If an authority
#     and ID match cannot be made, then the next will be queried.
# @param path
#     (character) Path to directory containing taxa_map.csv. This tracks
#     provenance throughout the data cleaning process. Create this file with
#     \code{create_taxa_map()}.
#
# @return
#     (data frame; taxa_map.csv) If using \code{x}, then a data frame
#     containing the input taxa, accepted taxa name, rank, authority,
#     authority ID, and score are returned. If using \code{path}, then an
#     updated version of taxa_map.csv will be returned to \code{path} and
#     a data frame of taxa_map.csv to the R environment.
#
txcl_resolve_sci_taxa <- function(x = NULL, data.sources, path = NULL){
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments("txcl_resolve_sci_taxa", as.list(environment()))
  
  # Create taxa list ----------------------------------------------------------
  
  if (!is.null(path)) {
    taxa_list <- data.frame(
      index = seq(nrow(taxa_map)),
      taxa = rep(NA_character_, nrow(taxa_map)),
      stringsAsFactors = F)
    taxa_list$taxa <- taxa_map$taxa_raw
    use_i <- !is.na(taxa_map$taxa_trimmed)
    taxa_list$taxa[use_i] <- taxa_map$taxa_trimmed[use_i]
    use_i <- !is.na(taxa_map$taxa_replacement)
    taxa_list$taxa[use_i] <- taxa_map$taxa_replacement[use_i]
    use_i <- !is.na(taxa_map$taxa_removed) | !is.na(taxa_map$authority_id)
    taxa_list <- taxa_list[!use_i, ]
  } else {
    taxa_list <- data.frame(
      index = seq(length(x)),
      taxa = rep(NA_character_, length(x)),
      stringsAsFactors = F)
    taxa_list$taxa <- x
  }
  
  # Optimize match ------------------------------------------------------------
  
  r <- lapply(
    unique(taxa_list$taxa),
    txcl_optimize_match,
    data.sources = data.sources)
  
  # Update taxa_map.csv -----------------------------------------------------
  
  if (!is.null(path)) {
    r <- data.frame(
      matrix(
        unlist(r),
        nrow = length(r),
        byrow = T),
      stringsAsFactors = F)
    colnames(r) <- c(
      'taxa_clean',
      'rank',
      'authority',
      'authority_id',
      'score')
    r$taxa <- unique(taxa_list$taxa)
    rj <- dplyr::full_join(r, taxa_list, by = "taxa")
    taxa_map[rj$index, c("taxa_clean", "rank", "authority", "authority_id", "score")] <-
      dplyr::select(rj, -taxa, -index)
    taxa_map$rank <- stringr::str_to_title(taxa_map$rank)
  } else {
    r <- data.frame(
      matrix(
        unlist(r),
        nrow = length(r),
        byrow = T),
      stringsAsFactors = F)
    colnames(r) <- c(
      'taxa_clean',
      'rank',
      'authority',
      'authority_id',
      'score')
    taxa_map <- cbind(taxa_list, r)
  }
  
  # Return --------------------------------------------------------------------
  
  return(taxa_map)
  
}









# Create the taxonomicCoverage EML node
#
# @param sci_names
#     (list) Object returned by \code{txcl_get_classification()}.
#
# @return
# \item{list}{If \code{write.file = FALSE} an emld list object is returned
# for use with the EML R Package.}
# \item{.xml file}{If \code{write.file = TRUE} a .xml file is written to
# \code{path}}.
#
txcl_set_taxonomic_coverage <- function(sci_names) {
  
  pop <- function(taxa) {
    if (length(taxa) > 1) {
      list(
        taxonRankName = taxa[[1]]$taxonRankName,
        taxonRankValue = taxa[[1]]$taxonRankValue,
        taxonId = taxa[[1]]$taxonId,
        commonName = taxa[[1]]$commonName,
        taxonomicClassification = pop(taxa[-1]))
    } else {
      list(
        taxonRankName = taxa[[1]]$taxonRankName,
        taxonRankValue = taxa[[1]]$taxonRankValue,
        taxonId = taxa[[1]]$taxonId,
        commonName = taxa[[1]]$commonName)
    }
  }
  
  taxa <- lapply(
    sci_names,
    function(sci_name) {
      pop(sci_name)
    })
  
  return(list(taxonomicClassification = taxa))
  
}








# View taxonomic authorities
#
# @description
#     List taxonomic authorities supported by taxonomic funcs.
#
# @details
#     View taxonomic authorities supported by `resolve_taxa` and
#     `resolve_common`.
#
# @return
#     (data frame) Taxonomic authorities and corresponding identifiers
#     supported by `resolve_taxa` and `resolve_common`.
#
view_taxa_authorities <- function(){
  
  suggs <- c("ritis", "taxize", "worrms") # suggested packages
  suggsmissing <- !unlist(lapply(suggs, requireNamespace, quietly = TRUE))
  if (any(suggsmissing)) {
    stop("Packages ", paste(suggs, collapse = ", "), " are required for ",
         "running view_taxa_authorities(). Packages ", 
         paste(suggs[suggsmissing], collapse = ", "), " are not installed.", 
         call. = FALSE)
  }
  
  # Get GNR datasources -----------------------------------------------------
  
  gnr_list <- txcl_load_gnr_datasources()
  
  # Mark supported databases ------------------------------------------------
  
  gnr_list$return_to_user <- NA
  gnr_list$resolve_taxa <- NA
  gnr_list$resolve_common <- NA
  
  use_i <- gnr_list[ , 'title'] == 'ITIS'
  gnr_list[use_i, 'return_to_user'] <- 'Integrated Taxonomic Information System (ITIS)'
  gnr_list[use_i, 'resolve_taxa'] <- 'supported'
  gnr_list[use_i, 'resolve_common'] <- 'supported'
  
  use_i <- gnr_list[ , 'title'] == 'EOL'
  gnr_list[use_i, 'return_to_user'] <- 'Encyclopedia of Life (EOL)'
  gnr_list[use_i, 'resolve_taxa'] <- 'not supported'
  gnr_list[use_i, 'resolve_common'] <- 'not supported'
  
  use_i <- gnr_list[ , 'title'] == 'Tropicos - Missouri Botanical Garden'
  gnr_list[use_i, 'return_to_user'] <- 'Tropicos - Missouri Botanical Garden'
  gnr_list[use_i, 'resolve_taxa'] <- 'supported'
  gnr_list[use_i, 'resolve_common'] <- 'not supported'
  
  use_i <- gnr_list[ , 'title'] == 'GBIF Backbone Taxonomy'
  gnr_list[use_i, 'return_to_user'] <- 'Global Biodiversity Information Facility (GBIF)'
  gnr_list[use_i, 'resolve_taxa'] <- 'supported'
  gnr_list[use_i, 'resolve_common'] <- 'not supported'
  
  # use_i <- gnr_list[ , 'title'] == 'Catalogue of Life'
  # gnr_list[use_i, 'return_to_user'] <- 'Catalogue of Life (COL)'
  # gnr_list[use_i, 'resolve_taxa'] <- 'supported'
  # gnr_list[use_i, 'resolve_common'] <- 'not supported'
  
  use_i <- gnr_list[ , 'title'] == 'World Register of Marine Species'
  gnr_list[use_i, 'return_to_user'] <- 'World Register of Marine Species (WORMS)'
  gnr_list[use_i, 'resolve_taxa'] <- 'supported'
  gnr_list[use_i, 'resolve_common'] <- 'not supported'
  
  use_i <- !is.na(gnr_list$return_to_user)
  taxonomic_authorities <- gnr_list[use_i, c('id', 'return_to_user', 'resolve_taxa', 'resolve_common')]
  colnames(taxonomic_authorities) <- c('id', 'authority', 'resolve_sci_taxa', 'resolve_comm_taxa')
  rownames(taxonomic_authorities) <- c()
  
  # Return
  
  taxonomic_authorities
  
}
