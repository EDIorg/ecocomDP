#' Write ecocomDP tables to file
#'
#' @description  
#'     Write ecocomDP tables to file
#'
#' @usage 
#'     write_ecocomDP_tables(path, sep,  observation, location, taxon, 
#'     dataset_summary, observation_ancillary = NULL, 
#'     location_ancillary = NULL, taxon_ancillary = NULL, 
#'     variable_mapping = NULL)
#'
#' @param path
#'     (character) Directory to which the files will be written.
#' @param sep
#'     (character) Field delimiter used in the ecocomDP tables. Valid options 
#'     are "," or "\\t".
#' @param study.name
#'     (character) Name to be prefixed to each ecocomDP table name (e.g. 
#'     'bird_survey_').
#' @param observation
#'     (data frame) observation table
#' @param location
#'     (data frame) location table
#' @param taxon
#'     (data frame) taxon table
#' @param dataset_summary
#'     (data frame) dataset_summary table
#' @param observation_ancillary
#'     (data frame) observation_ancillary table
#' @param location_ancillary
#'     (data frame) location_ancillary table
#' @param taxon_ancillary
#'     (data frame) taxon_ancillary table
#' @param variable_mapping
#'     (data frame) variable_mapping table
#'
#' @return 
#'     ecocomDP tables written to file.
#'     
#' @export
#'

write_ecocomDP_tables <- function(path, sep, study.name, observation, location, 
                                  taxon, dataset_summary, 
                                  observation_ancillary = NULL, 
                                  location_ancillary = NULL, 
                                  taxon_ancillary = NULL, 
                                  variable_mapping = NULL){
  
  # Validate arguments --------------------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing.')
  }
  
  if (missing(sep)){
    stop('Input argument "sep" is missing.')
  } else if (!missing(sep)){
    if ((!stringr::str_detect(sep, ',')) | (!stringr::str_detect(sep, '\\t'))){
      stop('Input argument "sep" must be "," or "\\t".')
    }
  }
  
  if (missing(study.name)){
    stop('Input argument "study.name" is missing.')
  }
  
  if (!missing(observation)){
    if (class(observation) != 'data.frame'){
      stop('Input argument "observation" must be a data.frame')
    }
  } else if (missing(observation)){
    stop('Input argument "observation" is missing')
  }
  
  if (!missing(location)){
    if (class(location) != 'data.frame'){
      stop('Input argument "location" must be a data.frame')
    }
  } else if (missing(location)){
    stop('Input argument "location" is missing')
  }
  
  if (!missing(taxon)){
    if (class(taxon) != 'data.frame'){
      stop('Input argument "taxon" must be a data.frame')
    }
  } else if (missing(taxon)){
    stop('Input argument "taxon" is missing')
  }
  
  if (!missing(dataset_summary)){
    if (class(dataset_summary) != 'data.frame'){
      stop('Input argument "dataset_summary" must be a data.frame')
    }
  } else if (missing(dataset_summary)){
    stop('Input argument "dataset_summary" is missing')
  }
  
  if (!is.null(observation_ancillary)){
    if (class(observation_ancillary) != 'data.frame'){
      stop('Input argument "observation_ancillary" must be a data.frame')
    }
  } else if (missing(observation_ancillary)){
    stop('Input argument "observation_ancillary" is missing')
  }
  
  if (!is.null(location_ancillary)){
    if (class(location_ancillary) != 'data.frame'){
      stop('Input argument "location_ancillary" must be a data.frame')
    }
  } else if (missing(location_ancillary)){
    stop('Input argument "location_ancillary" is missing')
  }
  
  if (!is.null(taxon_ancillary)){
    if (class(taxon_ancillary) != 'data.frame'){
      stop('Input argument "taxon_ancillary" must be a data.frame')
    }
  } else if (missing(taxon_ancillary)){
    stop('Input argument "taxon_ancillary" is missing')
  }
  
  if (!is.null(variable_mapping)){
    if (class(variable_mapping) != 'data.frame'){
      stop('Input argument "variable_mapping" must be a data.frame')
    }
  } else if (missing(variable_mapping)){
    stop('Input argument "variable_mapping" is missing')
  }
  
  # Write tables to file ------------------------------------------------------
  
  message('Writing tables to file:')
  
  # observation
  
  if (sep == ','){
    
    message(paste0(study.name, '_observation.csv'))
    
    write.csv(
      observation,
      file = paste0(
        path,
        '/',
        study.name,
        '_observation.csv'),
      row.names = F,
      quote = F
    )
    
  } else if (sep == '\t'){
    
    message(paste0(study.name, '_observation.txt'))
    
    write.table(
      observation,
      file = paste0(
        path,
        '/',
        study.name,
        '_observation.txt'
      ),
      col.names = T,
      row.names = F,
      sep = "\t",
      quote = F
    )
    
  }
  
  # location
  
  if (sep == ','){
    
    message(paste0(study.name, '_location.csv'))
    
    write.csv(
      location,
      file = paste0(
        path,
        '/',
        study.name,
        '_location.csv'),
      row.names = F,
      quote = F
    )
    
  } else if (sep == '\t'){
    
    message(paste0(study.name, '_location.txt'))
    
    write.table(
      location,
      file = paste0(
        path,
        '/',
        study.name,
        '_location.txt'
      ),
      col.names = T,
      row.names = F,
      sep = "\t",
      quote = F
    )
    
  }
  
  # taxon
  
  if (sep == ','){
    
    message(paste0(study.name, '_taxon.csv'))
    
    write.csv(
      taxon,
      file = paste0(
        path,
        '/',
        study.name,
        '_taxon.csv'),
      row.names = F,
      quote = F
    )
    
  } else if (sep == '\t'){
    
    message(paste0(study.name, '_taxon.txt'))
    
    write.table(
      taxon,
      file = paste0(
        path,
        '/',
        study.name,
        '_taxon.txt'
      ),
      col.names = T,
      row.names = F,
      sep = "\t",
      quote = F
    )
    
  }
  
  # dataset_summary
  
  if (sep == ','){
    
    message(paste0(study.name, '_dataset_summary.csv'))
    
    write.csv(
      dataset_summary,
      file = paste0(
        path,
        '/',
        study.name,
        '_dataset_summary.csv'),
      row.names = F,
      quote = F
    )
    
  } else if (sep == '\t'){
    
    message(paste0(study.name, '_dataset_summary.txt'))
    
    write.table(
      dataset_summary,
      file = paste0(
        path,
        '/',
        study.name,
        '_dataset_summary.txt'
      ),
      col.names = T,
      row.names = F,
      sep = "\t",
      quote = F
    )
    
  }
  
  # observation_ancillary
  
  if (sep == ','){
    
    message(paste0(study.name, '_observation_ancillary.csv'))
    
    write.csv(
      observation_ancillary,
      file = paste0(
        path,
        '/',
        study.name,
        '_observation_ancillary.csv'),
      row.names = F,
      quote = F
    )
    
  } else if (sep == '\t'){
    
    message(paste0(study.name, '_observation_ancillary.txt'))
    
    write.table(
      observation_ancillary,
      file = paste0(
        path,
        '/',
        study.name,
        '_observation_ancillary.txt'
      ),
      col.names = T,
      row.names = F,
      sep = "\t",
      quote = F
    )
    
  }
  
  # location_ancillary
  
  if (sep == ','){
    
    message(paste0(study.name, '_location_ancillary.csv'))
    
    write.csv(
      location_ancillary,
      file = paste0(
        path,
        '/',
        study.name,
        '_location_ancillary.csv'),
      row.names = F,
      quote = F
    )
    
  } else if (sep == '\t'){
    
    message(paste0(study.name, '_location_ancillary.txt'))
    
    write.table(
      location_ancillary,
      file = paste0(
        path,
        '/',
        study.name,
        '_location_ancillary.txt'
      ),
      col.names = T,
      row.names = F,
      sep = "\t",
      quote = F
    )
    
  }
  
  # taxon_ancillary
  
  if (sep == ','){
    
    message(paste0(study.name, '_taxon_ancillary.csv'))
    
    write.csv(
      taxon_ancillary,
      file = paste0(
        path,
        '/',
        study.name,
        '_taxon_ancillary.csv'),
      row.names = F,
      quote = F
    )
    
  } else if (sep == '\t'){
    
    message(paste0(study.name, '_taxon_ancillary.txt'))
    
    write.table(
      taxon_ancillary,
      file = paste0(
        path,
        '/',
        study.name,
        '_taxon_ancillary.txt'
      ),
      col.names = T,
      row.names = F,
      sep = "\t",
      quote = F
    )
    
  }
  
  # variable_mapping
  
  if (sep == ','){
    
    message(paste0(study.name, '_variable_mapping.csv'))
    
    write.csv(
      variable_mapping,
      file = paste0(
        path,
        '/',
        study.name,
        '_variable_mapping.csv'),
      row.names = F,
      quote = F
    )
    
  } else if (sep == '\t'){
    
    message(paste0(study.name, '_variable_mapping.txt'))
    
    write.table(
      variable_mapping,
      file = paste0(
        path,
        '/',
        study.name,
        '_variable_mapping.txt'
      ),
      col.names = T,
      row.names = F,
      sep = "\t",
      quote = F
    )
    
  }

}
