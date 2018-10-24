#' Make variable_mapping table
#'
#' @description  
#'     Make a data frame of the location table.
#'
#' @usage 
#'     make_location(x, cols = c('site', 'nested_level_1', 'nested_level_2', 
#'     'etc.', kml = NULL, col = NULL))
#'
#' @param x
#'     A data frame containing sampling location names (in columns) for each 
#'     observation (rows).
#' @param cols
#'     A list of columns in x containing sampling location data. The list 
#'     order should reflect the hierarchical arrangement of sampling locations.
#'     E.g.: If 'site' is the coarsest level of observation, 'habitat' is 
#'     nested within 'site', 'transect' is nested within 'habitat', and 
#'     'quadrat' is nested within 'transect' then the 'cols' argument should
#'     be supplied as:
#'     
#'     cols = c('site', 'habitat','transect', 'quadrat')
#' @param kml
#'     A data object (simple feature collection) created with the `st_read``
#'     function of the `sf`` (simple features) R package. This data object
#'     contains a field called 'Name'. Values listed in this field must match
#'     the corresponding values listed in the data frame (x) and specified in
#'     'cols'.
#' @param col
#'     A character string indicating the column name specified in the input
#'     argument 'cols' to which the data in the kml object corresponds to.
#'     E.g., if the kml corresponds with with the column named 'site' then
#'     enter 'site' for the col argument.
#'     
#' @note 
#'     The arguments 'kml' and 'col' only accept single inputs. I.e. does 
#'     not support kml data for more than one col.
#'
#' @return 
#'     A data frame of the location table.
#'     
#' @export
#'

make_variable_mapping <- function(x, cols, kml = NULL, col = NULL){
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(x)){
    stop('Input argument "x" is missing! Specify the data frame containing your location information.')
  }
  if (missing(cols)){
    stop('Input argument "cols" is missing! Specify column names (arranged hierarchically containing location data.')
  }
  
  
  # Create location table ---------------------------------------------------
  
  # Index spatial columns
  
  col_i <- match(
    cols,
    colnames(
      x
    )
  )
  
  col_i <- col_i[!is.na(col_i)]
  
  # Initialize storage space for intermediary dataframes
  
  df <- rep(
    list(
      c()
    ), 
    (length(
      col_i))
  )
  
  # Continue ... if spatial data is present
  
  if (length(col_i) > 0){
    
    # Make keys in observation table for linking to the location table.
    
    x$keys <- apply( 
      x[ , col_i],
      1,
      paste,
      collapse = "_"
    )
    
    # Form location working table. This table is an intermediary between 
    # the observation and location tables. Copy in data$keys.
    
    k <- length(col_i)
    
    df[[k]] <- x[ , col_i]
    
    df[[k]]$key <- x$keys
    
    df[[k]] <- unique.data.frame(
      df[[k]]
    )
    
    # Make location_id for most nested location
    
    df[[k]][ , paste0('srl_key_', k)] <- paste0(
      k,
      '_',
      seq(
        nrow(
          df[[k]]
        )
      )
    )
    
    # Make similar tables for coarser spatial scales
    
    if (length(col_i) > 1){
      
      col_names <- colnames(x[ , col_i])
      
      for (j in ((length(col_i)) - 1):1){
        
        # Create table of next coarser spatial scale
        
        df[[j]] <- x[ , col_i[1:j]]
        
        # Get unique elements of this table
        
        if (j == 1){
          df[[j]] <- as.data.frame(df[[j]])
          colnames(df[[j]]) <- col_names[j]
        }
        
        df[[j]] <- unique.data.frame(
          df[[j]]
        )
        
        # Create location keys for this level
        
        df[[j]][ , paste0('srl_key_', j)] <- paste0(
          j,
          '_',
          seq(
            nrow(
              df[[j]]
            )
          )
        )
        
        # Create keys to finer nested tables (i.e. j + 1)
        # Create key at this level and create at next finer level.
        # This links info here to the parent of the next finer level.
        
        
        if (j == 1){
          df[[j]]['key'] <- df[[j]][1]
        } else {
          df[[j]]['key'] <- apply( # Create key at this level
            df[[j]][ , 1:(ncol(df[[j]])-1)],
            1,
            paste,
            collapse = '_'
          )
        }
        
        if (j == 1){
          df[[j+1]][paste0('key_to_L', j)] <- df[[j+1]][ , 1:length(col_i[1:(j)])]
        } else {
          df[[j+1]][paste0('key_to_L', j)] <- apply( # Create same key in next finer level 
            df[[j+1]][ , 1:length(col_i[1:(j)])],
            1,
            paste,
            collapse = '_'
          )
        }
        
        
        # Add srl_j_key to level = j+1 (essentially we are adding the parent_location_id)
        
        index <- match(
          df[[j+1]][[paste0('key_to_L', j)]],
          df[[j]][['key']]
        )
        
        df[[j+1]]['parent'] <- df[[j]][[paste0('srl_key_', j)]][index]
        
      }
      
    } else {
      
      j <- 1
      
    }
    
    # Coarsest level does not have a parent.
    # Add NA as parent value.
    
    df[[j]]['parent'] <- rep(
      NA_character_,
      nrow(
        df[[j]]
      )
    )
    
    # Compile the location table.
    
    location_id <- c() # initialize vectors
    location_name <- c()
    latitude <- c()
    longitude <- c()
    elevation <- c()
    parent_location_id <- c()
    
    for (j in (length(col_i)):1){ # Populate the vectors
      
      location_id <- c(
        df[[j]][[paste0('srl_key_', j)]],
        location_id
      )
      
      location_name <- c(
        as.character(df[[j]][['key']]),
        location_name
      )
      
      latitude <- c(
        latitude,
        rep(
          NA_character_,
          nrow(
            df[[j]]
          )
        )
      )
      
      longitude <- c(
        longitude,
        rep(
          NA_character_,
          nrow(
            df[[j]]
          )
        )
      )
      
      elevation <- c(
        elevation,
        rep(
          NA_character_,
          nrow(
            df[[j]]
          )
        )
      )
      
      parent_location_id <- c(
        df[[j]][['parent']],
        parent_location_id
      )
      
    }
    
    # Assemble into data.frame and arrange
    
    location_table <- data.frame(
      location_id,
      location_name,
      latitude,
      longitude,
      elevation,
      parent_location_id,
      stringsAsFactors = F
    )
    
  }
  
  if (!missing(kml)){
    
    if (missing(col)){
      stop('Input argument "col" is missing! Specify the spatial level your kml data correspond with.')
    }
    
    use_i <- match(
      location_table$location_name,
      kml$Name
    )
    
    use_i <- use_i[!is.na(use_i)]
    
    r <- length(
      unlist(
        kml$geometry
      )
    )/2
    
    location_id <- c()
    location_name <- c()
    latitude <- c()
    longitude <- c()
    elevation <- c()
    parent_location_id <- c()
    
    for (i in 1:length(use_i)){
      
      location_name <- c(
        location_name,
        paste0(
          as.character(kml$Name[use_i[i]]),
          '_outerBoundaryls_',
          seq(
            (length(unlist(kml$geometry[use_i[i]]))/2)
          )
        )                 
      )
      
      poly_coord <- unlist(
        kml$geometry[use_i[i]]
      )
      
      longitude <- c(
        longitude,
        poly_coord[1:(length(poly_coord)/2)]
      )
      
      latitude <- c(
        latitude,
        poly_coord[((length(poly_coord)/2)+1):length(poly_coord)]
      )
      
      elevation <- c(
        elevation,
        rep(
          NA,
          length(poly_coord)/2
        )
      )
      
      index <- location_table$location_name == as.character(kml$Name[use_i[i]])
      
      parent_location_id <- c(
        parent_location_id,
        rep(
          location_table$location_id[index],
          length(unlist(kml$geometry[use_i[i]]))/2
        )
      ) 
      
    }
    
    location_id <- paste0(
      'lo_p_',
      seq(
        length(
          location_name
        )
      )
    )
    
    location_poly <- data.frame(
      location_id,
      location_name,
      latitude,
      longitude,
      elevation,
      parent_location_id,
      stringsAsFactors = F
    )
    
    location_poly$parent_location_id <- paste0(
      'lo_',
      location_poly$parent_location_id
    )
    
    # Create column: location_id
    
    location_table$location_id <- paste0(
      'lo_',
      location_table$location_id
    )
    
    location_table$parent_location_id <- paste0(
      'lo_',
      location_table$parent_location_id
    )
    
    location_table$parent_location_id[location_table$parent_location_id == 'lo_NA'] <- NA_character_ 
    
    # Append location table and polygon nested table
    
    location_table <- rbind(
      location_table,
      location_poly
    )
    
  } else {
    
    # Create column: location_id
    
    location_table$location_id <- paste0(
      'lo_',
      location_table$location_id
    )
    
    location_table$parent_location_id <- paste0(
      'lo_',
      location_table$parent_location_id
    )
    
    
    location_table$parent_location_id[location_table$parent_location_id == 'lo_NA'] <- NA_character_ 
    
  }
  
  
  location_table
  
  
}
