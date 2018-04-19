#' Make location table
#'
#' @description  
#'     Make a data frame of the location table.
#'
#' @usage 
#'     make_location(x, cols = c('site', 'nested_level_1', 'nested_level_2', 
#'     'etc.' ))
#'
#' @param x
#'     A data frame containing sampling location names (in columns) for each 
#'     observation (rows).
#'     
#' @param cols
#'     A list of columns in x containing sampling location data. The list 
#'     order should reflect the hierarchical arrangement of sampling locations.
#'     E.g.: If 'site' is the coarsest level of observation, 'habitat' is 
#'     nested within 'site', 'transect' is nested within 'habitat', and 
#'     'quadrat' is nested within 'transect' then the 'cols' argument should
#'     be supplied as:
#'     
#'     cols = c('site', 'habitat','transect', 'quadrat')
#'
#' @return 
#'     A data frame of the location table.
#'     
#' @export
#'



make_location <- function(x, cols){

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
      data
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
    
    data$keys <- apply( 
      data[ , col_i],
      1,
      paste,
      collapse = "_"
    )
    
    # Form location working table. This table is an intermediary between 
    # the observation and location tables. Copy in data$keys.
    
    k <- length(col_i)
    
    df[[k]] <- data[ , col_i]
    
    df[[k]]$key <- data$keys
    
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
      
      col_names <- colnames(data[ , col_i])
      
      for (j in ((length(col_i)) - 1):1){
        
        # Create table of next coarser spatial scale
        
        df[[j]] <- data[ , col_i[1:j]]
        
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
  
  
  
  
}
