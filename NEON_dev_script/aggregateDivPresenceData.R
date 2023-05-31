
#aggregate plant diversity presence data across scales

#############################################################################################
#' @title aggregate plant presence and percent cover presence data 
#'
#' @author
#' Dave T Barnett \email{dbarnettl@battelleecology.org} \cr
#'
#' @description Use this function to aggregate NEON plant presence data to reflect plant species present at each plot scale.
#'
#' @import httr XML dplyr
#'
#' @param 
#'
#' @return This function returns a data frame
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @export
#'
#' @examples
#' divData <- divStack(data_1m2 = data_1m2, data_10_100m2 = data_10_100m2) 



# changelog and author contributions / copyrights
#   Dave T Barnett (2022-08-30)
#     original creation
##############################################################################################
 
divStack <- function(data_1m2 = NA,
                     data_10_100m2 = NA) {
  
  library(dplyr)
  library(stringr)  

  ###deal with eventID and year - 1m2 data
  #create year column
  data_1m2$year <- substr(data_1m2$endDate, start = 1, stop = 4)
  data_1m2$eventID <- ifelse(is.na(data_1m2$eventID) | str_length(data_1m2$eventID)>11, paste(data_1m2$siteID, data_1m2$boutNumber, data_1m2$year, sep="."), data_1m2$eventID)
  data_1m2 <- data_1m2 %>% dplyr::select(-year)
  
  ###deal with eventID and year - larger subplot data table
  #create year column
  data_10_100m2$year <- substr(data_10_100m2$endDate, start = 1, stop = 4)
  data_10_100m2$eventID <- ifelse(is.na(data_10_100m2$eventID) | str_length(data_10_100m2$eventID)>11, paste(data_10_100m2$siteID, data_10_100m2$boutNumber, data_10_100m2$year, sep="."), data_10_100m2$eventID)
  data_10_100m2 <- data_10_100m2 %>% dplyr::select(-year)

  ###limit 1m2 data to plant species records
  data_1m2 <- filter(data_1m2, divDataType == "plantSpecies")


  ###get rid of extra fields that could be hard for generating unique records
  #data_1m2
  data_1m2 <- dplyr::select(data_1m2, namedLocation, domainID,	siteID,	decimalLatitude,	decimalLongitude,	geodeticDatum,	coordinateUncertainty,	elevation,	elevationUncertainty,
    nlcdClass, eventID, plotType, plotID,	subplotID,	boutNumber,	targetTaxaPresent,	taxonID,	scientificName,	taxonRank,	family,	nativeStatusCode,
    identificationQualifier,	morphospeciesID,	samplingImpractical, samplingImpracticalRemarks, biophysicalCriteria, publicationDate, release)  

  #data 10_100
  data_10_100m2 <- dplyr::select(data_10_100m2, namedLocation, domainID,	siteID,	decimalLatitude,	decimalLongitude,	geodeticDatum,	coordinateUncertainty,	elevation,	elevationUncertainty,
    nlcdClass, eventID, plotType, plotID,	subplotID,	boutNumber,	targetTaxaPresent,	taxonID,	scientificName,	taxonRank,	family,	nativeStatusCode,
    identificationQualifier,	morphospeciesID,	samplingImpractical, samplingImpracticalRemarks, biophysicalCriteria, publicationDate, release)  

  ##############identify those years where sample 1m2 subplots only and those years where sample both (alternating years)##############
  #don't want to process 1m2 only year with larger subplot data that don't exist

  #1m2 data data frame of eventIDs
  smallEventID <- dplyr::select(data_1m2, eventID) %>% unique()

  #10_100m2 data data frame of eventIDs
  bigEventID <- select(data_10_100m2, eventID) %>% unique()

  #make df of those eventIDs sampled the 1m2 and not the larger subplots
  smallOut <- anti_join(smallEventID, bigEventID)

  #pull out the corresponding data into unique data frame that will not be incorporated in the larger scale data
  data_1m2Out <- data_1m2 %>%
    filter(eventID %in% smallOut$eventID)

  #make df of eventID common to both
  smallMerge <- inner_join(smallEventID, bigEventID)
  smallMergeEventID <- smallMerge$eventID

  #subset to data that corresponds to evenID in both the small and large data
  data_1m2 <- data_1m2 %>%
    filter(eventID %in% smallMergeEventID)

  ###############set up data aggregation across scales, will need to update to new naming convention##############

  ###preparation

  #make sure subplotID is character (seems like it comes down that way now)
  data_10_100m2$subplotID <- as.character(data_10_100m2$subplotID)

  #separate the 10m2 data from the 100m2 data:
  data_100m2 = data_10_100m2[which(nchar(data_10_100m2$subplotID)<3), ]
  data_10m2 = data_10_100m2[which(nchar(data_10_100m2$subplotID)>2), ]

  ###build 10m2 data by aggregating observations from 1m2 (years sampled both 1 and 10_100) and 10m2
  #rename 1m2 to combine with 10
  data_10m2Build <- data_1m2

  #rename 1m2 subplots so associated observations will combine with 10m2 observations 
  data_10m2Build$subplotID[data_10m2Build$subplotID == "31.1.1"] <- "31.1.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "31.4.1"] <- "31.4.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "32.2.1"] <- "32.2.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "32.4.1"] <- "32.4.10"  
  data_10m2Build$subplotID[data_10m2Build$subplotID == "40.1.1"] <- "40.1.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "40.3.1"] <- "40.3.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "41.1.1"] <- "41.1.10"
  data_10m2Build$subplotID[data_10m2Build$subplotID == "41.4.1"] <- "41.4.10"

  #combine what was the nested 1m2 subplot data with the 10m2 data to get the complete list of species in each 10m2 subplot 
  data_10m2 <- rbind(data_10m2, data_10m2Build)

  ###aggregate 100m2 data by combining 10m2 observations with 100m2 observations 
  #rename 10m2 to combine with 100m2
  data_100m2Build <- data_10m2

  #rename 10m2 subplots so associated observations will combine with 100m2 observations
  data_100m2Build$subplotID[data_100m2Build$subplotID == "31.1.10"] <- 31
  data_100m2Build$subplotID[data_100m2Build$subplotID == "31.4.10"] <- 31
  data_100m2Build$subplotID[data_100m2Build$subplotID == "32.2.10"] <- 32
  data_100m2Build$subplotID[data_100m2Build$subplotID == "32.4.10"] <- 32
  data_100m2Build$subplotID[data_100m2Build$subplotID == "40.1.10"] <- 40
  data_100m2Build$subplotID[data_100m2Build$subplotID == "40.3.10"] <- 40
  data_100m2Build$subplotID[data_100m2Build$subplotID == "41.1.10"] <- 41
  data_100m2Build$subplotID[data_100m2Build$subplotID == "41.4.10"] <- 41

  #combine what was the nested 10m2 subplot data with the 100m2 data to get the complete list of species in each 100m2 subplot  
  data_100m2 <- rbind(data_100m2, data_100m2Build)

  #################recombine the 1m2 data#####################
  data_1m2 <- rbind(data_1m2, data_1m2Out)

  #################remove duplicates and combine to one data frame#####################
  ###make sure unique at each scale after combinations Need to figure out how to do unique on specific columns or get rid Different people might have measured the 1 and 10m subplots which could result in otherwise duplicate entries, for example. Maybe have to get rid of the stuff like date above?
  data_1m2 <- unique(data_1m2)
  data_10m2 <- unique(data_10m2)
  data_100m2 <- unique(data_100m2)

  ###create the 400m2 plot species lists
  data_400m2 <- data_100m2
  data_400m2$subplotID <- "400"
  data_400m2 <- unique(data_400m2)

  ###this chunk combines the data from all scales of measurement into one table

  #data10_100_400 <- rbind(data_10m2, data_100m2)
  data <- rbind(data_1m2, data_10m2, data_100m2, data_400m2)

  #remove duplicates
  divPlantPresenceData <- unique(data)

  #don't pass target taxa present to larger-scale subplots if not true
  divplantPresenceData <- divPlantPresenceData %>%
    group_by(eventID, plotID, subplotID) %>%
    mutate(tot = n()) %>%
    filter(if (tot > 1) {targetTaxaPresent != "N"} else {targetTaxaPresent == targetTaxaPresent}) %>%
    select(-tot) %>%
    ungroup()
  
  return(divplantPresenceData)

}





