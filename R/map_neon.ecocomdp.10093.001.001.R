##############################################################################################
##############################################################################################

# @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for TICK taxa from neon.data.product.id DP1.10093.001 from the NEON data portal and map to the ecocomDP format

##############################################################################################
# mapping function for TICK taxa
map_neon.ecocomdp.10093.001.001 <- function(
  neon.data.list,
  neon.data.product.id = "DP1.10093.001",
  ...){
  
  # Download and clean tick data
  
  #NEON target taxon group is SMALL_MAMMALS
  neon_method_id <- "neon.ecocomdp.10093.001.001"
  
  
  
  # make sure neon.data.list matches the method
  if(!any(grepl(
    neon.data.product.id %>% gsub("^DP1\\.","",.) %>% gsub("\\.001$","",.), 
    names(neon.data.list)))) stop(
      "This dataset does not appeaer to be sourced from NEON ", 
      neon.data.product.id,
      " and cannot be mapped using method ", 
      neon_method_id)
  
  
  
  ### Get the Data
  Tick_all <- neon.data.list
  
  # unlist
  tck_taxonomyProcessed = Tick_all$tck_taxonomyProcessed
  tck_fielddata = Tick_all$tck_fielddata
  
  
  
  # NOTES ON GENERAL DATA ISSUES #
  
  
  # Issue 1: the latency between field (30 days) and lab (300 days) is different
  # In 2019, tick counts were switched over to the lab instead of the field
  # If the samples aren't processed yet, there will be an NA for some or all the counts (regardless of whether ticks were present)
  # If targetTaxaPresent == "N" we will assign the counts a 0 (there is no lab data pending)
  # If targetTaxaPresent == "Y" but no associated field record, we will keep counts as NA but define that there are counts pending
  # Yearly trends may show issues for recent years because ONLY 0s are in. 
  
  # Issue 2: larva counts
  # larvae were not always counted or ID'd in earlier years
  # there are excess larvae counted in the field but not IDd in the lab -- assign these to order level
  
  # Issue 3: discrepancies between field counts and lab counts
  # Often lab counts are less than field because they stop counting at a certain limit
  # This is not always recorded in the same way
  # Other times ticks were miscounted or lost on either end
  # Probably OK to ignore most minor issues, and make best attempt at more major issues
  # Drop remaining records that are confusing
  
  #### NAs
  # replace empty characters with NAs instead of ""
  repl_na <- function(x) ifelse(x=="", NA, x)
  
  
  # CLEAN TICK TAXONOMY DATA #
  
  ### replace "" with NA
  tck_tax_filtered <- dplyr::mutate_if(
    tck_taxonomyProcessed, .predicate = is.character, .funs = repl_na) %>% 
    tidyr::as_tibble() 
  
  ### only retain lab records that have associated field data
  
  tck_tax_filtered <- dplyr::filter(tck_tax_filtered, sampleID %in% tck_fielddata$sampleID)  # none lost
  
  ### check sample quality flags
  # retain only samples deemed "OK"
  tck_tax_filtered <- dplyr::filter(tck_tax_filtered, sampleCondition == "OK")
  
  ### require date and taxon ID
  # samples without id's are not ticks! (n = 3) remove them
  tck_tax_filtered <- dplyr::filter(tck_tax_filtered, !is.na(acceptedTaxonID))
  
  # require a date (no records are lost)
  tck_tax_filtered <- dplyr::filter(tck_tax_filtered, !is.na(identifiedDate)) 
  
  ### sex or age columns
  # required (no records are lost)
  tck_tax_filtered <- dplyr::filter(tck_tax_filtered, !is.na(sexOrAge))
  
  # require counts (no records are lost)
  tck_tax_filtered <- dplyr::filter(tck_tax_filtered, !is.na(individualCount))
  
  # create a lifestage column so tax counts can be compared to field data
  tck_tax_filtered <- dplyr::mutate(
    tck_tax_filtered, 
    lifeStage = dplyr::case_when(
      sexOrAge == "Male" | 
        sexOrAge == "Female" | 
        sexOrAge == "Adult" ~ "Adult",
      sexOrAge == "Larva" ~ "Larva",
      sexOrAge == "Nymph" ~ "Nymph")) 
  
  ### rename taxonomy columns distinctly from field data
  
  # rename columns containing data unique to each dataset
  colnames(tck_fielddata)[colnames(tck_fielddata)=="uid"] <- "uid_field"
  colnames(tck_fielddata)[colnames(tck_fielddata)=="sampleCondition"] <- "sampleCondition_field"
  colnames(tck_fielddata)[colnames(tck_fielddata)=="remarks"] <- "remarks_field"
  colnames(tck_fielddata)[colnames(tck_fielddata)=="publicationDate"] <- "publicationDate_field"
  
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="uid"] <- "uid_tax"
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="sampleCondition"] <- "sampleCondition_tax"
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="remarks"] <- "remarks_tax"
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="publicationDate"] <- "publicationDate_tax"
  
  
  if("dataQF" %in% names(tck_tax_filtered)){
    colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="dataQF"] <- "dataQF_tax"
  }else{
    tck_tax_filtered[,"dataQF_tax"] <- NA_character_
  }
  
  
  if("dataQF" %in% names(tck_fielddata)){
    colnames(tck_fielddata)[colnames(tck_fielddata)=="dataQF"] <- "dataQF_field"
  }else{
    tck_fielddata[,"dataQF_field"] <- NA_character_
  }
  
  # merge counts from male/female into one adult class
  # summing male and female counts (assuming biodiversity analyses won't be sex specific)
  
  tck_tax_wide <- tck_tax_filtered %>% 
    dplyr::group_by(sampleID, acceptedTaxonID, lifeStage) %>%
    dplyr::summarise(individualCount = sum(individualCount, na.rm = TRUE)) %>%
    # now make it wide;  only one row per sample id
    tidyr::pivot_wider(
      id_cols = sampleID,  
      names_from = c(acceptedTaxonID, lifeStage), 
      values_from = individualCount, values_fill = 0)  %>% 
    dplyr::ungroup()
  
  # each row is now a unique sample id (e.g. a site x species matrix)
  
  
  
  # CLEAN TICK FIELD DATA #
  
  tck_fielddata = dplyr::mutate_if(
    tck_fielddata, 
    .predicate = is.character, 
    .funs = repl_na) 
  
  #### Quality Flags 
  # remove samples that had logistical issues
  # keep only those with NA in samplingImpractical 
  # this is ~11% as of 2020 (many related to COVID)
  
  tck_fielddata_filtered = dplyr::filter(
    tck_fielddata, 
    is.na(samplingImpractical)|samplingImpractical == "OK")
  
  # make sure all record with ticks were assigned a sample ID
  tck_fielddata_filtered = dplyr::filter(
    tck_fielddata_filtered, 
    !(targetTaxaPresent=="Y" & is.na(sampleID))) 
  
  
  # MERGE AND RESOLVE COUNTS #
  
  
  #### merge the field and lab data and figure out where counts disagree (pre-2019)
  # add non-existent columns to eventually put unidentified counts into
  if(!"IXOSP2_Adult" %in% colnames(tck_tax_wide)) {tck_tax_wide$IXOSP2_Adult <- 0}
  if(!"IXOSP2_Nymph" %in% colnames(tck_tax_wide)) {tck_tax_wide$IXOSP2_Nymph <- 0}
  if(!"IXOSP2_Larva" %in% colnames(tck_tax_wide)) {tck_tax_wide$IXOSP2_Larva <- 0}
  
  # get a list of columns containing taxonomic counts
  tax_cols = tck_tax_wide %>% 
    dplyr::ungroup() %>%  
    dplyr::select(-sampleID) %>% 
    colnames()
  
  
  #### left-join field data to lab data using sample ID
  tck_merged = tck_fielddata_filtered %>% 
    dplyr::left_join(tck_tax_wide, by = "sampleID") 
  
  # add in the lab taxonomy remarks
  tck_merged = tck_tax_filtered %>% 
    dplyr::filter(!is.na(dataQF_tax)) %>% 
    dplyr::group_by(sampleID) %>% 
    dplyr::summarise(
      dataQF_tax = paste0(dataQF_tax, collapse = " "), 
      remarks_tax = paste0(remarks_tax, collapse = " ")) %>% 
    dplyr::select(sampleID, dataQF_tax, remarks_tax) %>% 
    dplyr::ungroup() %>% 
    dplyr::right_join(tck_merged) 
  
  
  #### fill in 0s
  # if target taxa are not present, fill counts as 0s
  for(i in 1:length(tax_cols)){
    tck_merged[
      which(tck_merged$targetTaxaPresent =="N"), 
      which(colnames(tck_merged) == tax_cols[i])] <- 0
  } # this will fill in recent years data too
  
  
  ### get totals from the lab for comparisons to field counts
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      Total_Lab_Adult = rowSums(dplyr::across(contains("_Adult"))),
      Total_Lab_Nymph = rowSums(dplyr::across(contains("_Nymph"))),
      Total_Lab_Larva = rowSums(dplyr::across(contains("_Larva")))) 
  
  
  ###### get rid of/fix data with missing info 
  # 2014 and 2015 have some missing field larvae count 
  # filter to samples where there is no field larvae count but there is a lab larvae count
  field_larvae_na = dplyr::filter(tck_merged, targetTaxaPresent=="Y") %>% 
    dplyr::filter(is.na(larvaCount), !is.na(Total_Lab_Larva)) %>% 
    dplyr::mutate(year = lubridate::year(collectDate)) %>% 
    dplyr::filter(year<2019) %>% 
    dplyr::filter(!is.na(adultCount), !is.na(nymphCount)) %>% 
    dplyr::pull(sampleID) 
  
  
  # replace the field larvae counts with the lab larvae count (not really necessary as we will be using the field data anyways)
  tck_merged$larvaCount[which(tck_merged$sampleID%in%field_larvae_na)] <- tck_merged$Total_Lab_Larva[which(tck_merged$sampleID%in%field_larvae_na)]
  
  
  # check for other samples where there are missing counts prior to 2019 
  # drop those records
  # filter to samples where there is a NA in one of the field counts and it is prior to 2019
  # these are mostly legacy data and are not very abundant
  no_counts <- dplyr::filter(tck_merged, targetTaxaPresent=="Y") %>% 
    dplyr::filter(is.na(adultCount)|is.na(larvaCount)|is.na(nymphCount)) %>% 
    dplyr::mutate(year = lubridate::year(collectDate)) %>% 
    dplyr::filter(year<2019) %>% dplyr::pull(sampleID) 
  
  tck_merged <- dplyr::filter(tck_merged, !sampleID %in% no_counts)
  
  rm(field_larvae_na, no_counts)
  
  
  #### get summed lab vs. field counts for comparisons ####
  tck_merged <- tck_merged %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      totalFieldCount = sum(adultCount, larvaCount, nymphCount), # calculate total tick count from field data
      totalLabCount = sum(Total_Lab_Adult, Total_Lab_Nymph, Total_Lab_Larva)) %>%  # calculate total tick count from lab data
    dplyr::ungroup()
  
  
  # check for field data that don't have associated lab data 
  # e.g. there is a field count but no taxonomy
  # these should be removed as we don't know the species
  missing_lab <- dplyr::filter(
    tck_merged, !is.na(totalFieldCount) & is.na(totalLabCount)) %>% 
    dplyr::pull(sampleID) 
  
  tck_merged <- dplyr::filter(tck_merged, !(!is.na(sampleID) & sampleID %in% missing_lab)) 
  
  rm(missing_lab)
  
  
  # FIX COUNT DISCREPANCIES 
  
  # first flag where we have discrepancies in count that we need to fix
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      Discrepancy = dplyr::case_when(
        is.na(adultCount) & is.na(nymphCount) & 
          is.na(larvaCount) & targetTaxaPresent=="Y" & 
          !is.na(totalLabCount) ~"LAB COUNT ONLY", # no field counts but lab counts exist (2019 onwards)
        (is.na(adultCount) | is.na(nymphCount) | is.na(larvaCount)) & 
          targetTaxaPresent=="Y" & 
          lubridate::year(collectDate)>2018 ~"COUNT PENDING", # still waiting on lab counts
        totalFieldCount==0 & totalLabCount == 0 ~ "NONE", # no ticks; no discrepancy
        totalFieldCount >0 & totalLabCount > 0 & totalFieldCount==totalLabCount~"NONE",  # matching counts; no discrepancy
        targetTaxaPresent=="N"~"NONE", # no ticks; no discrepancy
        totalFieldCount > totalLabCount ~ "FIELD COUNT GREATER",
        totalFieldCount < totalLabCount ~ "LAB COUNT GREATER"
      )) 
  
  
  
  ## FIX COUNTS 1: ONLY A SUBSET OF LARVAE WERE SAMPLED/COUNTED
  
  # if there are more larvae in the field than lab and a quality flag add the remaining larvae to unidentified order
  
  add_unknown_larvae <- tck_merged %>% 
    dplyr::filter(dataQF_tax == "ID lab count subsample of total field larvae") %>% # flag indicating the lab didn't count all larvae
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER") %>% # larvae in the field > larvae in lab
    dplyr::filter((larvaCount - Total_Lab_Larva)==(totalFieldCount-totalLabCount)) %>%  # if the discrepancy in larvae is equal to the entire discrepancy
    dplyr::pull(sampleID)  # if so, pull those sample IDs
  
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Larva = dplyr::case_when( 
        sampleID %in% add_unknown_larvae ~ IXOSP2_Larva + totalFieldCount - totalLabCount, # add discrepancy in larvae to the order level (IXOSP2_Larva)
        TRUE ~ IXOSP2_Larva
      ))  
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% add_unknown_larvae] <- "FIXED" # remove the flag
  rm(add_unknown_larvae)
  
  # if there are more larvae in the field than lab and there is a note about invoicing, add remaining larvae
  add_unknown_larvae <- tck_merged %>% 
    dplyr::filter(stringr::str_detect(remarks_tax, "invoice")) %>% # flag that the lab reached an invoicing limit
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER") %>%
    dplyr::filter((larvaCount - Total_Lab_Larva)==(totalFieldCount-totalLabCount)) %>%  # if the discrepancy in larvae is equal to the entire discrepancy
    dplyr::pull(sampleID)  # if so pull those sample IDs
  
  tck_merged <- tck_merged %>% 
    dplyr::mutate(IXOSP2_Larva = dplyr::case_when(
      sampleID %in% add_unknown_larvae ~ IXOSP2_Larva + totalFieldCount - totalLabCount, # add discrepancy to the order level
      TRUE~IXOSP2_Larva)) 
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% add_unknown_larvae] <- "FIXED" # remove the flag
  rm(add_unknown_larvae)
  
  # another indicator that not all the larvae were counted
  add_unknown_larvae <- tck_merged %>% 
    dplyr::filter(stringr::str_detect(remarks_tax, "possibly subsampled in ID lab counts")) %>%  # flag indicating the lab didn't count all larvae
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER") %>%
    dplyr::filter((larvaCount - Total_Lab_Larva)==(totalFieldCount-totalLabCount)) %>%  # if the discrepancy in larvae is equal to the entire discrepancy
    dplyr::pull(sampleID)  # if so pull those sample IDs
  
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Larva = dplyr::case_when(
        sampleID %in% add_unknown_larvae ~ IXOSP2_Larva + totalFieldCount - totalLabCount, # add discrepancy to the order level
        TRUE~IXOSP2_Larva)) 
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% add_unknown_larvae] <- "FIXED"
  rm(add_unknown_larvae)
  
  ## do the same for nymphs
  add_unknown_nymph <- tck_merged %>% 
    dplyr::filter(
      stringr::str_detect(remarks_tax, "possibly subsampled in ID lab counts")) %>%  # flag indicating the lab didn't count all larvae
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER") %>%
    dplyr::filter((nymphCount - Total_Lab_Nymph)==(totalFieldCount-totalLabCount)) %>%  # if the discrepancy in nymphs is equal to the entire discrepancy
    dplyr::pull(sampleID) 
  
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Nymph= dplyr::case_when(
        sampleID %in% add_unknown_nymph ~ IXOSP2_Nymph + totalFieldCount - totalLabCount, # add discrepancy to the order level
        TRUE~IXOSP2_Nymph)) 
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% add_unknown_nymph] <- "FIXED"
  rm(add_unknown_nymph)
  
  
  
  ##  FIX COUNTS 2: LARVA COUNT RECORDED AS 1  
  # NEON working on fixing this one
  # in some years, the lab accidentally put larvae as 1 but the field count says it should be higher
  add_unknown_larvae <- tck_merged %>% 
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER") %>% 
    dplyr::filter(stringr::str_detect(dataQF_tax, "field larva count higher")) %>% 
    dplyr::filter(lubridate::year(collectDate) %in% c(2016, 2017)) %>% 
    dplyr::filter(Total_Lab_Larva==1) %>%   # flag indicating the lab assigned all as a 1 
    dplyr::filter((larvaCount - Total_Lab_Larva)==(totalFieldCount-totalLabCount)) %>% 
    dplyr::pull(sampleID) 
  
  tck_merged = tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Larva = dplyr::case_when(
        sampleID %in% add_unknown_larvae ~ 
          IXOSP2_Larva + totalFieldCount - totalLabCount, # add discrepancy to the order level
        TRUE~IXOSP2_Larva)) 
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% add_unknown_larvae] <- "FIXED"
  rm(add_unknown_larvae)
  
  add_unknown_larvae <- tck_merged %>% 
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER") %>% 
    dplyr::filter(dataQF_field == "field larva count higher than ID lab (PDE >25%)") %>% 
    dplyr::filter(Total_Lab_Larva == 1) %>% 
    dplyr::pull(sampleID)
  
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Larva = dplyr::case_when(
        sampleID %in% add_unknown_larvae ~ IXOSP2_Larva + totalFieldCount - totalLabCount, # add discrepancy to the order level
        TRUE~IXOSP2_Larva)) 
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% add_unknown_larvae] <- "FIXED"
  rm(add_unknown_larvae)
  
  
  
  ## FIX COUNTS 3: ADULTS WENT MISSING 
  add_unknown_adult <- tck_merged %>% 
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER") %>% 
    dplyr::filter(dataQF_field == "field adult count higher than ID lab (PDE >25%)") %>%  # a few missing adults
    dplyr::pull(sampleID) 
  
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Adult = dplyr::case_when(
        sampleID %in% add_unknown_adult ~ IXOSP2_Adult + totalFieldCount - totalLabCount, # add discrepancy to the order level
        TRUE~IXOSP2_Adult)) 
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% add_unknown_adult] <- "FIXED"
  rm(add_unknown_adult)
  
  
  ## FIX COUNTS 4: IGNORE MINOR DISCREPANCIES
  # if the difference is small, we will use the taxonomy data and ignore the discrepancy 
  
  # less than 5 individuals
  minor_discrep = tck_merged %>% 
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER", totalFieldCount-totalLabCount<5) %>%
    dplyr::pull(sampleID)   # get records where the differences < 5 ticks
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% minor_discrep] <- "MINOR"
  rm(minor_discrep)
  
  # less than 10%
  minor_discrep <- tck_merged %>% 
    dplyr::filter(
      Discrepancy == "FIELD COUNT GREATER", 
      (totalFieldCount-totalLabCount)/totalFieldCount<.1) %>%
    dplyr::pull(sampleID)  # records where the difference is <10%
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% minor_discrep] <- "MINOR"
  rm(minor_discrep)
  
  
  ## FIX COUNTS 6: MORE THAN ONE ERROR 
  # wrong lifestage ID and wrong counts... 
  
  # more than 500 larvae -- the lab will not count all of them
  add_unknown_larvae <- tck_merged %>% 
    dplyr::filter(Discrepancy == "FIELD COUNT GREATER", 
                  larvaCount>Total_Lab_Larva, larvaCount>500) %>% 
    dplyr::pull(sampleID) 
  
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Larva = dplyr::case_when(
        sampleID %in% add_unknown_larvae ~ IXOSP2_Larva + totalFieldCount - totalLabCount, # add discrepancy to the order level
        TRUE~IXOSP2_Larva))
  
  tck_merged$Discrepancy[tck_merged$sampleID %in% add_unknown_larvae] <- "FIXED"
  rm(add_unknown_larvae)
  
  ## GET RID OF ANY UNSOLVED MYSTERIES 
  
  # if less than 1 percent of records are unresolved, get rid of them
  if((tck_merged %>% 
      dplyr::filter(Discrepancy =="FIELD COUNT GREATER") %>% 
      nrow())/nrow(tck_merged) < 0.05){
    
    tck_merged <- tck_merged %>% 
      dplyr::filter(Discrepancy !="FIELD COUNT GREATER") 
  }
  
  # table(tck_merged$Discrepancy)
  
  #### OTHER Q/C STEPS 
  # add a column to designate samples for which counts have not been assigned yet
  # this is for recent data where the field info is published but no associated lab data
  tck_merged <- tck_merged  %>% 
    dplyr::mutate(
      COUNT_PENDING = dplyr::case_when(
        targetTaxaPresent == "Y" & 
          (is.na(adultCount) | is.na(larvaCount) | is.na(nymphCount)) & 
          is.na(totalLabCount)~"Y",
        !is.na(totalLabCount)~"N")) 
  
  
  #### Final notes on count data
  # note that some of these could have a "most likely" ID assigned based on what the majority of IDs are
  # for now don't attempt to assign. 
  # the user can decide what to do with the IXOSP2 later (e.g. assign to lower taxonomy)
  # from this point on, trust lab rather than field counts because lab were corrected if discrepancy exists
  rm(repl_na, tax_cols)
  
  
  # RESHAPE FINAL DATASET
  
  
  # will depend on the end user but creating two options
  # note that for calculation of things like richness, might want to be aware of the level of taxonomic resolution
  
  taxon.cols <- tck_merged %>% 
    dplyr::select(-contains("Total")) %>% 
    dplyr::select(contains(c("_Larva", "_Nymph", "_Adult"))) %>% 
    colnames()   # columns containing counts per taxon
  
  
  ### option 1) long form data including 0s
  
  # get rid of excess columns (user discretion)
  tck_merged_final <- tck_merged %>% 
    dplyr::select(-Discrepancy, -totalLabCount, 
                  -totalFieldCount, -Total_Lab_Adult, -Total_Lab_Larva, 
                  -Total_Lab_Nymph,
                  # -publicationDate_field, 
                  -measuredBy, 
                  -larvaCount, -nymphCount, -adultCount,
                  -coordinateUncertainty, -elevationUncertainty,
                  -samplingImpractical, -dataQF_field, 
                  # -remarks_field, 
                  # -remarks_tax
    ) %>% 
    dplyr::rename(countPending = COUNT_PENDING)
  # long form data
  
  tck_merged_final <- tck_merged_final %>% 
    tidyr::pivot_longer(
      cols = all_of(taxon.cols), 
      names_to = "Species_LifeStage", 
      values_to = "IndividualCount") %>%
    tidyr::separate(
      Species_LifeStage, 
      into = c("acceptedTaxonID", "LifeStage"), 
      sep = "_", remove = FALSE) 
  
  # add in taxonomic resolution
  
  tax_rank <- tck_taxonomyProcessed %>% 
    dplyr::group_by(acceptedTaxonID) %>% 
    dplyr::summarise(
      taxonRank = dplyr::first(taxonRank), 
      scientificName = dplyr::first(scientificName)) 
  
  tck_merged_final <- tck_merged_final %>% 
    dplyr::left_join(tax_rank, by = "acceptedTaxonID") 
  
  
  # flat data to map to ecocomDP
  data_tick <- tck_merged_final %>%
    dplyr::rename(
      taxonID = acceptedTaxonID) %>%
    dplyr::filter(
      !is.na(totalSampledArea),
      totalSampledArea > 0,
      is.finite(totalSampledArea))
  
    # replace NA counts with zeroes
    data_tick$IndividualCount[is.na(data_tick$IndividualCount)] <- 0
  
    # only keep records with counts that make sense
    data_tick <- data_tick %>%
      dplyr::filter(
        !is.na(IndividualCount),
        IndividualCount >= 0,
        is.finite(IndividualCount))
    
  #location ----
  table_location_raw <- data_tick %>%
    dplyr::select(domainID, siteID, plotID, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  nlcdClass, plotType,
                  geodeticDatum) %>%
    apply(2, as.character) %>% as.data.frame() %>%
    dplyr::distinct() 
  
  table_location <- make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"))
  
  table_location_ancillary <- make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "plotID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "nlcdClass", "plotType",
                            "geodeticDatum"))
  
  
  
  
  # taxon ----
  my_dots <- list(...)
  
  if("token" %in% names(my_dots)){
    my_token <- my_dots$token
  }else{
    my_token <- NA
  }
  
  # get tick taxon table from NEON
  neon_tick_taxon_table <- neonOS::getTaxonList(
    taxonType = "TICK", token = my_token) %>%
    dplyr::filter(taxonID %in% data_tick$taxonID)
  
  table_taxon <- neon_tick_taxon_table %>%
    dplyr::select(taxonID, taxonRank, scientificName, nameAccordingToID) %>%
    dplyr::distinct() %>% 
    dplyr::rename(taxon_id = taxonID,
                  taxon_rank = taxonRank,
                  taxon_name = scientificName,
                  authority_system = nameAccordingToID) %>%
    dplyr::select(taxon_id,
                  taxon_rank,
                  taxon_name,
                  authority_system) 
  
  
  
  # observation ----
  
  my_package_id <- paste0(
    neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  
  
  
  table_observation_all <- data_tick %>%
    # dplyr::filter(!is.na(totalSampledArea) & totalSampledArea > 0) %>%
    dplyr::distinct() %>%
    # dplyr::rename(location_id, plotID, trapID) %>%
    dplyr::rename(location_id = namedLocation) %>%
    # package id
    dplyr::mutate(
      package_id = my_package_id) %>%
    dplyr:: rename(
      neon_event_id = eventID,
      datetime = collectDate, 
      taxon_id = taxonID) %>%
    dplyr::mutate(
      observation_id = paste0("obs_",1:nrow(.)), 
      # event_id = observation_id,
      event_id = paste0(plotID, "_", datetime), #equal to sampleID, but there are NAs for sampleIDs in the data
      variable_name = "abundance",
      value = IndividualCount/totalSampledArea,
      unit = "count per square meter") 
  
  
  
  
  
  
  table_observation <- table_observation_all %>%
    dplyr::select(
      observation_id,
      event_id,
      package_id,
      location_id,
      datetime,
      taxon_id,
      variable_name,
      value,
      unit) %>%
    dplyr::filter(!is.na(taxon_id))
  
  
  
  
  
  table_observation_ancillary <- make_neon_ancillary_observation_table(
    obs_wide = table_observation_all,
    ancillary_var_names = c(
      "observation_id",
      "neon_event_id",
      # "sampleID",
      "LifeStage",
      "samplingMethod",
      "totalSampledArea",
      "targetTaxaPresent",
      "remarks_field",
      "identificationReferences",
      "release",
      "publicationDate"
    )) %>%
    dplyr::mutate(
      unit = dplyr::case_when(
        variable_name == "totalSampledArea" ~ "square meter"))
  
  # data summary ----
  # make dataset_summary -- required table
  
  years_in_data <- table_observation$datetime %>% lubridate::year()
  # years_in_data %>% ordered()
  
  table_dataset_summary <- data.frame(
    package_id = table_observation$package_id[1],
    original_package_id = neon.data.product.id,
    length_of_survey_years = max(years_in_data) - min(years_in_data) + 1,
    number_of_years_sampled	= years_in_data %>% unique() %>% length(),
    std_dev_interval_betw_years = years_in_data %>% 
      unique() %>% sort() %>% diff() %>% stats::sd(),
    max_num_taxa = table_taxon$taxon_id %>% unique() %>% length()
  )
  
  
  
  # return tables ----
  
  out_list <- list(
    location = table_location,
    location_ancillary = table_location_ancillary,
    taxon = table_taxon,
    observation = table_observation,
    observation_ancillary = table_observation_ancillary,
    dataset_summary = table_dataset_summary)
  
  return(out_list)
  
}