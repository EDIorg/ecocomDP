##############################################################################################
##############################################################################################
#' @examples 
#' \dontrun{
#' my_result <- map_neon.ecocomdp.10093.001.001(
#' site = c("BART","DSNY"), 
#' startdate = "2016-01",
#' enddate = "2017-11",
#' token = Sys.getenv("NEON_TOKEN"),
#'   check.size = FALSE)
#' }

#' @describeIn map_neon_data_to_ecocomDP This method will retrieve count data for TICK taxa from neon.data.product.id DP1.10093.001 from the NEON data portal and map to the ecocomDP format
#' @export

##############################################################################################
# mapping function for SMALL_MAMMAL taxa
map_neon.ecocomdp.10093.001.001 <- function(
  neon.data.product.id = "DP1.10093.001",
  ...){
  
  # Download and clean tick data
  # Authors: Wynne Moss, Melissa Chen, Brendan Hobart, Matt Bitters
  # Date: 7/8/2020
  # updated by Eric Sokol for ecocomDP formatting 21 Feb 2021
  
  
  #NEON target taxon group is SMALL_MAMMALS
  neon_method_id <- "neon.ecocomdp.10093.001.001"
  
  # check arguments passed via dots for neonUtilities
  dots_updated <- list(..., dpID = neon.data.product.id)
  
  #Error handling if user provides a value for "package" other than "expanded"
  if("package" %in% names(dots_updated) && dots_updated$package != "expanded") message("WARNING: expanded package for DP1.10003.001 is required to execute this request. Downloading the expanded package")
  
  dots_updated[["package"]] <- "expanded"
  
  ### Get the Data
  Tick_all <- rlang::exec( 
    neonUtilities::loadByProduct,
    !!!dots_updated)
  
  
  

  
  
  # NOTES ON GENERAL DATA ISSUES #
  
  # Issue 1: the latency between field (30 days) and lab (300 days) is different
  # In 2019, tick counts were switched over to the lab instead of the field
  # If the samples aren't processed yet, there will be an NA for all the counts (regardless of whether ticks were present)
  # Normally if ticks weren't present (targetTaxaPresent == "N") we could feel comfortable assigning all the tick counts a 0 (no ticks)
  # In this case, doing that would mean that there are 0s when ticks are absent and NAs when ticks are present
  # Yearly trends would show a bias since there are no counts for all the sites with ticks, and 0s when there aren't ticks
  # Rather, we should not use those dates until the lab data come in
  # Would be good to check with NEON whether the 0s for counts are assigned at the same time the lab data come in?
  
  # Issue 2: larva counts
  # larvae were not always counted or ID'd in earlier years
  # requiring larval counts to be non-NA will remove records from earlier years
  
  # Issue 3: discrepancies between field counts and lab counts
  # Often lab counts are less than field because they stop counting at a certain limit
  # This is not always recorded in the same way
  # Other times ticks were miscounted or lost on either end
  # Probably OK to ignore most minor issues, and make best attempt at more major issues
  # Drop remaining records that are confusing
  
  # CLEAN TICK FIELD DATA #
  
  #### NAs
  # replace empty characters with NAs instead of ""
  repl_na <- function(x) ifelse(x == "", NA, x)
  
  tck_fielddata <- dplyr::mutate_if(Tick_all$tck_fielddata, .predicate = is.character,
                                   .funs = repl_na) %>% tibble::as_tibble()
  
  # # check which fields have NAs
  # dplyr::summarise_all(tck_fielddata, ~sum(is.na(.))) %>% as.data.frame()
  # 
  #### Quality Flags
  # remove samples that had logistical issues
  # keep only those with NA in samplingImpractical
  tck_fielddata_filtered <- dplyr::filter(tck_fielddata, is.na(samplingImpractical) |
                                           samplingImpractical == "OK")
  
  #### Remove records with no count data
  # # first confirm that lots of the records with no count data are recent years
  # tck_fielddata_filtered %>% dplyr::filter(is.na(adultCount), is.na(nymphCount)) %>%
  #   dplyr::mutate(year = lubridate::year(collectDate)) %>%
  #   dplyr::pull(year) %>% table()
  
  # filter only records with count data
  tck_fielddata_filtered = dplyr::filter(tck_fielddata_filtered, !is.na(adultCount),
                                         !is.na(nymphCount), !is.na(larvaCount))
  # note that requiring larval counts to be non na will drop some legacy data
  
  #### Check correspondence between field and lab
  # Making the assumption that the user only cares about ID'd ticks and not raw abundances.
  # If so, only include field records corresponding to the dates with lab data
  
  # Get rid of field samples that have no taxonomic info
  tck_fielddata_filtered = dplyr::filter(
    tck_fielddata_filtered,
    sampleID %in% Tick_all$tck_taxonomyProcessed$sampleID |
                                           is.na(sampleID))
  
  # Get rid of the tax samples that have no field data
  # many of these are legacy samples where larvae weren't counted
  tck_tax_filtered = dplyr::filter(
    Tick_all$tck_taxonomyProcessed,
    sampleID %in% tck_fielddata_filtered$sampleID) %>%
    tibble::as_tibble()
  
  # # double check same sample ID in both datasets
  # length(unique(na.omit(tck_fielddata_filtered$sampleID)))
  # length(unique(tck_tax_filtered$sampleID)) # match
   
  #### Check other quality control/sample remarks
  
  # table(tck_fielddata_filtered$sampleCondition) # none of these are major issues
  
  # table(tck_fielddata_filtered$dataQF) # legacy data only
  
  # tck_fielddata_filtered %>% dplyr::filter(dataQF == "legacyData") %>%
  #   dplyr::mutate(year = lubridate::year(collectDate)) %>%
  #   dplyr::pull(year) %>% table()
  # from earlier years (keep for now)
  
  #### Sample IDs
  # check that all of the rows WITHOUT a sample ID have no ticks
  # tck_fielddata_filtered %>% dplyr::filter(is.na(sampleID)) %>%
  #   dplyr::pull(targetTaxaPresent) %>% table() # true
  
  # check that all the rows WITH a sample ID have ticks
  # tck_fielddata_filtered %>% dplyr::filter(!is.na(sampleID)) %>%
  #   dplyr::pull(targetTaxaPresent) %>% table() # true
  
  # sampleID only assigned when there were ticks present; all missing sample IDs are for drags with no ticks (good)
  # for now, retain sampling events without ticks
  
  # make sure sample IDs are unique
  # tck_fielddata_filtered %>% group_by(sampleID) %>% summarise(n = n()) %>% filter(n > 1) # yes all unique
  # sum(duplicated(na.omit(tck_fielddata_filtered$sampleID)))
  
  # check that drags with ticks present have a sample ID
  # tck_fielddata_filtered %>% filter(targetTaxaPresent == "Y" & is.na(sampleID)) # none are missing S.ID
  
  #### Check Counts vs. NAs
  
  # # make sure samples with ticks present have counts
  # tck_fielddata_filtered %>% filter(targetTaxaPresent == "Y") %>%
  #   filter(is.na(adultCount) & is.na(nymphCount) & is.na(larvaCount)) %>% nrow()
  # 
  # # are there any 0 counts where there should be > 0?
  # tck_fielddata_filtered %>% filter(targetTaxaPresent == "Y") %>%
  #   mutate(totalCount = adultCount + nymphCount + larvaCount) %>%
  #   filter(totalCount == 0) # no
  
  ### Check for other missing count data
  # list of fields that shouldn't have NAs
  req_cols <- c("siteID", "plotID", "collectDate", 
                "adultCount", "nymphCount", "larvaCount")
  
  # # all should have no NAs
  # tck_fielddata_filtered %>% select(req_cols) %>% summarise_all(~sum(is.na(.)))
  
  # CLEAN TICK TAXONOMY DATA
  
  ### replace "" with NA
  tck_tax_filtered <- dplyr::mutate_if(
    tck_tax_filtered, 
    .predicate = is.character,
    .funs = repl_na) %>%
    ## only retain lab records that are also in the field dataset
    dplyr::filter(sampleID %in% tck_fielddata_filtered$sampleID)
  
  # ### check sample quality flags
  # table(tck_tax_filtered$sampleCondition)
  
  # none of these are deal breakers but just keep those deemed OK
  tck_tax_filtered = dplyr::filter(tck_tax_filtered, sampleCondition == "OK")
  
  # ### check for NAs in fields
  # tck_tax_filtered %>% select(everything()) %>%
  #   summarise_all(~sum(is.na(.))) %>% as.data.frame()
  
  ### create a flag for potential lab ID/count issues
  # table(Tick_all$tck_taxonomyProcessed$remarks)
  
  tck_fielddata_filtered$IDflag <- NA
  
  # mark sample IDs that have taxonomy issues
  # these come from remarks indicating that ticks in field were mis-ID'd
  # useful for correcting counts later
  tax.issues = Tick_all$tck_taxonomyProcessed %>%
    dplyr::filter(
      str_detect(remarks, "insect|mite|not a tick|NOT A TICK|arachnid|spider")) %>%
    pull(sampleID) %>% unique()
  
  # add a flag for these samples in the field dataset
  tck_fielddata_filtered$IDflag[which(tck_fielddata_filtered$sampleID %in% tax.issues)] <- "ID WRONG"
  
  # sample IDs where lab reached limit and stopped counting
  # will help explain why lab counts < field counts
  invoice.issues <- Tick_all$tck_taxonomyProcessed %>%
    dplyr::filter(stringr::str_detect(remarks, "billing limit|invoice limit")) %>%
    dplyr::pull(sampleID) %>% unique()
  
  tck_fielddata_filtered$IDflag[
    which(tck_fielddata_filtered$sampleID %in% invoice.issues)] <- "INVOICE LIMIT"
  
  ### require date and taxon ID
  tck_tax_filtered <- dplyr::filter(tck_tax_filtered, !is.na(identifiedDate)) %>%
   dplyr::filter(!is.na(acceptedTaxonID))
  
  # # epithet are all NAs (don't need these columns)
  # table(tck_tax_filtered$infraspecificEpithet)
  
  # # qualifier
  # table(tck_tax_filtered$identificationQualifier)
  
  # # legacy data is the only flag (OK)
  # table(tck_tax_filtered$dataQF)
  
  # other remarks: some seem relevant for reconciling field and lab
  # however it will become onerous to go through these individually as dataset grows
  # ignore most of these for now
  # unique(tck_tax_filtered$remarks)
  
  # ### check that the IDs all make sense
  # table(tck_tax_filtered$acceptedTaxonID)
  # # IXOSPP1 = genus ixodes (Ixodes spp.):
  # tck_tax_filtered %>% filter(acceptedTaxonID == "IXOSPP1") %>% select(scientificName, taxonRank) %>% head()
  # # IXOSPP = family ixodidae (Ixodidae spp.):
  # tck_tax_filtered %>% filter(acceptedTaxonID == "IXOSPP") %>% select(scientificName, taxonRank) %>% head()
  # IXOSP2 = order ixodes (Ixodida sp.):
  # tck_tax_filtered %>% filter(acceptedTaxonID == "IXOSP2") %>% select(scientificName, taxonRank) %>% head()
  # IXOSP = family exodidae (Ixodidae sp.)
  # tck_tax_filtered %>% filter(acceptedTaxonID == "IXOSP") %>% select(scientificName, taxonRank) %>% head()
  
  # table(tck_tax_filtered$taxonRank)
  # table(tck_tax_filtered$family)
  
  # if only ID'd to order, all are IXOSP2:
  # tck_tax_filtered %>% filter(taxonRank == "order") %>% pull(acceptedTaxonID) %>% table()
  # if only ID'd to family, either IXOSP or IXOSPP
  # not sure how to deal with mixed species (e.g. IXOSPP)
  # tck_tax_filtered %>% filter(taxonRank == "family") %>% pull(acceptedTaxonID) %>% table()
  # tck_tax_filtered %>% filter(taxonRank == "species") %>% pull(acceptedTaxonID) %>% table()
  
  # ### sex or age columns
  # table(tck_tax_filtered$sexOrAge)
  # tck_tax_filtered %>% filter(is.na(sexOrAge)) # all have sex or age
  # tck_tax_filtered %>% filter(individualCount == 0 | is.na(individualCount)) %>% nrow() # all have counts
  
  # create a lifestage column so tax counts can be compared to field data
  tck_tax_filtered <- tck_tax_filtered %>%
    mutate(lifeStage = case_when(sexOrAge == "Male" | sexOrAge == "Female" |
                                   sexOrAge == "Adult" ~ "Adult",
                                 sexOrAge == "Larva" ~ "Larva",
                                 sexOrAge == "Nymph" ~ "Nymph"))
  # sum(is.na(tck_tax_filtered$lifeStage))
  # this assumes that any ticks that were sexed must be adults (I couldn't confirm this)
  
  #  MERGE FIELD AND TAXONOMY DATA #
  
  # there are a few options here:
  # 1) left join tick_field to tick_tax: this will keep all the 0s but requires some wide/long manipulation
  # 2) left join tick_tax to tick_field: this will only keep the records where ticks were ID'd. could add in 0s afterwards depending on preference
  
  # We will left join tick_field to tick_tax, so that 0s are preserved.
  # Merging will require us to resolve count discrepancies (this may be overkill for those interested in P/A)
  # For simplicity we will also collapse M/F into all adults (also optional)
  
  # first check for dup colnames
  # intersect(colnames(tck_fielddata_filtered), colnames(tck_tax_filtered))
  
  # rename columns containing data unique to each dataset
  colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="uid"] <- "uid_field"
  colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="sampleCondition"] <- "sampleCondition_field"
  colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="remarks"] <- "remarks_field"
  colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="dataQF"] <- "dataQF_field"
  colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="publicationDate"] <- "publicationDate_field"
  
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="uid"] <- "uid_tax"
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="sampleCondition"] <- "sampleCondition_tax"
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="remarks"] <- "remarks_tax"
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="dataQF"] <- "dataQF_tax"
  colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="publicationDate"] <- "publicationDate_tax"
  
  # in order to merge counts with field data we will first combine counts from male/female in the tax data into one adult class
  # if male/female were really of interest you could skip this (but the table will just be much wider since all will have a M-F option)
  
  tck_tax_wide <- tck_tax_filtered %>% 
    dplyr::group_by(sampleID, acceptedTaxonID, lifeStage) %>%
    dplyr::summarise(individualCount = sum(individualCount, na.rm = TRUE)) %>%
    # now make it wide;  only one row per sample id
    tidyr::pivot_wider(id_cols = sampleID,  
                names_from = c(acceptedTaxonID, lifeStage), 
                values_from = individualCount, values_fill = 0)
  
  # each row is now a unique sample id (e.g. a site x species matrix)
  
  # sum(duplicated(tck_tax_wide$sampleID)) # none are duplicated
  # sum(duplicated(na.omit(tck_fielddata_filtered$sampleID)))
  # length(unique(tck_tax_wide$sampleID))
   
  tck_merged <- dplyr::left_join(
    tck_fielddata_filtered, tck_tax_wide, by = "sampleID")
  
  #FIX COUNT DISCREPANCIES BETWEEN FIELD AND LAB #
  
  ### clean up 0s
  # if ticks were not found in the field, counts should be 0
  taxon.cols <- tck_merged %>% 
    dplyr::select(
      dplyr::contains(c("_Nymph", "_Adult" , "_Larva"))) %>% 
    colnames() # columns containing counts per taxon
  
  

  
  for(i in 1:length(taxon.cols)){
    tck_merged[which(tck_merged$targetTaxaPresent =="N"), which(colnames(tck_merged) == taxon.cols[i])] <- 0
  }
  
  

  
  
  # ### check for NA
  # # check for NAs in the count columns
  # tck_merged %>% select_if(is_numeric)%>% summarise_all(~sum(is.na(.)))
  
  # NAs indicate the lab determined no ticks in sample or the associated record was pulled
  
  # if there are NAs for counts and an ID flag, most likely the lab did not find ticks and field data should be corrected
  uid_change <- tck_merged %>% 
    dplyr::select(uid_field, 
                  dplyr::all_of(taxon.cols), 
                  IDflag) %>%
    dplyr::filter_at(
      dplyr::vars(taxon.cols), 
      dplyr::all_vars(is.na(.))) %>%
    dplyr::filter(!is.na(IDflag)) %>% 
    dplyr::pull(uid_field)
  
  # correct the field data to reflect no ticks
  tck_merged[which(tck_merged$uid_field == uid_change), "targetTaxaPresent"] <- "N"
  tck_merged[which(tck_merged$uid_field == uid_change), "adultCount"] <- 0
  tck_merged[which(tck_merged$uid_field == uid_change), "nymphCount"] <- 0
  tck_merged[which(tck_merged$uid_field == uid_change), "larvaCount"] <- 0
  
  

  
  for(i in 1:length(taxon.cols)){
    tck_merged[which(tck_merged$uid_field==uid_change), which(colnames(tck_merged) == taxon.cols[i])] = 0
  }
  
  

  
  
  
  #### FIX COUNT 1.1 FLAG DISCREPANCIES ###
  
  # first get list of columns for easy summing
  adult_cols <- tck_merged %>% select(contains("_Adult")) %>% colnames() 
  nymph_cols <- tck_merged %>% select(contains("_Nymph")) %>% colnames()
  larva_cols <- tck_merged %>% select(contains("_Larva")) %>% colnames()
  
  # sum up lab and field totals
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      totalAdult_tax = rowSums(.[adult_cols]),
      totalNymph_tax = rowSums(.[nymph_cols]),
      totalLarva_tax = rowSums(.[larva_cols]),
      totalCount_tax = rowSums(.[c(adult_cols, nymph_cols, larva_cols)]),
      totalCount_field = rowSums(.[c("adultCount", "nymphCount", "larvaCount")])
    )
  
  # get a list of columns with counts for easily viewing dataset (optional)
  countCols <- tck_merged %>%
    dplyr::select(
      dplyr::contains(c("adult", "nymph","larva", 
                        "total", "Adult",  "Nymph",  
                        "Larva", "Count"), ignore.case = FALSE)) %>%
    dplyr::select(-totalSampledArea) %>% 
    colnames()
  
  # create flag column to mark issues with count discrepancies
  tck_merged <- tck_merged %>% 
    dplyr::mutate(CountFlag = NA) 
  
  # flag 1: the total count matches, but the life stage columns don't (error 1)
  tck_merged$CountFlag[tck_merged$totalCount_field == tck_merged$totalCount_tax] <- 1
  
  # no flag:  all the counts match (no flag) -- some of the 1s will now be  0s
  tck_merged$CountFlag[tck_merged$nymphCount == tck_merged$totalNymph_tax &
                         tck_merged$adultCount == tck_merged$totalAdult_tax &
                         tck_merged$larvaCount == tck_merged$totalLarva_tax] <- 0
  
  # no flag: no ticks
  tck_merged$CountFlag[tck_merged$targetTaxaPresent=="N"] <- 0
  
  # flag 2: the field total is > than tax total
  tck_merged$CountFlag[tck_merged$totalCount_field > tck_merged$totalCount_tax] <- 2
  
  # flag 3: the field total is < than the tax total
  tck_merged$CountFlag[tck_merged$totalCount_field < tck_merged$totalCount_tax] <- 3
  
  # ### reconcile count discrepancies
  # table(tck_merged$CountFlag)
  # # the vast majority of counts match.
  
  
  #### FIX COUNT 2.1 RECONCILE DISCREPANCIES WHERE TOTALS MATCH ###
  
  # ### Flag Type 1: the total count from field and lab matches but not individual lifestages
  # tck_merged %>% filter(CountFlag == 1) %>% select(all_of(countCols))
  # # many of these are adults misidentified as nymphs or vice versa
  # # trust the lab counts here (ignore nymphCount, adultCount, larvaCount from field data)
  # # don't correct any counts and change the flag from a 1 to 0
  tck_merged <- tck_merged %>%  
    dplyr::mutate(CountFlag = ifelse(CountFlag == 1, 0, CountFlag))
  # table(tck_merged$CountFlag)
  
  
  #### FIX COUNT 2.2 RECONCILE DISCREPANCIES WHERE FIELD COUNT > TAX COUNT ###
  
  
  ### Flag Type 2:more ticks found in the field than in the lab
  
  # in some cases, there were too many larvae and not all were ID'd
  # in other cases, there may be issues with the counts and some ended up not being ticks
  # if we can't rectify, lump the excess field ticks into IXOSP2
  # IXOSP2 is the least informative ID, essentially the same as marking them as "unidentified tick"
  
  ### some of these have notes in the ID flag column indicating a non-tick
  # trust the lab count and don't correct the counts
  # remove flag (2->0)
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      CountFlag = dplyr::case_when(
        CountFlag == 2 & IDflag == "ID WRONG" ~ 0,
        TRUE ~ CountFlag))
  
  ### some have more larvae in the field than in the lab
  # larvae weren't always identified/counted in the lab, or if they were, only counted up to a limit
  # if there were more larvae in the field than in lab, the extra larvae should be added to the order level (IXOSP2)
  # "extra larvae" = larvae not counted - those moved to another lifestage
  
  

  if(!"IXOSP2_Larva" %in% names(tck_merged)) tck_merged[,"IXOSP2_Larva"] <- NA_real_
  
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Larva = dplyr::case_when(
        CountFlag == 2 & larvaCount > totalLarva_tax & is.na(IDflag) ~ 
          IXOSP2_Larva + 
          (larvaCount - totalLarva_tax) + 
          (nymphCount - totalNymph_tax) + 
          (larvaCount - totalLarva_tax),
    TRUE ~ IXOSP2_Larva))
  
  # remove flag
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      CountFlag = dplyr::case_when(
        CountFlag == 2 & larvaCount>totalLarva_tax & is.na(IDflag)~0,
        TRUE ~ CountFlag)) 
  
  
  ### some counts are off by only a few individuals (negligible)
  # could be miscounted in the field or lost in transit
  # if counts are off by < 10% this difference is not too meaningful
  # trust the lab count data, don't change any counts
  # remove flag
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      CountFlag = dplyr::case_when(
        CountFlag == 2 & (totalCount_field - totalCount_tax)/totalCount_field < 0.1 ~0, # if discrepancy < 10% of total
        TRUE ~ CountFlag))
  
  ### some counts are off by more than 10% but explanation is obvious
  # cases where 1-2 field ticks are "missing" but discrepancy is more than 10% of total count
  # if there are field remarks, assume they are about ticks being lost (see field_remarks output)
  # assign missing field ticks to order level IXOSP2
  # missing (unidentified) ticks = missing counts that were not assigned to other lifestages
  
  # if IXOSP2 doesn't exist for other lifestages, add it
  if(sum(colnames(tck_merged) == "IXOSP2_Adult")==0) {
    # if column doesn't exist,
    # add it with a default of 0
    tck_merged <- tck_merged %>% 
      dplyr::mutate(IXOSP2_Adult = 0)
  }
  if (sum(colnames(tck_merged) == "IXOSP2_Nymph") == 0) {
    tck_merged <- tck_merged %>% 
      dplyr::mutate(IXOSP2_Nymph = 0)
  }
  if (sum(colnames(tck_merged) == "IXOSP2_Larva") == 0) {
    tck_merged <- tck_merged %>% 
      dplyr::mutate(IXOSP2_Larva = 0)
  }
  
  # if there are missing adults and remarks in the field, add the missing adults to IXOSP2_Adult
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Adult = dplyr::case_when(
        CountFlag == 2 & adultCount>totalAdult_tax & 
          (adultCount-totalAdult_tax) <= 2 & !is.na(remarks_field) ~
          IXOSP2_Adult+ (adultCount - totalAdult_tax) + 
          (nymphCount-totalNymph_tax) + (larvaCount-totalLarva_tax), # count discrepancy add to order IXOSP2
        TRUE ~ IXOSP2_Adult)) %>%
    # if there are missing nymphs and remarks in the field, add the missing adults to IXOSP2_Nymph
    dplyr::mutate(
      IXOSP2_Nymph = dplyr::case_when(
        CountFlag == 2 & nymphCount>totalNymph_tax & 
          (nymphCount-totalNymph_tax) <= 2 & !is.na(remarks_field) ~
          IXOSP2_Nymph+ (adultCount - totalAdult_tax) + 
          (nymphCount-totalNymph_tax) + (larvaCount-totalLarva_tax), # count discrepancy add to order IXOSP2
        TRUE ~ IXOSP2_Nymph)) %>%
    # if there are missing larvae and remarks in the field, add the missing adults to IXOSP2_Larva
    dplyr::mutate(
      IXOSP2_Larva = dplyr::case_when(
        CountFlag == 2 & larvaCount>totalLarva_tax & 
          (larvaCount-totalLarva_tax) <= 2 & !is.na(remarks_field) ~
          IXOSP2_Larva + (adultCount - totalAdult_tax) + 
          (nymphCount-totalNymph_tax) + (larvaCount-totalLarva_tax),
      TRUE ~ IXOSP2_Larva))
  
  
  # remove flag
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      CountFlag = dplyr::case_when(
        CountFlag == 2 & adultCount>totalAdult_tax & 
          (adultCount-totalAdult_tax) <= 2 & !is.na(remarks_field) ~ 0,
        CountFlag == 2 & nymphCount>totalNymph_tax & 
          (nymphCount-totalNymph_tax) <=2 & !is.na(remarks_field) ~ 0,
        CountFlag == 2 & larvaCount>totalLarva_tax & 
          (larvaCount-totalLarva_tax) <=2 & !is.na(remarks_field) ~ 0,
        TRUE~ CountFlag))
  
  
  ### some counts are off because over invoice limit
  # in this case trust the field counts, and extra are assigned to order level
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      IXOSP2_Nymph = dplyr::case_when(
        CountFlag == 2 & nymphCount > totalNymph_tax & IDflag == "INVOICE LIMIT" ~
          IXOSP2_Nymph+ (adultCount - totalAdult_tax) + 
          (nymphCount-totalNymph_tax) + (larvaCount-totalLarva_tax), # add count discrepancy to order
        TRUE ~ IXOSP2_Nymph)) %>%
    dplyr::mutate(
      IXOSP2_Larva = dplyr::case_when(
        CountFlag == 2 & larvaCount>totalLarva_tax & IDflag == "INVOICE LIMIT" ~
          IXOSP2_Larva+ (adultCount - totalAdult_tax) + 
          (nymphCount-totalNymph_tax) + (larvaCount-totalLarva_tax), # add count discrepancy to order
        TRUE ~ IXOSP2_Larva)) %>%
    dplyr::mutate(
      IXOSP2_Adult = dplyr::case_when(
        CountFlag == 2 & adultCount>totalAdult_tax & IDflag == "INVOICE LIMIT" ~
          IXOSP2_Adult+ (adultCount - totalAdult_tax) + 
          (nymphCount-totalNymph_tax) + (larvaCount-totalLarva_tax), # add count discrepancy to order
        TRUE ~ IXOSP2_Adult))
  
  # remove flag
  tck_merged <- tck_merged %>% 
    dplyr::mutate(
      CountFlag = dplyr::case_when(
    CountFlag == 2 & adultCount>totalAdult_tax &IDflag == "INVOICE LIMIT" ~ 0,
    CountFlag == 2 & nymphCount>totalNymph_tax &IDflag == "INVOICE LIMIT" ~ 0,
    CountFlag == 2 & larvaCount>totalLarva_tax &IDflag == "INVOICE LIMIT" ~ 0,
    TRUE ~ CountFlag))
  
  #### FIX COUNT 2.3 RECONCILE DISCREPANCIES WHERE FIELD COUNT < TAX COUNT
  
  ### cases where counts are off by minor numbers
  # this could be miscounts in the field
  # if the discrepancy is less than 30% of the total count or 5 or less individuals
  # trust the lab count here and don't correct counts
  
  # remove flag
  tck_merged <- tck_merged %>% dplyr::mutate(
    CountFlag = dplyr::case_when(
      CountFlag == 3 & (abs(totalCount_tax-totalCount_field))/totalCount_tax < 0.3 ~ 0,
      (totalCount_tax - totalCount_field)<=5 ~ 0,
      TRUE ~ CountFlag))
  
  #### FIX COUNT 2.4 REMOVE THE UNSOLVED MYSTERY SAMPLES
  # cases with larger discrepancies that aren't obvious are removed from final dataset
  # make sure this is less than 1% of total cases
  # more work could be done here if there are many cases in this category
  if((tck_merged %>% filter(CountFlag!=0) %>% nrow())/nrow(tck_merged)< 0.01){
    tck_merged <- tck_merged %>% dplyr::filter(CountFlag==0)
  }
  
  #### Final notes on count data
  # note that some of these could have a "most likely" ID assigned based on what the majority of IDs are
  # for now don't attempt to assign.
  # the user can decide what to do with the IXOSP2 later (e.g. assign to lower taxonomy)
  # from this point on, trust lab rather than field counts because lab were corrected if discrepancy exists
  
  # RESHAPE FINAL DATASET
  
  # will depend on the end user but creating two options
  # note that for calculation of things like richness, might want to be aware of the level of taxonomic resolution
  
  taxon.cols <- tck_merged %>% 
    dplyr::select(
      dplyr::contains(c("_Larva", "_Nymph", "_Adult"))) %>% 
    colnames() # columns containing counts per taxon
  
  
  ### option 1) long form data including 0s
  
  # get rid of excess columns (user discretion)
  tck_merged_final <- tck_merged %>% 
    dplyr::select(
      -CountFlag, -IDflag, -totalAdult_tax, -totalNymph_tax, 
      -totalLarva_tax, -totalCount_tax, -totalCount_field,
      -nymphCount, -larvaCount, -adultCount,
      -samplingImpractical, -measuredBy, -dataQF_field, -publicationDate_field)
  
  
  # long form data
  tck_merged_final <- tck_merged_final %>% 
    tidyr::pivot_longer(
      cols = all_of(taxon.cols), 
      names_to = "Species_LifeStage", 
      values_to = "IndividualCount") %>%
    tidyr::separate(
      Species_LifeStage, 
      into = c("acceptedTaxonID", "LifeStage"), 
      sep = "_", 
      remove = FALSE)
  
  
  # # add in taxonomic resolution
  # table(Tick_all$tck_taxonomyProcessed$acceptedTaxonID, Tick_all$tck_taxonomyProcessed$taxonRank)# accepted TaxonID is either at family, order, or species level
  # table(Tick_all$tck_taxonomyProcessed$acceptedTaxonID, Tick_all$tck_taxonomyProcessed$scientificName)
  
  
  tax_rank <- Tick_all$tck_taxonomyProcessed %>% 
    dplyr::group_by(acceptedTaxonID) %>% 
    dplyr::summarise(
      taxonRank =  dplyr::first(taxonRank), 
      scientificName = dplyr::first(scientificName))
  
  tck_merged_final <- tck_merged_final %>% 
    dplyr::left_join(tax_rank, by = "acceptedTaxonID")
  
  # setdiff(tck_merged_final$acceptedTaxonID, tck_tax_filtered$acceptedTaxonID)
  
  data_tick <- tck_merged_final %>% 
    dplyr::filter(
      sampleCondition_field == "No known compromise") %>%
    dplyr::select(
      # -uid_field, 
      -sampleCondition_field, 
      -samplingProtocolVersion, -Species_LifeStage) %>%
    unique() %>%
    dplyr::left_join(
      dplyr::select(
        tck_tax_filtered, acceptedTaxonID, family, identificationReferences) %>%
        dplyr::distinct() %>%
        dplyr::group_by(acceptedTaxonID) %>%
        dplyr::summarise(
          family = unique(family)[1],
          identificationReferences = paste(unique(identificationReferences),
                                           collapse = "; "), .groups = "drop") %>%
        dplyr::distinct(),
      by = "acceptedTaxonID") %>%
    dplyr::distinct()
  
  # table(data_tick$plotType) # all distributed
  # table(str_sub(data_tick$namedLocation, 10)) # all tickPlot.tck
  data_tick <- dplyr::select(data_tick, -plotType) %>% dplyr::distinct()
  data_tick <- dplyr::rename(data_tick, taxonID = acceptedTaxonID)
  # NOTES FOR END USERS  #
  # be aware of higher order taxa ID -- they are individuals ASSIGNED at a higher taxonomic class, rather than the sum of lower tax. class
  # higher order taxonomic assignments could be semi-identified by looking at most likely ID (e.g. if 200 of 700 larvae are IXOSPAC, very likely that the Unidentified larvae are IXOSPAC)
  # unidentified could be changed to IXOSPP across the board
  # All combos of species-lifestage aren't in the long form (e.g. if there were never IXOAFF_Adult, it will be missing instead of 0)
  

  
  
  #location ----
  table_location_raw <- data_tick %>%
    dplyr::select(domainID, siteID, namedLocation, 
                  decimalLatitude, decimalLongitude, elevation, 
                  nlcdClass, geodeticDatum) %>%
    dplyr::distinct() 
  
  table_location <- ecocomDP::make_neon_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"))
  
  table_location_ancillary <- ecocomDP::make_neon_ancillary_location_table(
    loc_info = table_location_raw,
    loc_col_names = c("domainID", "siteID", "namedLocation"),
    ancillary_var_names = c("namedLocation", "nlcdClass", "geodeticDatum"))
  
  
  
  
  # taxon ----
  
  if("token" %in% names(dots_updated)){
    my_token <- dots_updated$token
  }else{
    my_token <- NA
  }
  
  # get tick taxon table from NEON
  neon_tick_taxon_table <- neonUtilities::getTaxonTable(
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
  
  # replace NA counts with zeroes
  data_tick$IndividualCount[is.na(data_tick$IndividualCount)] <- 0
  
  table_observation_all <- data_tick %>%
    dplyr::filter(!is.na(totalSampledArea) && totalSampledArea > 0) %>%
    dplyr::distinct() %>%
    # dplyr::rename(location_id, plotID, trapID) %>%
    dplyr::rename(location_id = namedLocation) %>%
    # package id
    dplyr::mutate(package_id = paste0(neon_method_id, ".", format(Sys.time(), "%Y%m%d%H%M%S"))) %>%
    dplyr:: rename(
      neon_event_id = eventID,
      observation_datetime = collectDate, 
      taxon_id = taxonID) %>%
    dplyr::mutate(
      observation_id = paste0("obs_",1:nrow(.)), 
      event_id = observation_id,
      neon_sample_id = sampleID,
      variable_name = "abundance",
      value = IndividualCount/totalSampledArea,
      unit = "count per square meter") 
  
  table_observation <- table_observation_all %>%
    dplyr::select(
      observation_id,
      event_id,
      package_id,
      location_id,
      observation_datetime,
      taxon_id,
      variable_name,
      value,
      unit) %>%
    dplyr::filter(!is.na(taxon_id))
  
  
  
  table_observation_ancillary <- ecocomDP::make_neon_ancillary_observation_table(
    obs_wide = table_observation_all,
    ancillary_var_names = c(
      "event_id",
      "neon_event_id",
      "neon_sample_id",
      "LifeStage",
      "samplingMethod",
      "totalSampledArea",
      "targetTaxaPresent",
      "remarks_field",
      "release",
      "identificationReferences"))
  
  # data summary ----
  # make dataset_summary -- required table
  
  years_in_data <- table_observation$observation_datetime %>% lubridate::year()
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