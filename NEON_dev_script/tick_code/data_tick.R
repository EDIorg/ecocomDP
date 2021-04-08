#' Ticks sampled using drag cloths
#'
#' This dataset was derived from [NEON data portal](https://data.neonscience.org) with data product ID 'DP1.10093.001'. Details about this data product can be found at <https://data.neonscience.org/data-products/DP1.10093.001>.
#'
#' Here, we:
#' 1. Cleaned taxonomic data: First, we removed taxonomic samples that had low sample quality (`sampleCondition` != "OK"), or did not have a taxonomy assigned (!is.na(`acceptedTaxonID`)). We then simplified the taxonomic data by creating a `lifeStage` column: this was either Nymph, Larvae, or Adult. Any tick that was sexed (e.g. `sexOrAge` was Male or Female) was assigned to the Adult lifestage. The Nymph or Larvae assignments were taken from the `sexOrAge` column. We dropped the Sex information and summed M and F counts into a total adult count (i.e. we grouped by `sampleID`, `acceptedTaxonID`, and `lifeStage` and summed counts). Finally, we converted the taxonomic data into "wide" format for joining with field data. In the wide format, each column is a unique taxonomicID_lifestage and each row is a sample.
#' 2. Cleaned the field data: First, we removed field surveys that had logistical issues (`samplingImpractical`!="OK"). We verified that all field surveys where ticks were located also had an associated `sampleID`. 
#' 3. Merged the field and taxonomic data. Specifically we left-joined the tick field data to tick taxonomy data using the `sampleID`. This retains 0 counts from the field data which will not have a record in the taxonomy table. For any record where no ticks were found in the field (`targetTaxaPresent`=="N"), we assigned 0s to all taxonomicID_lifestage columns.
#' 4. Fixed count discrepancies between field and lab (note that as of 2019 counts may ONLY come from lab so this may not be necessary in future). The lab count (which was taxon-specific) was used as the source of final counts per species-lifestage. When discrepancies were minor we trusted the lab counts. When discrepancies were larger, we used the following decisions:
#' 4a. In cases where the field count total was greater than the taxonomic count total AND the discrepancy was in the larval stages, we assumed the lab stopped ID'ing after a certain count. We added the additional, un-ID'd "field" larvae to `IXOSP2_Larva` (the highest taxonomic ID)
#' 4b. In other cases where field count was greater than the taxonomic count and there were `remarks_field` we assumed the remarks were about lost ticks (a common remark). We assigned any un-identified ticks to IXOSP2.
#' 4c. Some counts did not match because there were too many samples sent to the lab and the invoice limit was reached. If the taxonomy table contained remarks about "invoice limit" or "billing limit" for a given sample ID, we trusted the field counts and added any difference between field and lab counts to the IXOSP2 column. *Note this was a conservative decision but one could reasonably assume that counts assigned to IXOSP2 would really belong to whatever lower order taxonomy was commonly ID'd for the remaining samples.*.
#' 4d. Removed any remaining samples where field and lab counts were off by >20 percet and there was no obvious explanation. (<1 percent of total records)
#' 5. Added a column to designate whether ticks were found in the field but counts and IDs were still pending (`COUNT_PENDING`). This will apply to the most recent data, where taxonomists instead of field surveyors perform the counts. Field surveys where no ticks were found are infilled with 0s in all of the count columns, but counts are still NA for samples where ticks were observed. Users of the dataset should consider filtering the data to just those years where there are no counts pending (instead of just removing NAs, which will bias the data towards low abundances).
#' 5. Created a long-form dataset and re-formatted data into the ecocomDP formatting. 
#' 
#' @note Details of locations (e.g. latitude/longitude coordinates can be found in [neon_locations]).
#'
#' @format A data frame (also a tibble) with the following columns:
#'
#' - `namedLocation`: Name of the measurement location in the NEON database.
#' - `siteID`: NEON site code.
#' - `plotID`: Plot identifier (NEON site code_XXX).
#' - `collectDate`: Date of the collection event.
#' - `eventID`: An identifier for the set of information associated with the event, which includes information about the place and time of the event.
#' - `sampleID`: Identifier for sample.
#' - `sampleCode`: Barcode of a sample.
#' - `samplingMethod`: Name or code for the method used to collect or test a sample.
#' - `totalSampledArea`: Total area sampled (square Meter).
#' - `targetTaxaPresent`: Indicator of whether the sample contained individuals of the target taxa ('Y' or 'N').
#' - `remarks_field`: Technician notes; free text comments accompanying the record.
#' - `taxonID`: Accepted species code, based on one or more sources. This is renamed from the `acceptedTaxonID` column so that column names are consistent across different taxonomic groups.
#' - `LifeStage`: Life stage of the sample ('Adult', 'Larva', or 'Nymph').
#' - `IndividualCount`: Number of individuals of the same type.
#' - `CountPending`: Whether counts are still being processed from taxonomy lab ('Y' or 'N'). Consider filtering to years where all counts are available (CountPending=='Y')
#' - `taxonRank`: The lowest level taxonomic rank that can be determined for the individual or specimen.
#' - `scientificName`: Scientific name, associated with the taxonID. This is the name of the lowest level taxonomic rank that can be determined.
#'
#' @author Wynne Moss, Melissa Chen, Brendan Hobart, Matt Bitters
#'
"data_tick"
