.onLoad <- function(libname = find.package("ecocomDP"), pkgname = "ecocomDP") {
  
  #' @importFrom magrittr "%>%"
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      c(
        ".","..nms", "COUNT_PENDING", "DATE", "Discrepancy", "Domain Name", "Domain Number",
        "IndividualCount", "OBSERVATION_TYPE", "SITE_ID", "Species_LifeStage",
        "Total_Lab_Adult", "Total_Lab_Larva", "Total_Lab_Nymph", "VALUE", "VARIABLE_NAME",
        "VARIABLE_UNITS", "abundance", "acceptedTaxonID", "additionalCoordUncertainty",
        "adjCountPerBottle", "adjTrappingDays", "adultCount", "algalParameterUnit",
        "analysisType", "anno_ls", "aquaticSiteType", "archiveIDCode.x", "archiveIDCode.y",
        "argument_issues", "authority_system", "benthicArea", "bout", "boutID", "boutNumber",
        "catch_per_effort", "cell_density_standardized_unit", "clusterSize",
        "collectDate", "column", "context", "coordinateUncertainty", "dataQF", "dataQF_field",
        "dataQF_tax", "datetime", "day", "decimalLatitude", "decimalLongitude", "density",
        "density_unit", "diffTrappingDays", "divDataType", "domainID", "elevation",
        "elevationUncertainty", "eml_L2", "endCollectDate", "endDate",
        "estPerBottleSampleVolume", "estimatedTotalCount", "eventID", "eventID_raw",
        "event_id", "family", "fieldSampleVolume", "fixedRandomReach", "geodeticDatum",
        "habitatType", "heightPlantOver300cm", "heightPlantSpecies",
        "identificationReferences", "identificationSource", "identifiedBy",
        "identifiedDate", "index", "individualCount", "individualCount_sum", "individualID",
        "key2", "key3", "labSampleVolume", "laboratoryName", "laboratoryName.x",
        "laboratoryName.y", "larvaCount", "lifeStage", "location_ancillary_id",
        "location_id", "location_name", "location_type", "location_type_parent",
        "mean_efishtime", "measuredBy", "month", "n_ided", "n_obs", "n_positive_tests", "n_tests",
        "n_trap_nights_per_bout_per_plot", "n_trap_nights_per_night_uid",
        "nameAccordingToID", "namedLocation", "nativeStatusCode", "nativeStatusCode.x",
        "nativeStatusCode.y", "neon_sample_id", "netDeploymentTime", "netEndTime",
        "netSetTime", "nightuid", "nlcdClass", "no.site", "no.taxa", "number_of_fish", "numbr",
        "nymphCount", "object_label", "object_uri", "observation_ancillary_id",
        "observation_id", "occurrence", "package_id", "parentSampleID",
        "parent_location_id", "percentCover", "plotID", "plotType", "ponarDepth",
        "pos_result_count", "pregnancyStatus", "preservativeVolume", "primaryKey",
        "publicationDate", "publicationDate.x", "raw_count", "reachID", "release", "release.x",
        "remarks", "remarks_tax", "rnames", "sampleCode.x", "sampleCode.y", "sampleCollected",
        "sampleCondition", "sampleCondition.x", "sampleCondition.y", "sampleID",
        "sampleType", "sample_area_m2", "samplerType", "samplingImpractical",
        "samplingImpracticalRemarks", "samplingProtocolVersion", "scientificName",
        "scientificName.x", "scientificName.y", "setDate", "sex", "sexOrAge", "shared", "site1",
        "site2", "siteID", "snagDiameter", "snagLength", "sortDate", "startCollectDate",
        "startDate", "subject", "subplotID", "subplot_id", "subsampleCode.x",
        "subsampleCode.y", "subsampleID", "subsamplePercent", "subsampleType",
        "subsampleWeight", "substratumSizeClass", "summary_data_edi",
        "summary_data_neon", "table_name", "tagID", "tailLength", "targetTaxaPresent", "taxa",
        "taxonID", "taxonID.x", "taxonID.y", "taxonRank", "taxonRank.x", "taxonRank.y",
        "taxon_ancillary_id", "taxon_id", "taxon_name", "taxon_rank", "template_issues",
        "testPathogenName", "testResult", "todrop", "totalFieldCount", "totalLabCount",
        "totalLength", "totalSampledArea", "totalSerialBouts", "totalWeight",
        "towsTrapsVolume", "trapCoordinate", "trapHours", "trapID", "trapStatus",
        "trappingDays", "uid", "unidentifiedCount", "value", "variable_name", "weight", "year",
        "zooMaximumLength", "zooMeanLength", "zooMinimumLength", "annotation_dictionary_table",
        "n_recs", "individualCount_corrected", "proportionIdentified", "estimated_totIndividuals",
        "taxonProtocolCategory", "uid_pertrapnight", "release_pertrapnight", "publicationDate_pertrapnight",
        "n_agg_count_vals", "n_recs", "density_aggregate")
    )
  
  
  invisible(NULL)
}
