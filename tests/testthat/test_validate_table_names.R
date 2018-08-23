context('Table names should be valid.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

criteria <- read.table(
  system.file('validation_criteria.txt', package = 'ecocomDP'),
  header = T,
  sep = "\t",
  as.is = T,
  na.strings = "NA")

valid_table_names <- c("Project_name_dataset_summary.csv",
                       "Project_name_location.csv",
                       "Project_name_location_ancillary.csv",
                       "Project_name_observation.csv",
                       "Project_name_observation_ancillary.csv",
                       "Project_name_taxon.csv",
                       "Project_name_taxon_ancillary.csv",
                       "Project_name_variable_mapping.csv")

# Valid tables ----------------------------------------------------------------

testthat::test_that('Valid tables result in a corresponding list of table names.', {
  
  expect_equal(
    validate_table_names(
      paste0(path.package('ecocomDP'), '/tests/test_data'),
      criteria),
    valid_table_names
  )
  
})
