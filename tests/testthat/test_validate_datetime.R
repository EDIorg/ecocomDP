context('Datetime format should match the standard.')

library(ecocomDP)

# Load data -------------------------------------------------------------------

tables <- list.files(
  paste0(path.package('ecocomDP'), '/tests/test_data')
)

data.path <- paste0(path.package('ecocomDP'), '/tests/test_data')

criteria <- read.table(
  system.file('validation_criteria.txt', package = 'ecocomDP'),
  header = T,
  sep = "\t",
  as.is = T,
  na.strings = "NA")

L1_table_names <- criteria$table[is.na(criteria$column)]

data.list <- lapply(tables, read_ecocomDP_table, data.path = data.path)
names(data.list) <- unlist(lapply(tables, is_table_rev, L1_table_names))

criteria_2 <- criteria[
  ((complete.cases(criteria$column)) & 
     ((criteria$column == 'datetime') | (criteria$column == 'observation_datetime'))), ]

# # Datetime format is invalid --------------------------------------------------
# 
# testthat::test_that('Invalid datetime formats result in error.', {
#   expect_error(
#     is_datetime_format(
#       L1.table = criteria_2$table[2],
#       tables = 'Project_name_location_ancillary.csv', 
#       data.list = data.list,
#       criteria = criteria_2
#       )
#   )
# })

# Datetime format is valid ----------------------------------------------------

# Fix datetime issues in location_ancillary
data.list <- d
x <- data.list$location_ancillary$datetime
x <- dataCleanr::iso8601_convert(
  x = x,
  orders = c('ymd', 'dmy')
  )
data.list$location_ancillary$datetime <- x

# Fix datetime issues in observation

x <- data.list$observation$observation_datetime
x <- dataCleanr::iso8601_convert(
  x = x,
  orders = c('ymd', 'dmy')
)
data.list$observation$observation_datetime <- x

d <- data.list

data.table::fwrite(data.list$observation, paste0(readClipboard(), "/observation.csv"))

data.table::fwrite(data.list$location_ancillary, paste0(readClipboard(), "/location_ancillary.csv"))

# Expect message

testthat::test_that('Valid datetime formats result in message.', {
  expect_message(
    validate_datetime(
      tables = tables,
      data.list = data.list,
      criteria = criteria
    )
  )
})

