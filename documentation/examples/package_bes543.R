
# This script converts the parent data package knb-lter-bes.543.170 into the
# ecocomDP tables, validates the tables, defines categorical variables and
# creates EML for the ecocomDP tables.

# Load dependencies

library(ecocomDP)

# Arguments

path <- 'C:/Users/Colin/Documents/EDI/data_sets/ecocomDP/edi_191'
parent_pkg_id <- 'knb-lter-bes.543.170'
child_pkg_id <- 'edi.191.3'

# Convert knb-lter-bes.543.x to ecocomDP

message('Creating ecocomDP tables')

convert_bes543_to_ecocomDP(path, parent_pkg_id, child_pkg_id)

# Validate ecocomDP tables

message('Validating ecocomDP tables')

validate_data(
  data.path = path
)

# Define variables found in the ecocomDP tables

message('Defining ecocomDP variables')

catvars <- define_variables(
  data.path = path,
  parent.pkg.id = parent_pkg_id
)

use_i <- catvars$code == 'bird_count'
catvars$definition[use_i] <- 'Number of birds counted at a point in a bird survey'
catvars$unit[use_i] <- 'number'

use_i <- catvars$code == 'survey_date'
catvars$definition[use_i] <- 'Dat of bird survey'

use_i <- catvars$code == 'observer'
catvars$definition[use_i] <- 'Code to identify observers'

use_i <- catvars$code == 'wind'
catvars$definition[use_i] <- 'Wind speed (mph)'
catvars$unit[use_i] <- 'meter'

use_i <- catvars$code == 'air_temp_F'
catvars$definition[use_i] <- 'Temperature in F'
catvars$unit[use_i] <- 'degree'



# Add contact information for the creator of this script

additional_contact <- data.frame(
  givenName = 'Colin',
  surName = 'Smith',
  organizationName = 'Environmental Data Initiative',
  electronicMailAddress = 'colin.smith@wisc.edu',
  stringsAsFactors = FALSE
)

# Make EML

message('Creating EML')

create_eml(
  data.path = path,
  code.path = path,
  code.files = 'convert_bes543_to_ecocomDP.R',
  parent.package.id = parent_pkg_id,
  child.package.id = child_pkg_id,
  sep = ',',
  cat.vars = catvars,
  user.id = 'csmith',
  affiliation = 'LTER',
  additional.contact = additional_contact
)

