# An example workflow for the data user

# Get a list of all ecocomDPs available from EDI and NEON
ecocomDP_list <- view_all_ecocomDP()

# Browse data package landing pages to identify ecocomDPs of interest
view_landing_page('edi.124.3')
view_landing_page('edi.191.2')
view_landing_page('knb-lter-ntl.345.3')
view_landing_page('DP1.20120.001')

# Aggregate select ecocomDPs and export to path
data <- aggregate_ecocomDP(
  package.ids = c('edi.124.3', 
                  'edi.191.2', 
                  'knb-lter-ntl.345.3', 
                  'DP1.20120.001'),
  neon.sites = c('MAYF', 'PRIN', 'CRAM'),
  path = '/Users/csmith/Downloads/ecocomDP_exports'
  )



