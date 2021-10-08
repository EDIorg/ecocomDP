
path_tbls <- "NEON_dev_script/edi_193"
d <- ecocomDP::read_data(from.file = path_tbls)
my_list_of_tables <- d[[1]]$tables
df <- ecocomDP::flatten_data(my_list_of_tables)

list2env(my_list_of_tables, .GlobalEnv)
