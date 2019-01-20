# Measurement vocabulary resources

Resources for linking a variable_name used in a data table to an external definition.

### Resources:
* [DarwinCore](https://dwc.tdwg.org/terms/)
* [The Ecosystem Ontology](https://bioportal.bioontology.org/ontologies/ECSO/?p=summary)
* [Combee](http://www.ontobee.org)

### Example usage:

Use DarwinCore to define a variable_name "count"

| variable_mapping_id | table_name | variable_name | mapped_system | mapped_id | mapped_label |
|---------------------|------------|---------------|---------------|-----------|--------------|
| va_1 | observation | count | DarwinCore | [http://rs.tdwg.org/dwc/terms/individualCount](http://rs.tdwg.org/dwc/terms/individualCount) | individualCount | 


Use ECSO to define a variable_name "biomass"

| variable_mapping_id | table_name | variable_name | mapped_system | mapped_id | mapped_label |
|---------------------|------------|---------------|---------------|-----------|--------------|
| va_1 | observation | biomass | ECSO | [http://purl.dataone.org/odo/ECSO_00000513](http://purl.dataone.org/odo/ECSO_00000513) | Biomass Measurement Type | 


Use Ontobee to search for an ontology.
