# Sampling locations that are polygons

In some datasets, the locations are described as polygons, eg, with a string of points in a KML file.

## Example input 
https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-cap&identifier=627


## Best Practices
1. use the location table.
1. Put the name of the polygon at the hightest level site (i.e., it has no parent site), e.g, siteId = Tonto
1. Each boundary point for the polygon is also a site. 
    1. Name them with polygonName_kmlTagName_#, e.g., Tonto_outerBoundaryIs_1
    1. Give it a parent polygonName
  
## Example location table


## Example output


## Other resources:
### KML turtorial:
https://developers.google.com/kml/documentation/kml_tut
