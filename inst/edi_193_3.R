# Package ID: edi.193.3 Cataloging System:https://pasta.edirepository.org.
# Data set title:        Ant Assemblages in Hemlock Removal Experiment at Harvard Forest since 2003 (Reformatted to ecocomDP Design Pattern)     .
# Data set creator:  Aaron Ellison -  
# Contact:  Aaron Ellison -  Harvard Forest  - aellison@fas.harvard.edu
# Stylesheet v2.7 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/193/3/457eba43b5db78121cbc8188cc3069fd" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="auto")

 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "observation_id",     
                    "event_id",     
                    "package_id",     
                    "location_id",     
                    "observation_datetime",     
                    "taxon_id",     
                    "variable_name",     
                    "value",     
                    "unit"    ), check.names=TRUE, as.is = TRUE)
               
  
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
# if (class(dt1$observation_id)!="factor") dt1$observation_id<- as.factor(dt1$observation_id)
# if (class(dt1$event_id)!="factor") dt1$event_id<- as.factor(dt1$event_id)
# if (class(dt1$package_id)!="factor") dt1$package_id<- as.factor(dt1$package_id)
# if (class(dt1$location_id)!="factor") dt1$location_id<- as.factor(dt1$location_id)                                   
# # attempting to convert dt1$observation_datetime dateTime string to R date structure (date or POSIXct)                                
# tmpDateFormat<-"%Y-%m-%d"
# tmp1observation_datetime<-as.Date(dt1$observation_datetime,format=tmpDateFormat)
# # Keep the new dates only if they all converted correctly
# if(length(tmp1observation_datetime) == length(tmp1observation_datetime[!is.na(tmp1observation_datetime)])){dt1$observation_datetime <- tmp1observation_datetime } else {print("Date conversion failed for dt1$observation_datetime. Please inspect the data and do the date conversion yourself.")}                                                                    
# rm(tmpDateFormat,tmp1observation_datetime) 
# if (class(dt1$taxon_id)!="factor") dt1$taxon_id<- as.factor(dt1$taxon_id)
# if (class(dt1$variable_name)!="factor") dt1$variable_name<- as.factor(dt1$variable_name)
# if (class(dt1$value)=="factor") dt1$value <-as.numeric(levels(dt1$value))[as.integer(dt1$value) ]
# if (class(dt1$unit)!="factor") dt1$unit<- as.factor(dt1$unit)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(observation_id)
summary(event_id)
summary(package_id)
summary(location_id)
summary(observation_datetime)
summary(taxon_id)
summary(variable_name)
summary(value)
summary(unit) 
detach(dt1)               
         

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/193/3/f4e1863671aab184c53f43c85db83c31" 
infile2 <- tempfile()
download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "observation_ancillary_id",     
                    "event_id",     
                    "variable_name",     
                    "value",     
                    "unit"    ), check.names=TRUE, as.is = TRUE)
               
  
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt2$observation_ancillary_id)!="factor") dt2$observation_ancillary_id<- as.factor(dt2$observation_ancillary_id)
# if (class(dt2$event_id)!="factor") dt2$event_id<- as.factor(dt2$event_id)
# if (class(dt2$variable_name)!="factor") dt2$variable_name<- as.factor(dt2$variable_name)
# if (class(dt2$value)!="factor") dt2$value<- as.factor(dt2$value)
# if (class(dt2$unit)!="factor") dt2$unit<- as.factor(dt2$unit)
#                 
# # Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(observation_ancillary_id)
summary(event_id)
summary(variable_name)
summary(value)
summary(unit) 
detach(dt2)               
         

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/193/3/c70ba7e3149e78355d0bb60620d97569" 
infile3 <- tempfile()
download.file(inUrl3,infile3,method="auto")

                   
 dt3 <-read.csv(infile3,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "location_ancillary_id",     
                    "location_id",     
                    "datetime",     
                    "variable_name",     
                    "value",     
                    "unit"    ), check.names=TRUE, as.is = TRUE)
               
  
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt3$location_ancillary_id)!="factor") dt3$location_ancillary_id<- as.factor(dt3$location_ancillary_id)
# if (class(dt3$location_id)!="factor") dt3$location_id<- as.factor(dt3$location_id)                                   
# # attempting to convert dt3$datetime dateTime string to R date structure (date or POSIXct)                                
# tmpDateFormat<-"%Y-%m-%d"
# tmp3datetime<-as.Date(dt3$datetime,format=tmpDateFormat)
# # Keep the new dates only if they all converted correctly
# if(length(tmp3datetime) == length(tmp3datetime[!is.na(tmp3datetime)])){dt3$datetime <- tmp3datetime } else {print("Date conversion failed for dt3$datetime. Please inspect the data and do the date conversion yourself.")}                                                                    
# rm(tmpDateFormat,tmp3datetime) 
# if (class(dt3$variable_name)!="factor") dt3$variable_name<- as.factor(dt3$variable_name)
# if (class(dt3$value)!="factor") dt3$value<- as.factor(dt3$value)
# if (class(dt3$unit)!="factor") dt3$unit<- as.factor(dt3$unit)
#                 
# # Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(location_ancillary_id)
summary(location_id)
summary(datetime)
summary(variable_name)
summary(value)
summary(unit) 
detach(dt3)               
         

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/193/3/18423b91b5e8fd6e771f25ab5b55c9ef" 
infile4 <- tempfile()
download.file(inUrl4,infile4,method="auto")

                   
 dt4 <-read.csv(infile4,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "taxon_ancillary_id",     
                    "taxon_id",     
                    "datetime",     
                    "variable_name",     
                    "value",     
                    "author"    ), check.names=TRUE, as.is = TRUE)
               
  
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt4$taxon_ancillary_id)!="factor") dt4$taxon_ancillary_id<- as.factor(dt4$taxon_ancillary_id)
# if (class(dt4$taxon_id)!="factor") dt4$taxon_id<- as.factor(dt4$taxon_id)                                   
# # attempting to convert dt4$datetime dateTime string to R date structure (date or POSIXct)                                
# tmpDateFormat<-"%Y-%m-%d"
# tmp4datetime<-as.Date(dt4$datetime,format=tmpDateFormat)
# # Keep the new dates only if they all converted correctly
# if(length(tmp4datetime) == length(tmp4datetime[!is.na(tmp4datetime)])){dt4$datetime <- tmp4datetime } else {print("Date conversion failed for dt4$datetime. Please inspect the data and do the date conversion yourself.")}                                                                    
# rm(tmpDateFormat,tmp4datetime) 
# if (class(dt4$variable_name)!="factor") dt4$variable_name<- as.factor(dt4$variable_name)
# if (class(dt4$value)!="factor") dt4$value<- as.factor(dt4$value)
# if (class(dt4$author)!="factor") dt4$author<- as.factor(dt4$author)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(taxon_ancillary_id)
summary(taxon_id)
summary(datetime)
summary(variable_name)
summary(value)
summary(author) 
detach(dt4)               
         

inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/193/3/a120704d15c5a3699e569e6fc618f3f7" 
infile5 <- tempfile()
download.file(inUrl5,infile5,method="auto")

                   
 dt5 <-read.csv(infile5,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "package_id",     
                    "original_package_id",     
                    "length_of_survey_years",     
                    "number_of_years_sampled",     
                    "std_dev_interval_betw_years",     
                    "max_num_taxa",     
                    "geo_extent_bounding_box_m2"    ), check.names=TRUE, as.is = TRUE)
               
  
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt5$package_id)!="factor") dt5$package_id<- as.factor(dt5$package_id)
# if (class(dt5$original_package_id)!="factor") dt5$original_package_id<- as.factor(dt5$original_package_id)
# if (class(dt5$length_of_survey_years)=="factor") dt5$length_of_survey_years <-as.numeric(levels(dt5$length_of_survey_years))[as.integer(dt5$length_of_survey_years) ]
# if (class(dt5$number_of_years_sampled)=="factor") dt5$number_of_years_sampled <-as.numeric(levels(dt5$number_of_years_sampled))[as.integer(dt5$number_of_years_sampled) ]
# if (class(dt5$std_dev_interval_betw_years)!="factor") dt5$std_dev_interval_betw_years<- as.factor(dt5$std_dev_interval_betw_years)
# if (class(dt5$max_num_taxa)=="factor") dt5$max_num_taxa <-as.numeric(levels(dt5$max_num_taxa))[as.integer(dt5$max_num_taxa) ]
# if (class(dt5$geo_extent_bounding_box_m2)=="factor") dt5$geo_extent_bounding_box_m2 <-as.numeric(levels(dt5$geo_extent_bounding_box_m2))[as.integer(dt5$geo_extent_bounding_box_m2) ]
#                 
# Convert Missing Values to NA for non-dates
                
# dt5$geo_extent_bounding_box_m2 <- ifelse((trimws(as.character(dt5$geo_extent_bounding_box_m2))==trimws("NA")),NA,dt5$geo_extent_bounding_box_m2)


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(package_id)
summary(original_package_id)
summary(length_of_survey_years)
summary(number_of_years_sampled)
summary(std_dev_interval_betw_years)
summary(max_num_taxa)
summary(geo_extent_bounding_box_m2) 
detach(dt5)               
         

inUrl6  <- "https://pasta.lternet.edu/package/data/eml/edi/193/3/e5c32a1dcbd24a310d8bb7e17dc37b5e" 
infile6 <- tempfile()
download.file(inUrl6,infile6,method="auto")

                   
 dt6 <-read.csv(infile6,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "location_id",     
                    "location_name",     
                    "latitude",     
                    "longitude",     
                    "elevation",     
                    "parent_location_id"    ), check.names=TRUE, as.is = TRUE)
               
  
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt6$location_id)!="factor") dt6$location_id<- as.factor(dt6$location_id)
# if (class(dt6$location_name)!="factor") dt6$location_name<- as.factor(dt6$location_name)
# if (class(dt6$latitude)=="factor") dt6$latitude <-as.numeric(levels(dt6$latitude))[as.integer(dt6$latitude) ]
# if (class(dt6$longitude)=="factor") dt6$longitude <-as.numeric(levels(dt6$longitude))[as.integer(dt6$longitude) ]
# if (class(dt6$elevation)=="factor") dt6$elevation <-as.numeric(levels(dt6$elevation))[as.integer(dt6$elevation) ]
# if (class(dt6$parent_location_id)!="factor") dt6$parent_location_id<- as.factor(dt6$parent_location_id)
#                 
# # Convert Missing Values to NA for non-dates
#                 
# dt6$latitude <- ifelse((trimws(as.character(dt6$latitude))==trimws("NA")),NA,dt6$latitude)
# dt6$longitude <- ifelse((trimws(as.character(dt6$longitude))==trimws("NA")),NA,dt6$longitude)
# dt6$elevation <- ifelse((trimws(as.character(dt6$elevation))==trimws("NA")),NA,dt6$elevation)


# Here is the structure of the input data frame:
str(dt6)                            
attach(dt6)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(location_id)
summary(location_name)
summary(latitude)
summary(longitude)
summary(elevation)
summary(parent_location_id) 
detach(dt6)               
         

inUrl7  <- "https://pasta.lternet.edu/package/data/eml/edi/193/3/17105526a66317756cc65eed4f5aba28" 
infile7 <- tempfile()
download.file(inUrl7,infile7,method="auto")

                   
 dt7 <-read.csv(infile7,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "taxon_id",     
                    "taxon_rank",     
                    "taxon_name",     
                    "authority_system",     
                    "authority_taxon_id"    ), check.names=TRUE, as.is = TRUE)
               
  
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#                 
# if (class(dt7$taxon_id)!="factor") dt7$taxon_id<- as.factor(dt7$taxon_id)
# if (class(dt7$taxon_rank)!="factor") dt7$taxon_rank<- as.factor(dt7$taxon_rank)
# if (class(dt7$taxon_name)!="factor") dt7$taxon_name<- as.factor(dt7$taxon_name)
# if (class(dt7$authority_system)!="factor") dt7$authority_system<- as.factor(dt7$authority_system)
# if (class(dt7$authority_taxon_id)!="factor") dt7$authority_taxon_id<- as.factor(dt7$authority_taxon_id)
#                 
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt7)                            
attach(dt7)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(taxon_id)
summary(taxon_rank)
summary(taxon_name)
summary(authority_system)
summary(authority_taxon_id) 
detach(dt7)               
        




