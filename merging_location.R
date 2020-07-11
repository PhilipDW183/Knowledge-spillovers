library(sqldf)
library(tidyr)
library(tidyverse)
library(rgdal)
library(tmap)
library(rgeos)
library(knitr)
library(tmap)
library(leafpop)
library(leaflet)
library(tmaptools)
library(tidyverse)
library(classInt)
library(RColorBrewer)
library(geojsonio)
library(plyr)
library(sf)
library(sp)
library(raster)

#The working directory is checked
getwd()

#the working directory is set to one that makes it easier to collect outputs and inputs
setwd("C:/Users/cex/Documents/Smart Cities and Urban Analytics/Dissertation/Data")


#The first attempt was to extract city names from the patent data and use OSM, Google maps or SQL merges to merge the results based on city level data
#https://stackoverflow.com/questions/38574695/sql-server-2008-merge-statement-multiple-match-conditions
#https://stackoverflow.com/questions/54637189/merging-two-tables-where-one-column-is-substring-of-the-other-in-r

data <- read_csv("Patents data/EPO_citing_for_OSM.csv")

EPO_ind_cited <- read_csv("Patents data/EPO_ind_cited_Address.csv")

data_subset <- data[c(1:1000), c(1:10)]

cities <- read_csv("Patents data/cities1000.csv")

cities = cities[-c(19,18,17,16,15,14,13,12,11,10,8,7)]


l <- strsplit(as.character(data$address), ",")
df1new <- data.frame(address = unlist(l),
                     address.string = rep(data$address, lengths(l)))
merge(cities, df1new, by_x = "name", by_y = "address", all.x = TRUE)

merged <- sqldf(' 
      select * from data one
      left join cities two
      on one.address like "%" || two.asciiname || "%" AND one.ctry_code = country_code
      ')

#write.csv(merged, "Patents data/EPO_citing_with_city.csv")

EPO_ind_cited_merged <- sqldf('
                              select * from EPO_ind_cited one
                              left join cities two
                              on one.address like "%" || two.asciiname || "%" AND one.ctry_code = country_code')

EPO_ind_cited_merged = EPO_ind_cited_merged[-c(32,31,30,29,28,27,26,25,24,23,21,20)]

#write.csv(EPO_ind_cited_merged, "Patents data/EPO_cited_with_city.csv")

EPO_ind_citing = read_csv("Patents data/EPO_ind_citing_for_cities.csv")

EPO_ind_citing_merged <- sqldf(' 
      select * from EPO_ind_citing one
      left join cities two
      on one.address like "%" || two.asciiname || "%" AND one.ctry_code = country_code
      ')

EPO_ind_citing_merged = EPO_ind_citing_merged[-c(31,30,29,28,27,26,25,24,23,22,20,19)]

#write.csv(EPO_ind_citing_merged, "Patents data/EPO_citing_with_city.csv")


#now doing the controls
EPO_ind_ctrl <- read_csv("Patents data/EPO_ind_ctrl_waddress.csv")

EPO_ind_ctrl_PCT <- read_csv("Patents data/EPO_ind_ctrlPCT_waddress.csv")

EPO_ind_ctrl_merged <- sqldf(' 
      select * from EPO_ind_ctrl one
      left join cities two
      on one.address like "%" || two.asciiname || "%" AND one.ctry_code = country_code
      ')

EPO_ind_ctrl_PCT_merged <- sqldf(' 
      select * from EPO_ind_ctrl_PCT one
      left join cities two
      on one.address like "%" || two.asciiname || "%" AND one.ctry_code = country_code
      ')

EPO_ind_ctrl_merged = EPO_ind_ctrl_merged[-c(30,29,28,27,26,25,24,23,22,20,19)]

EPO_ind_ctrl_PCT_merged = EPO_ind_ctrl_PCT_merged[-c(30,29,28,27,26,25,24,23,22,20,19)]

#write.csv(EPO_ind_ctrl_merged, "Patents data/EPO_ind_ctrl_with_city.csv")

#write.csv(EPO_ind_ctrl_PCT_merged, "Patents data/EPO_ind_ctrl_PCT_with_city.csv")


#The second (more successful) attempt used the regions specified in the OECD REGPAT database to extract centroids and hence use these to calculate the distance between patents

#The OECD regions were found from:
#https://gis.stackexchange.com/questions/147538/where-can-i-find-shapefiles-for-oecd-regions
#https://www.dropbox.com/sh/aqfzuofxocv6zgl/AABFWmuLByljQrvY1eXP8em8a?dl=0 FOR TL2 and TL3 shapefiles

#tl2 specification for regions
tl2 <- readOGR("shapefiles/OECD_TL2_2020.shp")
plot(tl2)

#tl3 specification for regions
tl3 <- readOGR("shapefiles/OECD_TL3_2020.shp")
plot(tl3)

#US counties were extracted from:
#https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
US_counties <- readOGR("shapefiles/cb_2013_us_county_5m.shp")
#Plot The result to make sure it is correct
plot(US_counties)
#extract the centroids from the US counties
US_2013_centroids =gCentroids(US_counties, byid= TRUE)

#European NUTS boundaries were extracted from:
#https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
Europe_2013 <- readOGR("shapefiles/NUTS_RG_01M_2013_4326_LEVL_3.shp/NUTS_RG_01M_2013_4326_LEVL_3.shp")
#plot the shapefile to check its shape
plot(Europe_2013)
#Extract the EU centroids
EU_2013_centroids = gCentroid(Europe_2013, byid = TRUE)
#plot the results
plot(EU_centroids)
#However we don't want the UK so subset where CNTR_code is not UK
Europe_2013_noUK = subset(Europe_2013, CNTR_CODE !="UK")
#plot the results
plot(Europe_2013_noUK)

Europe_2010 <- readOGR("shapefiles/NUTS_RG_01M_2010_4326_LEVL_3.shp/NUTS_RG_01M_2010_4326_LEVL_3.shp")
#For Europe 2010 only the UK is needed
#Thus subset on:
UK_2010 = subset(Europe_2010, CNTR_CODE=="UK")
#plot the UK to make sure it is correct
plot(UK_2010)
#Then the centroids for the UK can be extracted at the NUTS3 level
UK_centroids = gCentroid(UK_2010, byid = TRUE, id= NUTS_ID)

#The issue with these however are the several columns that are retained that we don't want and/or need

#UK_2010_SF <- st_as_sf(UK_2010)
#UK_2010_centroids <- st_centroid(UK_2010_SF)
#join <- join(UK_2010_SF, UK_2010_centr#oids)

#The first attempt was to extract only the NUTS_ID when getting centroids
UK_2010_centroids <- SpatialPointsDataFrame(gCentroid(UK_2010, byid=TRUE),UK_2010@data["NUTS_ID"],  match.ID=FALSE)

#The second attempt used all the data when getting centroids
UK_2010_centroids2 <- SpatialPointsDataFrame(gCentroid(UK_2010, byid=TRUE),UK_2010@data,  match.ID=FALSE)
#Then create a list of columns that we wanted to drop
UK_drops <- c("LEVL_CODE", "NUTS_NAME", "FID")
#Then remove those columns from the resulting dataframe
UK_2010_centroids2 <- UK_2010_centroids2[,!(names(UK_2010_centroids2) %in% UK_drops)]

#The same process was then repeated for the European centroids datframe
EUROPE_2013_centroids <- SpatialPointsDataFrame(gCentroid(Europe_2013_noUK, byid=TRUE),Europe_2013_noUK@data["NUTS_ID"], match.ID=FALSE)
EUROPE_2013_centroids2 <- SpatialPointsDataFrame(gCentroid(Europe_2013_noUK, byid=TRUE),Europe_2013_noUK@data, match.ID=FALSE)
EU_drops <- c("LEVL_CODE", "NUTS_NAME", "FID")
EUROPE_2013_centroids2 <- EUROPE_2013_centroids2[,!(names(EUROPE_2013_centroids2) %in% EU_drops)]

#For the US, GEOID was used as the regional code
US_2013_centroids <- SpatialPointsDataFrame(gCentroid(US_counties, byid=TRUE),US_counties@data["GEOID"], match.ID=FALSE)
#The country code could be assigned as US since it is all one country
US_2013_centroids$CNTR_CODE <- "US"
#The second version used all the data
US_2013_centroids2 <- SpatialPointsDataFrame(gCentroid(US_counties, byid=TRUE),US_counties@data, match.ID=FALSE)

#The same process was used for TL3 centroids
TL3_centroids <- SpatialPointsDataFrame(gCentroid(tl3, byid=TRUE),tl3@data["tl3_id"], match.ID=FALSE)

TL3_centroids2 <- SpatialPointsDataFrame(gCentroid(tl3, byid=TRUE),tl3@data, match.ID=FALSE)
TL3_drops = c("tl2_id", "tl1_id", "name_or", "name_en", "name_fr", "continenta", "reg_name_p")
TL3_centroids2 <- TL3_centroids2[,!(names(TL3_centroids2) %in% TL3_drops)]
#Only countries who were in the OECD REGPAT classification as occuring under TL3 regional classification were required
TL3_countries <- c("AUS", "CAN", "CHL", "JPN", "KOR", "MEX", "NZL", "CHN", "IND", "RUS", "SAU", "ZAF")
#Thus we get a subset of the countries (although some countries were missing)
TL3_centroids_small <- TL3_centroids2[TL3_centroids2$iso3 %in% TL3_countries,]

#For TL2 specification, only ISR was requried 
TL2_ISR = subset(tl2, iso3=="ISR")
#The same process was repeated
TL2_centroids <- SpatialPointsDataFrame(gCentroid(tl2, byid=TRUE),tl2@data["tl2_id"], match.ID=FALSE)

TL2_centroids2 <- SpatialPointsDataFrame(gCentroid(TL2_ISR, byid=TRUE),TL2_ISR@data, match.ID=FALSE)
TL2_drops <- c("tl1_id", "name_or", "name_en", "name_fr", "continenta", "reg_name_p")
TL2_centroids2 <- TL2_centroids2[,!(names(TL2_centroids2) %in% TL2_drops)]
plot(TL2_centroids2)

#The next stage was to bind all of these together into a single dataframe
centroids2 <- rbind(UK_2010_centroids2, EUROPE_2013_centroids2)

#This required the changing of names of columns so that they could be vertically stacked
names(TL3_centroids_small)[names(TL3_centroids_small) == "tl3_id"] <- "region_code"
names(TL3_centroids_small)[names(TL3_centroids_small) == "iso3"] <- "CNTR_CODE"
names(TL2_centroids2)[names(TL2_centroids2) == "tl2_id"] <- "region_code"
names(TL2_centroids2)[names(TL2_centroids2) == "iso3"] <- "CNTR_CODE"
#Thus another merged dataframe could be identified
centroids_TL <- rbind(TL3_centroids_small, TL2_centroids2)

#again, the name was changed in the centroids 2 dataframe
names(centroids2)[names(centroids2) == "NUTS_ID"] <- "region_code"
#thus, this could be bound together 
centroids2 <- rbind(centroids2, centroids_TL)
#The next thing is to make sure they have the same CRS
centroids2 <- spTransform(centroids2, CRS("+init=epsg:4326"))
#US names have to be changed
names(US_2013_centroids)[names(US_2013_centroids) == "GEOID"] <- "region_code"
#CRS has to make sre it is correct
US_2013_centroids <- spTransform(US_2013_centroids, CRS("+init=epsg:4326"))
#Then all can be bound together
centroids2 = rbind(centroids2, US_2013_centroids)
#And outputted to a shapefile
writeOGR(centroids2, "shapefiles/centroids2.shp", layer ="centroids", driver="ESRI Shapefile")


#This is a previous solution
centroids <- rbind(UK_2010_centroids, EUROPE_2013_centroids)

names(TL3_centroids)[names(TL3_centroids) == "tl3_id"] <- "region_code"
names(TL2_centroids)[names(TL2_centroids) == "tl2_id"] <- "region_code"

centroid2 <- rbind(TL3_centroids, TL2_centroids)

names(centroids)[names(centroids) == "NUTS_ID"] <- "region_code"

st_crs(centroids)
names(US_2013_centroids)[names(US_2013_centroids) == "GEOID"] <- "region_code"
US_2013_centroids <- spTransform(US_2013_centroids, CRS("+init=epsg:4326"))
centroids2 <- spTransform(centroids2, CRS("+init=epsg:4326"))

centroids = rbind(centroids, US_2013_centroids)

writeOGR(centroids, "shapefiles/centroids_test.shp", layer ="centroids", driver="ESRI Shapefile")


#The issue with the previous work is that several key countries are still missing
#This includes non OECD countries that the OECD REGPAT classifies under the TL2 and TL3 conditions
#Thus non OECD countries boundaries were extracted from:
#https://gadm.org/

#The same spatial points process for getting centroids was repeated
China <- readOGR("shapefiles/Non-OECD/gadm36_CHN_1.shp")
#plot the result to make sure it is correct
plot(China)
#extract centroids, but only retain GID_1 as the regional title
China_centroids <- SpatialPointsDataFrame(gCentroid(China, byid=TRUE),China@data["GID_1"], match.ID=FALSE)
#country code was assigned as CN - the 2 digit county code
China_centroids$CNTR_CODE <- "CN"

India <- readOGR("shapefiles/Non-OECD/gadm36_IND_1.shp")
plot(India)
India_centroids <- SpatialPointsDataFrame(gCentroid(India, byid=TRUE),India@data["GID_1"], match.ID=FALSE)
#Country code assigned as IN
India_centroids$CNTR_CODE <- "IN"

Brazil <- readOGR("shapefiles/Non-OECD/gadm36_BRA_1.shp")
plot(Brazil)
Brazil_centroids <- SpatialPointsDataFrame(gCentroid(Brazil, byid=TRUE),Brazil@data["GID_1"], match.ID=FALSE)
#country code set as BR
Brazil_centroids$CNTR_CODE <- "BR"

Russia <- readOGR("shapefiles/Non-OECD/gadm36_RUS_1.shp")
plot(Russia)
Russia_centroids <- SpatialPointsDataFrame(gCentroid(Russia, byid=TRUE),Russia@data["GID_1"], match.ID=FALSE)
#county code set as RU
Russia_centroids$CNTR_CODE <- "RU"

South_Africa <- readOGR("shapefiles/Non-OECD/gadm36_ZAF_1.shp")
plot(South_Africa)
ZA_centroids <- SpatialPointsDataFrame(gCentroid(South_Africa, byid=TRUE),South_Africa@data["GID_1"], match.ID=FALSE)
#Country code set as ZA
ZA_centroids$CNTR_CODE <- "ZA"

#All of these have the same CRS and so can easily be merged together to form a single dataframe
Non_OECD_Cent <- rbind(ZA_centroids, Russia_centroids)
Non_OECD_Cent2 <- rbind(Brazil_centroids, China_centroids)
Non_OECD_Cent <- rbind(Non_OECD_Cent, Non_OECD_Cent2)
Non_OECD_Cent <- rbind(Non_OECD_Cent, India_centroids)
#Set the same CRS as those in the conditions above
Non_OECD_Cent <- spTransform(Non_OECD_Cent, CRS("+init=epsg:4326"))
#Change the title of the regional column to region_code (the same as the previous specification)
names(Non_OECD_Cent)[names(Non_OECD_Cent) == "GID_1"] <- "region_code"

#Write this out as a shapefile
writeOGR(Non_OECD_Cent, "shapefiles/Non_OECD_cent.shp", layer ="centroids", driver="ESRI Shapefile")

#Still, world countries are missing, where regions are assigned as the centre of the country
#This also comes from the same source as above
world <- readOGR("shapefiles/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
#The 2 digit county codes are then read in
countries <- read_csv("country_codes.csv")
#This is then merged so that county name is replaced by the 2 digit county code
world <- merge(world,
               countries,
               by.x = "CNTRY_NAME",
               by.y = "Name")
#centroids are then extracted, retaining only the 2 digit code
world_centroids <- SpatialPointsDataFrame(gCentroid(world, byid=TRUE),world@data["Code"], match.ID=FALSE)
#The result is transformed to match those above
world_centroids <- spTransform(world_centroids, CRS("+init=epsg:4326"))
#The result is then outputted as a shapefile, in the same way that the others are
writeOGR(world_centroids, "shapefiles/World_centroids.shp", layer ="centroids", driver="ESRI Shapefile")

#There are several countries that are missing or not specified and so these are extracted seperately 
#These follow the same process as before
HK <- readOGR("shapefiles/gadm36_HKG_0.shp")
plot(HK)
HK_centroid <- SpatialPointsDataFrame(gCentroid(HK, byid=TRUE),HK@data["GID_0"], match.ID=FALSE)
HK_centroid$regn_cd <- "HK000"
names(HK_centroid)[names(HK_centroid) == "GID_0"] <- "CNTR_CODE"
HK_centroid$CNTR_CODE <- "HK"
HK_centroid <- spTransform(HK_centroid, CRS("+init=epsg:4326"))
writeOGR(HK_centroid, "shapefiles/HK_centroids.shp", layer ="centroids", driver="ESRI Shapefile")


Monaco <- readOGR("shapefiles/gadm36_MCO_0.shp")
plot(Monaco)
Monaco_centroid <- SpatialPointsDataFrame(gCentroid(Monaco, byid=TRUE),Monaco@data["GID_0"], match.ID=FALSE)
Monaco_centroid$regn_cd <- "MCZZZ"
names(Monaco_centroid)[names(Monaco_centroid) == "GID_0"] <- "CNTR_CO"
Monaco_centroid$CNTR_CO <- "MC"
Monaco_centroid <- spTransform(Monaco_centroid, CRS("+init=epsg:4326"))
writeOGR(Monaco_centroid, "shapefiles/MC_centroids.shp", layer ="centroids", driver="ESRI Shapefile")

IOM <- readOGR("shapefiles/gadm36_IMN_0.shp")
plot(IOM)
IOM_centroid <- SpatialPointsDataFrame(gCentroid(IOM, byid=TRUE),IOM@data["GID_0"], match.ID=FALSE)
IOM_centroid$regn_cd <- "IM000"
names(IOM_centroid)[names(IOM_centroid) == "GID_0"] <- "CNTR_CO"
IOM_centroid$CNTR_CO <- "GB"
IOM_centroid <- spTransform(IOM_centroid, CRS("+init=epsg:4326"))
writeOGR(IOM_centroid, "shapefiles/IOM_centroids.shp", layer ="centroids", driver="ESRI Shapefile")




