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

getwd()

setwd("C:/Users/cex/Documents/Smart Cities and Urban Analytics/Dissertation/Data")

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


#found from here: https://gis.stackexchange.com/questions/147538/where-can-i-find-shapefiles-for-oecd-regions
#https://www.dropbox.com/sh/aqfzuofxocv6zgl/AABFWmuLByljQrvY1eXP8em8a?dl=0 FOR TL2 and TL3 shapefiles
#need to find NUTS3 shapefiles for 2013
#https://gis.stackexchange.com/questions/282231/any-way-to-extract-nuts-3-shapefile-of-germany-from-eurostat-shapefile-in-r-up

tl2 <- readOGR("shapefiles/OECD_TL2_2020.shp")
#plot(tl2)

tl3 <- readOGR("shapefiles/OECD_TL3_2020.shp")
plot(tl3)

US_counties <- readOGR("shapefiles/cb_2013_us_county_5m.shp")
plot(US_counties)
US_2013_centroids =gCentroids(US_counties, byid= TRUE)

Europe_2013 <- readOGR("shapefiles/NUTS_RG_01M_2013_4326_LEVL_3.shp/NUTS_RG_01M_2013_4326_LEVL_3.shp")
plot(Europe_2013)
EU_2013_centroids = gCentroid(Europe_2013, byid = TRUE)
plot(EU_centroids)

Europe_2010 <- readOGR("shapefiles/NUTS_RG_01M_2010_4326_LEVL_3.shp/NUTS_RG_01M_2010_4326_LEVL_3.shp")
EU_2010_centroid = gCentroid(Europe_2010, byid = TRUE)

#https://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes/43558

UK_2010 = subset(Europe_2010, CNTR_CODE=="UK")
Europe_2013_noUK = subset(Europe_2013, CNTR_CODE !="UK")
plot(UK_2010)
plot(Europe_2013_noUK)
UK_centroids = gCentroid(UK_2010, byid = TRUE, id= NUTS_ID)
plot(UK_centroids)

#UK_2010_SF <- st_as_sf(UK_2010)
#UK_2010_centroids <- st_centroid(UK_2010_SF)
#join <- join(UK_2010_SF, UK_2010_centr#oids)

UK_2010_centroids <- SpatialPointsDataFrame(gCentroid(UK_2010, byid=TRUE),UK_2010@data["NUTS_ID"],  match.ID=FALSE)

UK_2010_centroids2 <- SpatialPointsDataFrame(gCentroid(UK_2010, byid=TRUE),UK_2010@data,  match.ID=FALSE)

UK_drops <- c("LEVL_CODE", "NUTS_NAME", "FID")
UK_2010_centroids2 <- UK_2010_centroids2[,!(names(UK_2010_centroids2) %in% UK_drops)]

EUROPE_2013_centroids <- SpatialPointsDataFrame(gCentroid(Europe_2013_noUK, byid=TRUE),Europe_2013_noUK@data["NUTS_ID"], match.ID=FALSE)

EUROPE_2013_centroids2 <- SpatialPointsDataFrame(gCentroid(Europe_2013_noUK, byid=TRUE),Europe_2013_noUK@data, match.ID=FALSE)
EU_drops <- c("LEVL_CODE", "NUTS_NAME", "FID")
EUROPE_2013_centroids2 <- EUROPE_2013_centroids2[,!(names(EUROPE_2013_centroids2) %in% EU_drops)]

US_2013_centroids <- SpatialPointsDataFrame(gCentroid(US_counties, byid=TRUE),US_counties@data["GEOID"], match.ID=FALSE)
US_2013_centroids$CNTR_CODE <- "US"

US_2013_centroids2 <- SpatialPointsDataFrame(gCentroid(US_counties, byid=TRUE),US_counties@data, match.ID=FALSE)
#US <- list(rep("US", 3234))

TL3_centroids <- SpatialPointsDataFrame(gCentroid(tl3, byid=TRUE),tl3@data["tl3_id"], match.ID=FALSE)

TL3_centroids2 <- SpatialPointsDataFrame(gCentroid(tl3, byid=TRUE),tl3@data, match.ID=FALSE)
TL3_drops = c("tl2_id", "tl1_id", "name_or", "name_en", "name_fr", "continenta", "reg_name_p")
TL3_centroids2 <- TL3_centroids2[,!(names(TL3_centroids2) %in% TL3_drops)]
TL3_countries <- c("AUS", "CAN", "CHL", "JPN", "KOR", "MEX", "NZL", "CHN", "IND", "RUS", "SAU", "ZAF")
TL3_centroids_small <- TL3_centroids2[TL3_centroids2$iso3 %in% TL3_countries,]



TL2_ISR = subset(tl2, iso3=="ISR")
TL2_centroids <- SpatialPointsDataFrame(gCentroid(tl2, byid=TRUE),tl2@data["tl2_id"], match.ID=FALSE)

TL2_centroids2 <- SpatialPointsDataFrame(gCentroid(TL2_ISR, byid=TRUE),TL2_ISR@data, match.ID=FALSE)
TL2_drops <- c("tl1_id", "name_or", "name_en", "name_fr", "continenta", "reg_name_p")
TL2_centroids2 <- TL2_centroids2[,!(names(TL2_centroids2) %in% TL2_drops)]
plot(TL2_centroids2)


centroids2 <- rbind(UK_2010_centroids2, EUROPE_2013_centroids2)

names(TL3_centroids_small)[names(TL3_centroids_small) == "tl3_id"] <- "region_code"
names(TL3_centroids_small)[names(TL3_centroids_small) == "iso3"] <- "CNTR_CODE"
names(TL2_centroids2)[names(TL2_centroids2) == "tl2_id"] <- "region_code"
names(TL2_centroids2)[names(TL2_centroids2) == "iso3"] <- "CNTR_CODE"

centroids_TL <- rbind(TL3_centroids_small, TL2_centroids2)

names(centroids2)[names(centroids2) == "NUTS_ID"] <- "region_code"

centroids2 <- rbind(centroids2, centroids_TL)

centroids2 <- spTransform(centroids2, CRS("+init=epsg:4326"))
names(US_2013_centroids)[names(US_2013_centroids) == "GEOID"] <- "region_code"
US_2013_centroids <- spTransform(US_2013_centroids, CRS("+init=epsg:4326"))

centroids2 = rbind(centroids2, US_2013_centroids)

writeOGR(centroids2, "shapefiles/centroids2.shp", layer ="centroids", driver="ESRI Shapefile")


#old
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


#non OECD countries
China <- readOGR("shapefiles/Non-OECD/gadm36_CHN_1.shp")
plot(China)
China_centroids <- SpatialPointsDataFrame(gCentroid(China, byid=TRUE),China@data["GID_1"], match.ID=FALSE)
China_centroids$CNTR_CODE <- "CN"

India <- readOGR("shapefiles/Non-OECD/gadm36_IND_1.shp")
plot(India)
India_centroids <- SpatialPointsDataFrame(gCentroid(India, byid=TRUE),India@data["GID_1"], match.ID=FALSE)
India_centroids$CNTR_CODE <- "IN"

Brazil <- readOGR("shapefiles/Non-OECD/gadm36_BRA_1.shp")
plot(Brazil)
Brazil_centroids <- SpatialPointsDataFrame(gCentroid(Brazil, byid=TRUE),Brazil@data["GID_1"], match.ID=FALSE)
Brazil_centroids$CNTR_CODE <- "BR"

Russia <- readOGR("shapefiles/Non-OECD/gadm36_RUS_1.shp")
plot(Russia)
Russia_centroids <- SpatialPointsDataFrame(gCentroid(Russia, byid=TRUE),Russia@data["GID_1"], match.ID=FALSE)
Russia_centroids$CNTR_CODE <- "RU"

South_Africa <- readOGR("shapefiles/Non-OECD/gadm36_ZAF_1.shp")
plot(South_Africa)
ZA_centroids <- SpatialPointsDataFrame(gCentroid(South_Africa, byid=TRUE),South_Africa@data["GID_1"], match.ID=FALSE)
ZA_centroids$CNTR_CODE <- "ZA"


Non_OECD_Cent <- rbind(ZA_centroids, Russia_centroids)
Non_OECD_Cent2 <- rbind(Brazil_centroids, China_centroids)
Non_OECD_Cent <- rbind(Non_OECD_Cent, Non_OECD_Cent2)
Non_OECD_Cent <- rbind(Non_OECD_Cent, India_centroids)
Non_OECD_Cent <- spTransform(Non_OECD_Cent, CRS("+init=epsg:4326"))

names(Non_OECD_Cent)[names(Non_OECD_Cent) == "GID_1"] <- "region_code"


writeOGR(Non_OECD_Cent, "shapefiles/Non_OECD_cent.shp", layer ="centroids", driver="ESRI Shapefile")

world <- readOGR("shapefiles/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")

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


countries <- read_csv("country_codes.csv")
#countries = countries[c(3,7)]

world <- merge(world,
               countries,
               by.x = "CNTRY_NAME",
               by.y = "Name")

world_centroids <- SpatialPointsDataFrame(gCentroid(world, byid=TRUE),world@data["Code"], match.ID=FALSE)

plot(world_centroids)
world_centroids <- spTransform(world_centroids, CRS("+init=epsg:4326"))


writeOGR(world_centroids, "shapefiles/World_centroids.shp", layer ="centroids", driver="ESRI Shapefile")

#checking distance between points
EPO_treatment_orig <- readOGR("shapefiles/EPO_treatment_cited.shp")
#EPO_treatment_orig <- st_point(EPO_treatment_orig)
EPO_treatment_orig <- st_as_sf(EPO_treatment_orig)
EPO_treatment_dest <- readOGR("shapefiles/EPO_treatment_citing.shp")
#EPO_treatment_dest <- st_point(EPO_treatment_dest)
EPO_treatment_dest <- st_as_sf(EPO_treatment_dest)

#EPO_treatment <- cbind(EPO_treatment_dest)

#EPO_treatment_orig <- st_distance(EPO_treatment_orig, EPO_treatment_dest, by_element = TRUE)

#EPO_treatment_orig <- mutate(distance=st_distance(EPO_treatment_orig, EPO_treatment_dest, by_element = T))

EPO_treatment_orig$distance <- st_distance(EPO_treatment_orig$geometry, EPO_treatment_dest$geometry, by_element=T)

#origin_df <- SpatialPointsDataFrame(coords = origin, data = EPO_treatment, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 
#EPO_treatment <- EPO_treatment %>% mutate(distance = st_distance(geometry_x, geometry_y, by_element = T))




UK_patents <- read_csv("Patents data/all_UK_patents_region.csv")
UK_cited <- read_csv("Patents data/cited_regions_count.csv")

UK <- merge(UK_2010,
            UK_patents,
            by.x = "NUTS_ID", 
            by.y = "reg_code")
UK  <- merge(UK,
             UK_cited,
             by.x = "NUTS_ID",
             by.y = "reg_code")

tmap_mode("view")

tm_shape(UK)+
   tm_fill("inv_share.x")+
   tm_borders(lwd = 1)

tm_shape(UK)+
   tm_fill("inv_share.y")+
   tm_borders(lwd =1)

citing <- read_csv("Patents data/citing_regions_count.csv")
