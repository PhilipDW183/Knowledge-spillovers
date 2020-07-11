
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

#make sure that the working directory is set for easy use
setwd("C:/Users/cex/Documents/Smart Cities and Urban Analytics/Dissertation/Data")

#The same files as in the previous workbook are read in as necessary for plotting

tl3 <- readOGR("shapefiles/OECD_TL3_2020.shp")

US_counties <- readOGR("shapefiles/cb_2013_us_county_5m.shp")

Europe_2013 <- readOGR("shapefiles/NUTS_RG_01M_2013_4326_LEVL_3.shp/NUTS_RG_01M_2013_4326_LEVL_3.shp")
plot(Europe_2013)

Europe_2010 <- readOGR("shapefiles/NUTS_RG_01M_2010_4326_LEVL_3.shp/NUTS_RG_01M_2010_4326_LEVL_3.shp")
#extract the UK from 2010 and 2013
UK_2010 = subset(Europe_2010, CNTR_CODE=="UK")
Europe_2013_noUK = subset(Europe_2013, CNTR_CODE !="UK")

#non OECD countries
China <- readOGR("shapefiles/Non-OECD/gadm36_CHN_1.shp")

India <- readOGR("shapefiles/Non-OECD/gadm36_IND_1.shp")

Brazil <- readOGR("shapefiles/Non-OECD/gadm36_BRA_1.shp")

Russia <- readOGR("shapefiles/Non-OECD/gadm36_RUS_1.shp")

South_Africa <- readOGR("shapefiles/Non-OECD/gadm36_ZAF_1.shp")

#World
world <- readOGR("shapefiles/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")

#Other countries
HK <- readOGR("shapefiles/gadm36_HKG_0.shp")

Monaco <- readOGR("shapefiles/gadm36_MCO_0.shp")

#Isle of mann to be merged with GB
IOM <- readOGR("shapefiles/gadm36_IMN_0.shp")
#set the NUTS_ID
IOM$NUTS_ID <- "IM000"
#change the name of GID_0 
names(IOM)[names(IOM) == "GID_0"] <- "CNTR_CODE"
#Change the country name
IOM$CNTR_CODE <- "GB"
#Change the NUTS_Name
IOM$NUTS_NAME <- "Isle of Man"
#Drop unecessary columns from GB
UK_drops <- c("LEVL_CODE", "FID")
UK_2010 <- UK_2010[,!(names(UK_2010) %in% UK_drops)]
#Drop unecessary columns from IOM
IOM_drops <- c("NAME_0")
IOM <- IOM[,!(names(IOM) %in% IOM_drops)]
#Add the IoM to the UK
UK <- rbind(UK_2010, IOM)


#read in patent data
UK_patents <- read_csv("Patents data/all_UK_patents_region.csv")
UK_cited <- read_csv("Patents data/cited_regions_count.csv")
#Merge these with the UK
UK <- merge(UK,
            UK_patents,
            by.x = "NUTS_ID", 
            by.y = "reg_code")
UK  <- merge(UK,
             UK_cited,
             by.x = "NUTS_ID",
             by.y = "reg_code")

#set tmap mode to plot
tmap_mode("plot")
#All of the subsequent plots are exported using the export button

#The first map is all patents in the UK
tm1 <- tm_shape(UK)+
      #fill with inventor share
      tm_fill("inv_share.x",
              #set alpha to 1 so no transparency
              alpha =1,
              #set the title
              title = "UK regional inventor share",
              #set the palette - single colour
              palette = "Oranges",
              #set the legend to show
              legend.show = TRUE)+
      #add borders to the map
      tm_borders(col = "grey",
                 alpha = 0.8)+
      #set conditions for the legend
      tm_legend(title.size =1.5,
                show = TRUE)+
  #Use layout to set the title, and legend conditions
  tm_layout(title = "UK originating patents",
            title.size = 2,
            title.position = c("left","top"),
            legend.height = 4,
            legend.width = 4,
            legend.text.size = 1.2)+
  #add a compass to the top right corned
  tm_compass(type="rose",
             position = c(0.75,0.8),
             color.light ="grey90",
             size = 4)
#output the result
tm1

#PRoduce the same output but for patents that were cited in the UK
tm_2 <- tm_shape(UK)+
  tm_fill("inv_share.y",
          alpha =1,
          title = "Regional inventor share cited",
          legend.show = TRUE,
          palette = "Blues")+
  tm_borders(col = "grey",
             alpha = 0.8)+
  tm_legend(title.size =1.5,
            show = TRUE)+
  tm_layout(title = "UK Cited patents",
            title.size = 2,
            title.position = c("left","top"),
            legend.height = 4,
            legend.width = 4,
            legend.text.size = 1.2)+
  tm_compass(type="rose",
             position = c(0.75,0.8),
             color.light ="grey90",
             size = 4)

tm_2

#all citing patents location
citing <- read_csv("Patents data/citing_regions_count.csv")
#create a histogram shwoing the distribution
hist(citing$inv_share,
     breaks = 1000)
#set the breaks according to this histogram
breaks = c(0,25,50,100,250,500,1000,10000)
#Add to the UK map
UK <- merge(UK,
            citing,
            by.x = "NUTS_ID",
            by.y = "reg_code")

#All maps should have the same legend so the first map is legend only
tm_shape(UK)+
  tm_fill("inv_share",
          alpha =1,
          title = "Regional inventor share citing",
          breaks = breaks,
          legend.show = TRUE,
          palette = "Reds")+
  tm_borders(col = "grey",
             alpha = 0.8)+
  tm_legend(title.size =2,
            show = TRUE)+
  tm_layout(title = "UK citing patents",
            title.size = 2,
            title.position = c("left","top"),
            legend.height = 4,
            legend.width = 4,
            legend.text.size = 1.2,
            #This means only the legend is outputted
            legend.only =  TRUE)

#The first map is the UK
tm_shape(UK)+
  tm_fill("inv_share",
          alpha =1,
          #set the title to a) to be referenced in the caption
          title = "a)",
          breaks = breaks,
          legend.show = FALSE,
          palette = "Reds")+
  tm_borders(col = "grey",
             alpha = 0.8)+
 # tm_legend(title.size =1.2,
  #          show = TRUE)+
  tm_layout(title = "a)",
            title.size = 4,
            title.position = c("left","top"))+
      #      legend.height = 4,
     #       legend.width = 4,
      #      legend.text.size = 1.2)+
  tm_compass(type="rose",
             position = c(0.75,0.8),
             color.light ="grey90",
             size = 4)


#The next map is the US
US <- merge(US_counties,
            citing,
            by.x = "GEOID",
            by.y = "reg_code")

#counties can be removed that are not necessary to show the US
remove <- c("02185", "02188","02290","02180","02090","02240","02068","02270","02050","02170","02261","02070","02164","02060","02122","02020","02261","02282","02105","02100","02230","02110","02105","02220","02195","02275","02130","02198","02275","02150","02164","02013","02016",15001,15009,15003,15007,60010,60050,60020,60400,60030,72097,72117,72011,72003,72097,72067,72023,72079,72125,72093,72083,72131,72099,72005,72071,72115,72027,72081,72153,72055,72121,72059,72111,72001,72141,72065,72013,72017,72054,72039,72073,72113,72075,72419,72107,72039,72054,72017,72091,72145,72101,72107,72149,72075,72133,72043,72107,72101,72145,72143,72051,72137,72135,72105,72019,72045,72009,72043,72133,72123,72057,72035,72057,72015,72041,72007,72021,72061,72127,72139,72063,72129,72109,72151,72095,72085,72077,72063,72139,72127,72031,72029,72087,72119,72103,72069,72037,72052,72089,72147,72049,78030,78020,78010,66010,69100,69120,69110,69085,02016, 15005,72047,72025,72033,72053,"02016",60040)
US <- US[! US$GEOID %in% remove,]
#plot the result to make sure it is correct
plot(US)
#plot the result with the same format as before
tm3 <- tm_shape(US)+
  tm_fill("inv_share",
          alpha =1,
          #title = "Regional inventor share citing",
          breaks = breaks,
          legend.show = FALSE,
          palette = "Reds")+
  tm_borders(col = "grey",
             alpha = 0.8)+
#  tm_legend(title.size =1.2,
 #           show = TRUE)+
  tm_layout(title = "c)",
            title.size = 4,
            title.position = c("left","top"))+
        #    legend.height = 4,
        #    legend.width = 4,
        #    legend.text.size = 1.2,
            #earth.boundary = c(-130,22,-60,50))+
  tm_compass(type="rose",
             position = c(0.75,0.8),
             color.light ="grey90",
             size = 4)

tm3

#Merge Europe with citing
Europe <- merge(Europe_2013_noUK,
                citing,
                by.x = "NUTS_ID",
                by.y = "reg_code")
#remove regions that obstruct the view
remove_EU <- c("PT200","PT300","ES704", "ES706","ES705","ES709","ES708","ES706",
               "ES703","ES707","FRA50","FRA40","IS002","IS001","FRA30","FRA20",
               "FRA10")
Europe <- Europe[! Europe$NUTS_ID %in% remove_EU,]

#plot the result
tm_shape(Europe)+
  tm_fill("inv_share",
          alpha =1,
         # title = "Regional inventor share citing",
          breaks = breaks,
          legend.show = FALSE,
          palette = "Reds")+
  tm_borders(col = "grey",
             alpha = 0.8)+
#  tm_legend(title.size =1.2,
 #           show = TRUE)+
  tm_layout(title = "b)",
            title.size = 4,
            title.position = c("left","top"))+
   #         legend.height = 4,
    #        legend.width = 4,
     #       legend.text.size = 1.2)+
  tm_compass(type="rose",
             position = c(0.8,0.8),
             color.light ="grey90",
             size = 4)


citing$County <- substr(citing$reg_code, start =1, stop =2)

count <- aggregate(citing$inv_share, by = list(Category = citing$County), FUN=sum)

#For China, regional codes need to be changed to match those from the OECD REGPAT database
China$GID_1 <- substr(China$GID_1, start = 1, stop = 6)
China$GID_1 <- gsub("_", "", China$GID_1)
China$GID_1 <- gsub("\\.", "", China$GID_1)
China$GID_1 <- sub("CHN1\\b", "CN12", China$GID_1)
China$GID_1 <- sub("CHN2\\b", "CN01", China$GID_1)
China$GID_1 <- sub("CHN3\\b", "CN22", China$GID_1)
China$GID_1 <- sub("CHN18", "CN06", China$GID_1)
China$GID_1 <- sub("CHN6", "CN19", China$GID_1)
China$GID_1 <- sub("CHN27", "CN02", China$GID_1)
China$GID_1 <- sub("CHN10", "CN03", China$GID_1)
China$GID_1 <- sub("CHN19", "CN05", China$GID_1)
China$GID_1 <- sub("CHN17", "CN07", China$GID_1)
China$GID_1 <- sub("CHN11", "CN08", China$GID_1)
China$GID_1 <- sub("CHN24", "CN09", China$GID_1)
China$GID_1 <- sub("CHN15", "CN10", China$GID_1)
China$GID_1 <- sub("CHN31", "CN11", China$GID_1)
China$GID_1 <- sub("CHN4", "CN13", China$GID_1)
China$GID_1 <- sub("CHN16", "CN14", China$GID_1)
China$GID_1 <- sub("CHN23", "CN15", China$GID_1)
China$GID_1 <- sub("CHN12", "CN16", China$GID_1)
China$GID_1 <- sub("CHN13", "CN17", China$GID_1)
China$GID_1 <- sub("CHN14", "CN18", China$GID_1)
China$GID_1 <- sub("CHN7", "CN20", China$GID_1)
China$GID_1 <- sub("CHN9", "CN21", China$GID_1)
China$GID_1 <- sub("CHN26", "CN23", China$GID_1)
China$GID_1 <- sub("CHN8", "CN24", China$GID_1)
China$GID_1 <- sub("CHN30", "CN25", China$GID_1)
China$GID_1 <- sub("CHN22", "CN27", China$GID_1)
China$GID_1 <- sub("CHN5", "CN28", China$GID_1)
China$GID_1 <- sub("CHN20", "CN30", China$GID_1)
China$GID_1 <- sub("CHN28", "CN31", China$GID_1)
China$GID_1 <- sub("CHN29", "CN26", China$GID_1)
China$GID_1 <- sub("CHN21", "CN29", China$GID_1)

#China$GID_1 <- gsub("CHN", "CN", China$GID_1)
#China$GID_1 <- gsub("CN19", "CN191", China$GID_1)

#We also want to extract JAP and KOR
JPN = subset(tl3, iso3=="JPN")
KOR = subset(tl3, iso3=="KOR")
#bind these together
JP_KR <- rbind(JPN, KOR)
#Drop unecessary rows
TL3_drops = c("tl2_id", "tl1_id", "name_or", "name_en", "name_fr", "continenta", "reg_name_p")
#Remove these from the shapefile
JP_KR <- JP_KR[,!(names(JP_KR) %in% TL3_drops)]
#drop unecessary rows for china
China_drops = c("NAME_0", "NAME_1", "VARNAME_1", "NL_NAME_1", "TYPE_1", "ENGTYPE_1", "CC_1", "HASC_1")
China <- China[,!(names(China) %in% China_drops)]
#set the codes to match
names(China)[names(China) == "GID_0"] <- "iso3"
names(China)[names(China) == "GID_1"] <- "reg_code"
names(JP_KR)[names(JP_KR) == "tl3_id"] <- "reg_code"
#merge them all together
JP_KR_CN <- rbind(JP_KR, China)
#merge these with citing regions
JP_KR_CN <- merge(JP_KR_CN,
                  citing,
                  by.x = "reg_code",
                  by.y= "reg_code")
#plot the output as result
tm_shape(JP_KR_CN)+
  tm_fill("inv_share",
          alpha =1,
  #        title = "Regional inventor share cititng",
          breaks = breaks,
          legend.show = FALSE,
          palette = "Reds")+
  tm_borders(col = "grey",
             alpha = 0.8)+
  #tm_legend(title.size =1.2,
  #          show = TRUE)+
  tm_layout(title = "d)",
            title.size = 4,
            title.position = c("left","top"))+
     #       legend.height = 4,
      #      legend.width = 4,
       #     legend.text.size = 1.2)+
  tm_compass(type="rose",
             position = c(0.8,0.85),
             color.light ="grey90",
             size = 4)


