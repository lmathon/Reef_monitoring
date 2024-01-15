library(tidyverse)
library(raster)
library(tools)
library(sf)
library(rgdal)
library(rgeos)

#load metadata file
GCRMN_meta <- read.csv("C://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata_raw.csv", sep=";", na.strings = "")

# remove empty rows
GCRMN_meta <- GCRMN_meta %>%
  filter(!is.na(Year))

# Cut latitude in degrees and minutes
GCRMN_meta$Lat_deg <- as.numeric(substr(GCRMN_meta$Latitude, 1,2))
GCRMN_meta$Lat_min <- as.numeric(substr(GCRMN_meta$Latitude, 4,9))

# Compute latitude in decimal degrees
GCRMN_meta$Latitude <- (GCRMN_meta$Lat_deg+GCRMN_meta$Lat_min/60)

# Cut longitude in degrees and minutes
GCRMN_meta$Long_deg <- as.numeric(substr(GCRMN_meta$Longitude, 1,2))
GCRMN_meta$Long_min <- as.numeric(substr(GCRMN_meta$Longitude, 4,9))

# Compute longitude in decimal degrees
GCRMN_meta$Longitude <- (-1*(GCRMN_meta$Long_deg+GCRMN_meta$Long_min/60))


# Remove unecessary columns
GCRMN_meta <- GCRMN_meta[,-c(16:19)]

# Update 2017 to 2017_pre and 2017_post
for (i in 1:nrow(GCRMN_meta)) {
  if(GCRMN_meta[i,"Year"]=="2017" & GCRMN_meta[i,"Month"]=="August"){
    GCRMN_meta[i,"Year"] <- "2017_pre"
  }
  if(GCRMN_meta[i,"Year"]=="2017" & GCRMN_meta[i,"Month"]%in%c("November","December")){
    GCRMN_meta[i,"Year"] <- "2017_post"
  }
}

# Create unique ID per transect
GCRMN_meta$Site_ID <- paste0(GCRMN_meta$Year, "-", GCRMN_meta$Site)

# join fish observer data
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_observer.rdata")
names(Fish_obs) <- c("Site_ID", "Fish_observer")
GCRMN_meta <- left_join(GCRMN_meta, Fish_obs, by="Site_ID")

# Remove unnecessary columns
GCRMN_meta <- GCRMN_meta[,-c(9,11,12)]

# Write new clean file
write.csv(GCRMN_meta, "C://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", row.names = F)

# Add year 2021 and 2022 with NA, manually on csv file.

#########################################################################################
#### Extract SST and DHW data ####

# Load metadata file again
GCRMN_meta <- read.csv("C://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep=",", na.strings = "")

# load all sst raster files in a list
setwd("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/SST/")
temp = list.files(pattern="*.nc")
sst_raster = lapply(temp, raster)
names <- file_path_sans_ext(temp)

names(sst_raster) <- names

# extract SST value for each site
df_sst <- data.frame(Site_ID=character(), SST=numeric())
for (i in 1:length(names)) {
  df <- GCRMN_meta %>%
    filter(Site_ID==names[[i]])%>%
    dplyr::select(Longitude,Latitude)
  names(df) <- c("x","y")
  sst <- extract(sst_raster[[i]], df)
  df_sst[i,"Site_ID"] <- names[[i]]
  df_sst[i, "SST"] <- sst
}

# Join SST value with metadata

GCRMN_meta <- left_join(GCRMN_meta, df_sst)


# load all DHW raster files in a list
setwd("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/DHW/")
temp = list.files(pattern="*.nc")
dhw_raster = lapply(temp, raster)
names <- file_path_sans_ext(temp)

names(dhw_raster) <- names

# extract DHW value for each site
df_dhw <- data.frame(Site_ID=character(), DHW=numeric())
for (i in 1:length(names)) {
  df <- GCRMN_meta %>%
    filter(Site_ID==names[[i]])%>%
    dplyr::select(Longitude,Latitude)
  names(df) <- c("x","y")
  dhw <- extract(dhw_raster[[i]], df)
  df_dhw[i,"Site_ID"] <- names[[i]]
  df_dhw[i, "DHW"] <- dhw
}

# Join DHW value with metadata

GCRMN_meta <- left_join(GCRMN_meta, df_dhw)


########################################################################################
#### Compute distance to the coast ####

coast <- readOGR("c://Users/mathonlocal/Desktop/Nature Foundation/Cartography/Land - Waterbodies/SXM_Land.shp")

crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crsmerc=CRS("+proj=merc +a=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs")

coast <- spTransform(coast, crsmerc)

metadata_dist <- GCRMN_meta %>%
  filter(Month!="NA")
pts <- metadata_dist %>%
  dplyr::select(Longitude,Latitude)
pts_sp <- SpatialPoints(pts,proj4string = crswgs84)
pts_sp2 <- spTransform(pts_sp, crsmerc)

# calculate and formate distance
dist <- gDistance(pts_sp2, coast, byid = T)
dist_min <- apply(dist,2,min)
dist_min <- round(dist_min, 0) 
metadata_dist$dist_to_coast <- dist_min

GCRMN_meta <- left_join(GCRMN_meta, metadata_dist[,c("Site_ID","dist_to_coast")])


write.csv(GCRMN_meta, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", row.names = F)
