library(vegan)
library(ade4)
library(lme4)
library(DHARMa)
library(car)
library(visreg)
library(ecospat)
library(modEvA)
library(psych)
library(MASS)
library(lmeInfo)
library(pscl)
library(spdep)
library(tidyverse)
library(nlme)
library(MuMIn)
library(rcompanion)
library(ggpubr)
library(ggplot2)
library(mgcv)



# Load data
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Site_biodiversity_metrics.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep = ",", na.strings = "NA")

meta$Protection[is.na(meta$Protection)] <- "Unprotected"
Site_biodiv$Year <- as.factor(Site_biodiv$Year)

myData <- left_join(meta[,c("Site_ID", "Protection", "Depth_m", "SST", "DHW", "dist_to_coast", "Longitude", "Latitude")], Site_biodiv)

myData <- myData %>%
  filter(!is.na(Site))

myData <- myData %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Benthic_cover_site_all.rdata")

myData <- left_join(myData, Benthic_cover_site_all[,c(1,3,4,5,7,9,122,123)])

myData$Site <- as.factor(myData$Site)
myData$Protection <- as.factor(myData$Protection)
myData$cyclone <- as.factor(myData$cyclone)

myData <- myData[,c("Year", "SST", "Protection","Site",
                    "DHW", "dist_to_coast", "Fish_density", "GORGONIANS", "MACROALGAE",
                    "Acanthuridae_Density", "Labridae_Density","Scaridae_Density", 
                    "Herbivorous_Density", "Piscivorous_Density", 
                    "MacroAlgae_MeanHeight", "Recruits_density", "CORAL")]

myData <- myData %>%
  filter(!is.na(GORGONIANS))


########## GAM

library(mgcv)
library(ggeffects)
library(gratia)

GAM1 = gam(CORAL ~ s(DHW), data=myData)
summary(GAM1)


GAM2 = gam(CORAL ~ s(MACROALGAE), data=myData)
summary(GAM2)


GAM3 = gam(CORAL ~ s(Herbivorous_Density), data=myData)
summary(GAM3)


GAM4 = gam(CORAL ~ s(SST)+s(MACROALGAE)+s(Herbivorous_Density)+Year+Site, data = myData, method = "REML")
summary(GAM4)
gratia::draw(GAM4, residuals=F, nrow=1)

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/GAM/Coral_cover/GAM_coral_cover.png")




