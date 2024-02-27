
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
library(ggeffects)
library(gratia)



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

myData <- left_join(myData, Benthic_cover_site_all[,c(1,3,4,5,7,9,123,124)])

myData$Site <- as.factor(myData$Site)
myData$Protection <- as.factor(myData$Protection)
myData$cyclone <- as.factor(myData$cyclone)

myData <- myData %>%
  filter(!is.na(GORGONIANS))


########## GAM 
GAM1 = gam(Piscivorous_Biomass ~ Year+SST+Protection+Fish_density+Site+Bleaching_frequency+Disease_frequency+DHW+
             dist_to_coast+GORGONIANS+MACROALGAE+Recruits_density+CORAL, data=myData)
summary(GAM1)



GAM2 = gam(Scaridae_Abundance ~ Year+s(GORGONIANS)+s(MACROALGAE)+
             s(Bleaching_frequency)+s(SST), data=myData)
summary(GAM2)
gratia::draw(GAM2, residuals=F, nrow=2)



ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/GAM/Scaridae_abundance/GAM_Scaridae_abundance.png")




