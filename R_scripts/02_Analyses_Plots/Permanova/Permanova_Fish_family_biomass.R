library(vegan)
library(tidyverse)
library(pairwiseAdonis)

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_family_biomass_site.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep=",", na.strings = "NA")


#-------------------------------------------------------------------------------------------
## Prepare Fish family assemblage
#-------------------------------------------------------------------------------------------

Fish_family_biomass_site$Year <- as.factor(Fish_family_biomass_site$Year)

Fish_family_biomass_site <- Fish_family_biomass_site %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))


meta$Protection[is.na(meta$Protection)] <- "Unprotected"


Fish_family_biomass_site <- left_join(meta[,c("Site_ID","Protection","Latitude","Longitude")], Fish_family_biomass_site)

Fish_family_biomass_site$Protection <- as.factor(Fish_family_biomass_site$Protection)
Fish_family_biomass_site$cyclone <- as.factor(Fish_family_biomass_site$cyclone)
Fish_family_biomass_site$Site <- as.factor(Fish_family_biomass_site$Site)

Fish_family_biomass_site <- Fish_family_biomass_site %>%
  filter(!is.na(Acanthuridae))

fam_matrix <- Fish_family_biomass_site[,c(5:34)]
dist_matrix <- vegdist(fam_matrix, method = "bray")


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Year, data = Fish_family_biomass_site, permutations = 999)
perm_sp # Significant effect of Year


pair.mod<-pairwise.adonis(dist_matrix,factors=Fish_family_biomass_site$Year)
pair.mod # 2016 different from 2018,2019,2020

#--------------------------------------------------------------------------------
## PERMANOVA ~ Site
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Site, data = Fish_family_biomass_site, permutations = 999)
perm_sp # significant effect of Site

pair.mod<-pairwise.adonis(dist_matrix,factors=Fish_family_biomass_site$Site)
pair.mod # Shark Hotel different than Moonscape

#--------------------------------------------------------------------------------
## PERMANOVA ~ Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Protection, data = Fish_family_biomass_site, permutations = 999)
perm_sp # significant effect of Protection

#--------------------------------------------------------------------------------
## PERMANOVA ~ Cyclone
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ cyclone, data = Fish_family_biomass_site, permutations = 999)
perm_sp # significant effect of Cyclone


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year*cyclone*Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ cyclone*Year*Protection, data = Fish_family_biomass_site, permutations = 999)
perm_sp # Significant effect of Year, cyclone and protection, but not their interaction


#--------------------------------------------------------------------------------
## PERMANOVA ~ Protection/Year
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Protection/Year, data = Fish_family_biomass_site, permutations = 999)
perm_sp #  Significant effect of Protection and Year nested in Protection


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year/Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Year/Protection, data = Fish_family_biomass_site, permutations = 999)
perm_sp #  Significant effect of Year and Protection nested in Year

