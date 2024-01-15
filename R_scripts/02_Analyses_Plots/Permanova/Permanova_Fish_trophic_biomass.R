library(vegan)
library(tidyverse)
library(pairwiseAdonis)


load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_trophic_site.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep=",", na.strings = "NA")


#-------------------------------------------------------------------------------------------
## Prepare Fish trophic assemblage
#-------------------------------------------------------------------------------------------

meta$Protection[is.na(meta$Protection)] <- "Unprotected"

Fish_trophic_site <- left_join(meta[,c("Site_ID","Protection","Latitude","Longitude","Year","Site")], Fish_trophic_site)

Fish_trophic_site$Year <- as.factor(Fish_trophic_site$Year)

Fish_trophic_site <- Fish_trophic_site %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))


Fish_trophic_site$Protection <- as.factor(Fish_trophic_site$Protection)
Fish_trophic_site$cyclone <- as.factor(Fish_trophic_site$cyclone)
Fish_trophic_site$Site <- as.factor(Fish_trophic_site$Site)

Fish_trophic_site <- Fish_trophic_site %>%
  filter(!is.na(Herbivorous_Density))


troph_matrix <- Fish_trophic_site[,c(15:18)]
dist_matrix <- vegdist(troph_matrix, method = "bray")


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Year, data = Fish_trophic_site, permutations = 999)
perm_sp # Significant Year effect


pair.mod<-pairwise.adonis(dist_matrix,factors=Fish_trophic_site$Year)
pair.mod # 2019 different than 2016 and 2017_pre

#--------------------------------------------------------------------------------
## PERMANOVA ~ Site
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Site, data = Fish_trophic_site, permutations = 999)
perm_sp # No site effect

#--------------------------------------------------------------------------------
## PERMANOVA ~ Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Protection, data = Fish_trophic_site, permutations = 999)
perm_sp # Significant protection effect

#--------------------------------------------------------------------------------
## PERMANOVA ~ Cyclone
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ cyclone, data = Fish_trophic_site, permutations = 999)
perm_sp # Significant cyclone effect


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year*cyclone*Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ cyclone*Year*Protection, data = Fish_trophic_site, permutations = 999)
perm_sp # Significant effect of Year, cyclone and protection, but not their interaction


#--------------------------------------------------------------------------------
## PERMANOVA ~ Protection/Year
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Protection/Year, data = Fish_trophic_site, permutations = 999)
perm_sp # Significant effect of Year and Year nested in Protection


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year/Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Year/Protection, data = Fish_trophic_site, permutations = 999)
perm_sp # Significant effect of Year and Protection nested in Year




