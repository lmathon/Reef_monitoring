library(vegan)
library(tidyverse)
library(pairwiseAdonis)

load("C:/Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Benthic_cover_site_all.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep=",", na.strings = "NA")


#-------------------------------------------------------------------------------------------
## Prepare benthic cover assemblage
#-------------------------------------------------------------------------------------------

meta$Protection[is.na(meta$Protection)] <- "Unprotected"

Benthic_cover_site_all <- left_join(meta[,c("Site_ID","Protection","Latitude","Longitude","Year","Site")], Benthic_cover_site_all)

Benthic_cover_site_all$Year <- as.factor(Benthic_cover_site_all$Year)

Benthic_cover_site_all <- Benthic_cover_site_all %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))

Benthic_cover_site_all <- Benthic_cover_site_all %>%
  filter(!is.na(CORAL))


cover_matrix <- Benthic_cover_site_all[,c(19:124)]
dist_matrix <- vegdist(cover_matrix, method = "bray")


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Year, data = Benthic_cover_site_all, permutations = 999)
perm_sp # Significant Year effect


pair.mod<-pairwise.adonis(dist_matrix,factors=Benthic_cover_site_all$Year)
pair.mod # 2016 is different

#--------------------------------------------------------------------------------
## PERMANOVA ~ Site
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Site, data = Benthic_cover_site_all, permutations = 999)
perm_sp # Significant site effect

pair.mod<-pairwise.adonis(dist_matrix,factors=Benthic_cover_site_all$Site)
pair.mod # Mikes Maze and Proselyte different

#--------------------------------------------------------------------------------
## PERMANOVA ~ Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Protection, data = Benthic_cover_site_all, permutations = 999)
perm_sp # Significant protection effect


#--------------------------------------------------------------------------------
## PERMANOVA ~ Cyclone
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ cyclone, data = Benthic_cover_site_all, permutations = 999)
perm_sp # Significant Cyclone effect


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year*cyclone*Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ cyclone*Year*Protection, data = Benthic_cover_site_all, permutations = 999)
perm_sp # Significant effect of Year, cyclone and protection, and their interaction


#--------------------------------------------------------------------------------
## PERMANOVA ~ Protection/Year
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Protection/Year, data = Benthic_cover_site_all, permutations = 999)
perm_sp # Significant effect of Year and Year nested in Protection


#--------------------------------------------------------------------------------
## PERMANOVA ~ Year/Protection
#--------------------------------------------------------------------------------
perm_sp=adonis2(dist_matrix ~ Year/Protection, data = Benthic_cover_site_all, permutations = 999)
perm_sp # Significant effect of Year and Protection nested in Year

