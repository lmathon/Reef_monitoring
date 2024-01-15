library(tidyverse)
library(rfishbase)
library(dplyr)
library(stringr)


# Load fish file
Fish <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/2 - Raw_data/2 - CSV/GCRMN_Fish.csv", sep=";", na.strings = "")

# remove empty rows
Fish <- Fish %>%
  filter(!is.na(Year))

# Update 2017 to 2017_pre and 2017_post

for (i in 1:nrow(Fish)) {
  if(Fish[i,"Year"]=="2017" & Fish[i,"Month"]=="August"){
    Fish[i,"Year"] <- "2017_pre"
  }
  if(Fish[i,"Year"]=="2017" & Fish[i,"Month"]%in%c("November","December")){
    Fish[i,"Year"] <- "2017_post"
  }
}
 
# Correct Site names
for (i in 1:nrow(Fish)) {
  if(Fish[i,"Site"]=="Mike's Maze"){
    Fish[i,"Site"] <- "Mikes Mazes"
  }
  if(Fish[i,"Site"]=="Proselyte"){
    Fish[i,"Site"] <- "Proselyte Reef"
  }
  if(Fish[i,"Site"]=="Long Bay"){
    Fish[i,"Site"] <- "Long Bay Reef"
  }
  if(Fish[i,"Site"]=="Fishbowl"){
    Fish[i,"Site"] <- "Fish Bowl"
  }
}

# Create unique ID per site
Fish$Site_ID <- paste0(Fish$Year, "-", Fish$Site)

# Fish observer
Fish_obs <- Fish %>%
  distinct(Site_ID, Observer)

save(Fish_obs, file = "c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_observer.rdata")


# Correct fish species names
Fish$SpeciesSN <- gsub("Chromis cyanea", "Azurina cyanea", Fish$SpeciesSN)
Fish$SpeciesSN <- gsub("Chromis multilineata", "Azurina multilineata", Fish$SpeciesSN)
Fish$SpeciesSN <- gsub("Carangoides ruber", "Caranx ruber", Fish$SpeciesSN)
Fish$SpeciesSN <- gsub("Carangoides bartholomaei", "Caranx bartholomaei", Fish$SpeciesSN)
Fish$SpeciesSN <- gsub("Dasyatis americana", "Hypanus americanus", Fish$SpeciesSN)
Fish$SpeciesSN <- gsub("Acanthostracion polygonius", "Acanthostracion polygonium", Fish$SpeciesSN)
Fish$SpeciesSN <- gsub("Carcharhinus perezi", "Carcharhinus perezii", Fish$SpeciesSN)
Fish$SpeciesSN <- gsub("Kyphosus sectator", "Kyphosus sectatrix", Fish$SpeciesSN)

# Add Family name
taxo <- load_taxa()
Fish <- left_join(Fish, taxo[,c("Species", "Family")], join_by("SpeciesSN" == "Species"))
Fish <- Fish %>%
  filter(!is.na(Family))

# Add trophic group
Fish <- Fish %>%
  mutate(trophic_group = case_when(
    Family%in%c("Acanthuridae","Scaridae","Pomacentridae","Ostraciidae") ~ "Herbivorous",
    Family%in%c("Labridae","Serranidae","Tetraodontidae","Pomacanthidae","Scorpaenidae","Kyphosidae","Ginglymostomatidae") ~ "Omnivorous",
    Family%in%c("Lutjanidae","Epinephelidae","Carcharhinidae","Carangidae","Sphyraenidae","Synodontidae") ~ "Piscivorous",
    Family%in%c("Dasyatidae","Chaetodontidae","Haemulidae","Grammatidae","Holocentridae","Balistidae","Monacanthidae","Sparidae","Sciaenidae","Mullidae","Gobiidae","Malacanthidae","Opistognathidae","Aulostomidae","Muraenidae","Diodontidae","Gerreidae") ~ "Invertivorous",
    
  ))

Fish <- Fish %>%
  select(-c(TP_NO,TP_SIZE))


########################################################################################
### List of fish species and taxo

Fish_list <- Fish %>%
  select(SpeciesSN, Family)%>%
  left_join(., taxo[,c("Species","Genus", "Order", "Class")], join_by("SpeciesSN" == "Species"))


Fish_list <- Fish_list%>%
  select(Class,Order,Family,Genus,SpeciesSN)%>%
  distinct(SpeciesSN, .keep_all=T)

save(Fish_list, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_list.rdata")

########################################################################################
### Compute fish biomass

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/species_LW.rdata")
Fish <- left_join(Fish, species_LW[,c("Species","a","b")], join_by("SpeciesSN" == "Species"))
Fish[,c("Size_cm","a","b")] <- as.numeric(unlist(Fish[,c("Size_cm","a","b")]))
Fish$Biomass <- (Fish$a*(Fish$Size_cm^Fish$b))*Fish$Number
Fish$Biomass <- Fish$Biomass/1000

Fish <- Fish %>%
  select(-c(a,b))

#########################################################################################
#### Fish species matrix abundance + abundance/richness/density per site ####

# Sum of abundance of each species per site
Fish_species_abund_site <- Fish %>%
  select(Site_ID, SpeciesSN, Number)%>%
  group_by(Site_ID, SpeciesSN) %>% 
  summarise_all(funs(sum))

# Transpose dataframe to have sites in lines and species in columns
Fish_species_abund_site <- spread(data=Fish_species_abund_site, key="SpeciesSN", value="Number", fill = 0)

# Compute total fish abundance per site
Fish_species_abund_site$Fish_abundance <- rowSums(Fish_species_abund_site[,c(2:ncol(Fish_species_abund_site))])

# Compute total fish species richness per site
Fish_species_abund_site$Fish_richness <- rowSums(Fish_species_abund_site[,c(2:ncol(Fish_species_abund_site)-1)] != 0)

# Compute fish density per site (area per site = 300m2)
Fish_species_abund_site$Fish_density <- Fish_species_abund_site$Fish_abundance/300

# Add columns Year, Site
Fish_species_abund_site <- Fish_species_abund_site %>%
  left_join(., Fish[,c("Site_ID", "Year", "Site")]) %>%
  distinct(Site_ID, .keep_all=T)


# Save Rdata
save(Fish_species_abund_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_species_abund_site.rdata")
write.csv(Fish_species_abund_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Species_abund_matrix_site.csv", row.names = F)

#########################################################################################
#### Fish species biomass matrix per site ####

# Sum of biomass of each species per site
Fish_species_biomass_site <- Fish %>%
  select(Site_ID, SpeciesSN, Biomass)%>%
  filter(!is.na(Biomass))%>%
  group_by(Site_ID, SpeciesSN) %>% 
  summarise_all(funs(sum))

# Transpose dataframe to have sites in lines and species in columns
Fish_species_biomass_site <- spread(data=Fish_species_biomass_site, key="SpeciesSN", value="Biomass", fill = 0)

# Compute total fish biomass per site
Fish_species_biomass_site$Fish_biomass <- rowSums(Fish_species_biomass_site[,c(2:ncol(Fish_species_biomass_site))])

# Save Rdata
save(Fish_species_biomass_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_species_biomass_site.rdata")
write.csv(Fish_species_biomass_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Species_biomass_matrix_site.csv", row.names = F)


#########################################################################################
#### Fish family abundance matrix per site ####

# Sum of abundance of each family per site
Fish_family_abund_site <- Fish %>%
  select(Site_ID, Family, Number)%>%
  group_by(Site_ID, Family) %>% 
  summarise_all(funs(sum))

save(Fish_family_abund_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Family_abundance.rdata")

# Transpose dataframe to have site in lines and families in columns
Fish_family_abund_site <- spread(data=Fish_family_abund_site, key="Family", value="Number", fill = 0)

# compute family richness per site
Fish_family_abund_site$Family_richness <- rowSums(Fish_family_abund_site[,c(2:ncol(Fish_family_abund_site))] != 0)

# Add columns Year, Site
Fish_family_abund_site <- Fish_family_abund_site %>%
  left_join(., Fish[,c("Site_ID", "Year", "Site")]) %>%
  distinct(Site_ID, .keep_all=T)

# Save Rdata
save(Fish_family_abund_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_family_abund_site.rdata")


#########################################################################################
#### Fish family biomass matrix per site ####


# Sum of biomass of each family per site
Fish_family_biomass_site <- Fish %>%
  select(Site_ID, Family, Biomass)%>%
  filter(!is.na(Biomass))%>%
  group_by(Site_ID, Family) %>% 
  summarise_all(funs(sum))

save(Fish_family_biomass_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Family_biomass.rdata")

# Transpose dataframe to have site in lines and families in columns
Fish_family_biomass_site <- spread(data=Fish_family_biomass_site, key="Family", value="Biomass", fill = 0)

# Add columns Year, Site
Fish_family_biomass_site <- Fish_family_biomass_site %>%
  left_join(., Fish[,c("Site_ID", "Year", "Site")]) %>%
  distinct(Site_ID, .keep_all=T)

# Save Rdata
save(Fish_family_biomass_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_family_biomass_site.rdata")


#########################################################################################
#### Fish mean individual size per family per site ####

Fish$Size_cm <- gsub(">", "", Fish$Size_cm )
Fish$Size_cm <- as.numeric(Fish$Size_cm)

# Compute mean fish size per family and site
Fish_family_size_sites_mean <- Fish %>%
  select(Site_ID, Family, Size_cm)%>%
  group_by(Site_ID, Family) %>% 
  summarise_all(funs(mean))


# compute SD fish size per family and site
Fish_family_size_sites_sd <- Fish %>%
  select(Site_ID, Family, Size_cm)%>%
  group_by(Site_ID, Family) %>% 
  summarise_all(funs(sd))

# assemble both
Fish_family_size_site <- cbind(Fish_family_size_sites_mean, Fish_family_size_sites_sd$Size_cm)
colnames(Fish_family_size_site) <- c("Site_ID", "Family", "Mean_size", "SD_size")

# add year and site
Fish_family_size_site <- left_join(Fish_family_size_site, Fish_family_abund_site[,c("Year", "Site", "Site_ID")])

# Save Rdata
save(Fish_family_size_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_family_size_site.rdata")

# spread table
Fish_family_size_site_spread <- Fish_family_size_site %>%
  select(Site_ID, Family, Mean_size)
  
Fish_family_size_site_spread <- spread(data=Fish_family_size_site_spread, key="Family", value="Mean_size", fill = 0)

# Save Rdata
save(Fish_family_size_site_spread, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_family_size_site_spread.rdata")


########################################################################################
#### Select important family data ####

# select important families abundance
select_abundance <- Fish_family_abund_site %>%
  select(Acanthuridae, Labridae, Lutjanidae, Serranidae, Epinephelidae, Carcharhinidae, Dasyatidae, Scaridae)

select_abundance <- select_abundance[,-1]

# compute important families density
select_density <- select_abundance / 300

# select important families size
select_size <- Fish_family_size_site_spread %>%
  select(Acanthuridae, Labridae, Lutjanidae, Serranidae, Epinephelidae, Carcharhinidae, Dasyatidae, Scaridae)

select_size <- select_size[,-1]

# select important families biomass
select_biomass <- Fish_family_biomass_site %>%
  select(Acanthuridae, Labridae, Lutjanidae, Serranidae, Epinephelidae, Carcharhinidae, Dasyatidae, Scaridae)

select_biomass <- select_biomass[,-1]


# update colnames
colnames(select_abundance) <- paste(colnames(select_abundance), "Abundance", sep="_")
colnames(select_density) <- paste(colnames(select_density), "Density", sep="_")
colnames(select_size) <- paste(colnames(select_size), "MeanSize", sep="_")
colnames(select_biomass) <- paste(colnames(select_biomass), "Biomass", sep="_")

# add column Site_ID
select_abundance$Site_ID <- Fish_family_abund_site$Site_ID
select_density$Site_ID <- Fish_family_abund_site$Site_ID
select_size$Site_ID <- Fish_family_size_site_spread$Site_ID
select_biomass$Site_ID <- Fish_family_biomass_site$Site_ID

# Join together
select_families <- full_join(select_abundance, select_density)
select_families <- full_join(select_families, select_size)
select_families <- full_join(select_families, select_biomass)

# save data
save(select_families, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Selected_families_data.rdata")


#########################################################################################
#### Abundance, density and biomass of Trophic group per site ####

# Sum of abundance of each trophic group per site
Fish_trophic_abund_site <- Fish %>%
  select(Site_ID, trophic_group, Number)%>%
  group_by(Site_ID, trophic_group) %>% 
  summarise_all(funs(sum))

save(Fish_trophic_abund_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Trophic_abundance.rdata")

# Transpose dataframe to have site in lines and trophic groups in columns
Fish_trophic_abund_site <- spread(data=Fish_trophic_abund_site, key="trophic_group", value="Number", fill = 0)


# compute densities
Fish_trophic_density_site <- Fish_trophic_abund_site[,-1]/300
colnames(Fish_trophic_density_site) <- paste(colnames(Fish_trophic_density_site), "Density", sep="_")

Fish_trophic_density_site$Site_ID <- Fish_trophic_abund_site$Site_ID

# update colnames of abundance dataframe
Fish_trophic_abund_site <- Fish_trophic_abund_site[,-1]
colnames(Fish_trophic_abund_site) <- paste(colnames(Fish_trophic_abund_site), "Abundance", sep="_")


# Sum of biomass of each trophic group per site
Fish_trophic_biomass_site <- Fish %>%
  select(Site_ID, trophic_group, Biomass)%>%
  filter(!is.na(Biomass))%>%
  group_by(Site_ID, trophic_group) %>% 
  summarise_all(funs(sum))

save(Fish_trophic_biomass_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Trophic_biomass.rdata")

# Transpose dataframe to have site in lines and trophic groups in columns
Fish_trophic_biomass_site <- spread(data=Fish_trophic_biomass_site, key="trophic_group", value="Biomass", fill = 0)

# update colnames of abundance dataframe
Fish_trophic_biomass_site <- Fish_trophic_biomass_site[,-1]
colnames(Fish_trophic_biomass_site) <- paste(colnames(Fish_trophic_biomass_site), "Biomass", sep="_")

Fish_trophic_site <- cbind(Fish_trophic_density_site,Fish_trophic_abund_site, Fish_trophic_biomass_site)

# Save Rdata
save(Fish_trophic_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_trophic_site.rdata")

