library(tidyverse)

########################################################################################
#### Fish ####

# load fish data
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_species_abund_site.rdata")
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_species_biomass_site.rdata")
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_family_abund_site.rdata")
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_family_size_site_spread.rdata")
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Selected_families_data.rdata")
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_trophic_site.rdata")


# assemble fish data
Site_biodiv <- Fish_species_abund_site %>%
  select(Year, Site, Site_ID, Fish_abundance, Fish_richness, Fish_density)

Site_biodiv <- full_join(Site_biodiv, Fish_species_biomass_site[,c("Site_ID", "Fish_biomass")])

Site_biodiv <- full_join(Site_biodiv, Fish_family_abund_site[,c("Site_ID", "Family_richness")])

Site_biodiv <- full_join(Site_biodiv, select_families)

Site_biodiv <- full_join(Site_biodiv, Fish_trophic_site)


########################################################################################
#### Macro-invertebrate ####

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Invert_abund_density_site.rdata")

Site_biodiv <- full_join(Site_biodiv, Invert_abund_site[,-c(1,2)])


########################################################################################
#### Algae ####

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Algae_size_site.rdata")

Site_biodiv <- full_join(Site_biodiv, Algae_size_site)


########################################################################################
#### Recruits ####

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Recruits_abund_site.rdata")

Site_biodiv <- full_join(Site_biodiv, Recruits_abund_site[,c(1,15:18)])


#### save data ####
save(Site_biodiv, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Site_biodiversity_metrics.rdata")




###########################################################################################################################
### Fish metrics

# Global metrics

Fish_metrics_site <- Site_biodiv[,c(1,2,4:8)]

write.csv(Fish_metrics_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Fish_metrics_site.csv", row.names = F)

Fish_metrics_year <- data.frame(matrix(ncol = 6, nrow = 0))
names(Fish_metrics_year) <- c("Year","Mean_Fish_abundance","Mean_Fish_richness","Mean_Fish_density","Mean_Fish_biomass","Mean_Family_richness")

year <- unique(Fish_metrics_site$Year)

for (i in 1:length(year)) {
  df <- Fish_metrics_site %>%
    filter(Year==year[[i]])
  Fish_metrics_year[i,"Year"] <- year[[i]]
  Fish_metrics_year[i,"Mean_Fish_abundance"] <- paste0(round(mean(df$Fish_abundance,na.rm = T),3), " ± ", round(sd(df$Fish_abundance,na.rm = T),3))
  Fish_metrics_year[i,"Mean_Fish_richness"] <- paste0(round(mean(df$Fish_richness,na.rm = T),3), " ± ", round(sd(df$Fish_richness,na.rm = T),3))
  Fish_metrics_year[i,"Mean_Fish_density"] <- paste0(round(mean(df$Fish_density,na.rm = T),3), " ± ", round(sd(df$Fish_density,na.rm = T),3))
  Fish_metrics_year[i,"Mean_Fish_biomass"] <- paste0(round(mean(df$Fish_biomass,na.rm = T),3), " ± ", round(sd(df$Fish_biomass,na.rm = T),3))
  Fish_metrics_year[i,"Mean_Family_richness"] <- paste0(round(mean(df$Family_richness,na.rm = T),3), " ± ", round(sd(df$Family_richness,na.rm = T),3))
}



Fish_metrics_year <- as.data.frame(t(Fish_metrics_year))
colnames(Fish_metrics_year) <- Fish_metrics_year[1,]
Fish_metrics_year <- Fish_metrics_year[-1,]


write.csv(Fish_metrics_year, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Fish_metrics_year.csv", row.names = T)


# Family density

Family_density_site <- Site_biodiv[,c(1,2,17:24)]

write.csv(Family_density_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family_density_site.csv", row.names = F)

Family_density_year <- data.frame(matrix(ncol = 9, nrow = 0))
names(Family_density_year) <- c("Year","Mean_Acanthuridae_Density","Mean_Labridae_Density","Mean_Lutjanidae_Density",
                                "Mean_Serranidae_Density","Mean_Epinephelidae_Density","Mean_Carcharhinidae_Density",
                                "Mean_Dasyatidae_Density","Mean_Scaridae_Density")

year <- unique(Family_density_site$Year)

for (i in 1:length(year)) {
  df <- Family_density_site %>%
    filter(Year==year[[i]])
  Family_density_year[i,"Year"] <- year[[i]]
  Family_density_year[i,"Mean_Acanthuridae_Density"] <- paste0(round(mean(df$Acanthuridae_Density,na.rm = T),3), " ± ", round(sd(df$Acanthuridae_Density,na.rm = T),3))
  Family_density_year[i,"Mean_Labridae_Density"] <- paste0(round(mean(df$Labridae_Density,na.rm = T),3), " ± ", round(sd(df$Labridae_Density,na.rm = T),3))
  Family_density_year[i,"Mean_Lutjanidae_Density"] <- paste0(round(mean(df$Lutjanidae_Density,na.rm = T),3), " ± ", round(sd(df$Lutjanidae_Density,na.rm = T),3))
  Family_density_year[i,"Mean_Serranidae_Density"] <- paste0(round(mean(df$Serranidae_Density,na.rm = T),3), " ± ", round(sd(df$Serranidae_Density,na.rm = T),3))
  Family_density_year[i,"Mean_Epinephelidae_Density"] <- paste0(round(mean(df$Epinephelidae_Density,na.rm = T),3), " ± ", round(sd(df$Epinephelidae_Density,na.rm = T),3))
  Family_density_year[i,"Mean_Carcharhinidae_Density"] <- paste0(round(mean(df$Carcharhinidae_Density,na.rm = T),3), " ± ", round(sd(df$Carcharhinidae_Density,na.rm = T),3))
  Family_density_year[i,"Mean_Dasyatidae_Density"] <- paste0(round(mean(df$Dasyatidae_Density,na.rm = T),3), " ± ", round(sd(df$Dasyatidae_Density,na.rm = T),3))
  Family_density_year[i,"Mean_Scaridae_Density"] <- paste0(round(mean(df$Scaridae_Density,na.rm = T),3), " ± ", round(sd(df$Scaridae_Density,na.rm = T),3))
}



Family_density_year <- as.data.frame(t(Family_density_year))
colnames(Family_density_year) <- Family_density_year[1,]
Family_density_year <- Family_density_year[-1,]


write.csv(Family_density_year, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family_density_year.csv", row.names = T)


# Family biomass

Family_biomass_site <- Site_biodiv[,c(1,2,33:40)]

write.csv(Family_biomass_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family_biomass_site.csv", row.names = F)

Family_biomass_year <- data.frame(matrix(ncol = 9, nrow = 0))
names(Family_biomass_year) <- c("Year","Mean_Acanthuridae_Biomass","Mean_Labridae_Biomass","Mean_Lutjanidae_Biomass",
                                "Mean_Serranidae_Biomass","Mean_Epinephelidae_Biomass","Mean_Carcharhinidae_Biomass",
                                "Mean_Dasyatidae_Biomass","Mean_Scaridae_Biomass")

year <- unique(Family_biomass_site$Year)

for (i in 1:length(year)) {
  df <- Family_biomass_site %>%
    filter(Year==year[[i]])
  Family_biomass_year[i,"Year"] <- year[[i]]
  Family_biomass_year[i,"Mean_Acanthuridae_Biomass"] <- paste0(round(mean(df$Acanthuridae_Biomass,na.rm = T),3), " ± ", round(sd(df$Acanthuridae_Biomass,na.rm = T),3))
  Family_biomass_year[i,"Mean_Labridae_Biomass"] <- paste0(round(mean(df$Labridae_Biomass,na.rm = T),3), " ± ", round(sd(df$Labridae_Biomass,na.rm = T),3))
  Family_biomass_year[i,"Mean_Lutjanidae_Biomass"] <- paste0(round(mean(df$Lutjanidae_Biomass,na.rm = T),3), " ± ", round(sd(df$Lutjanidae_Biomass,na.rm = T),3))
  Family_biomass_year[i,"Mean_Serranidae_Biomass"] <- paste0(round(mean(df$Serranidae_Biomass,na.rm = T),3), " ± ", round(sd(df$Serranidae_Biomass,na.rm = T),3))
  Family_biomass_year[i,"Mean_Epinephelidae_Biomass"] <- paste0(round(mean(df$Epinephelidae_Biomass,na.rm = T),3), " ± ", round(sd(df$Epinephelidae_Biomass,na.rm = T),3))
  Family_biomass_year[i,"Mean_Carcharhinidae_Biomass"] <- paste0(round(mean(df$Carcharhinidae_Biomass,na.rm = T),3), " ± ", round(sd(df$Carcharhinidae_Biomass,na.rm = T),3))
  Family_biomass_year[i,"Mean_Dasyatidae_Biomass"] <- paste0(round(mean(df$Dasyatidae_Biomass,na.rm = T),3), " ± ", round(sd(df$Dasyatidae_Biomass,na.rm = T),3))
  Family_biomass_year[i,"Mean_Scaridae_Biomass"] <- paste0(round(mean(df$Scaridae_Biomass,na.rm = T),3), " ± ", round(sd(df$Scaridae_Biomass,na.rm = T),3))
}



Family_biomass_year <- as.data.frame(t(Family_biomass_year))
colnames(Family_biomass_year) <- Family_biomass_year[1,]
Family_biomass_year <- Family_biomass_year[-1,]


write.csv(Family_biomass_year, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family_biomass_year.csv", row.names = T)





# Trophic density

Trophic_density_site <- Site_biodiv[,c(1,2,41:44)]

write.csv(Trophic_density_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic_density_site.csv", row.names = F)

Trophic_density_year <- data.frame(matrix(ncol = 5, nrow = 0))
names(Trophic_density_year) <- c("Year","Mean_Herbivorous_Density","Mean_Invertivorous_Density","Mean_Omnivorous_Density",
                                "Mean_Piscivorous_Density")

year <- unique(Trophic_density_site$Year)

for (i in 1:length(year)) {
  df <- Trophic_density_site %>%
    filter(Year==year[[i]])
  Trophic_density_year[i,"Year"] <- year[[i]]
  Trophic_density_year[i,"Mean_Herbivorous_Density"] <- paste0(round(mean(df$Herbivorous_Density,na.rm = T),3), " ± ", round(sd(df$Herbivorous_Density,na.rm = T),3))
  Trophic_density_year[i,"Mean_Invertivorous_Density"] <- paste0(round(mean(df$Invertivorous_Density,na.rm = T),3), " ± ", round(sd(df$Invertivorous_Density,na.rm = T),3))
  Trophic_density_year[i,"Mean_Omnivorous_Density"] <- paste0(round(mean(df$Omnivorous_Density,na.rm = T),3), " ± ", round(sd(df$Omnivorous_Density,na.rm = T),3))
  Trophic_density_year[i,"Mean_Piscivorous_Density"] <- paste0(round(mean(df$Piscivorous_Density,na.rm = T),3), " ± ", round(sd(df$Piscivorous_Density,na.rm = T),3))
  }



Trophic_density_year <- as.data.frame(t(Trophic_density_year))
colnames(Trophic_density_year) <- Trophic_density_year[1,]
Trophic_density_year <- Trophic_density_year[-1,]


write.csv(Trophic_density_year, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic_density_year.csv", row.names = T)


# Trophic biomass

Trophic_biomass_site <- Site_biodiv[,c(1,2,49:52)]

write.csv(Trophic_biomass_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic_biomass_site.csv", row.names = F)

Trophic_biomass_year <- data.frame(matrix(ncol = 5, nrow = 0))
names(Trophic_biomass_year) <- c("Year","Mean_Herbivorous_Biomass","Mean_Invertivorous_Biomass","Mean_Omnivorous_Biomass",
                                "Mean_Piscivorous_Biomass")

year <- unique(Trophic_biomass_site$Year)

for (i in 1:length(year)) {
  df <- Trophic_biomass_site %>%
    filter(Year==year[[i]])
  Trophic_biomass_year[i,"Year"] <- year[[i]]
  Trophic_biomass_year[i,"Mean_Herbivorous_Biomass"] <- paste0(round(mean(df$Herbivorous_Biomass,na.rm = T),3), " ± ", round(sd(df$Herbivorous_Biomass,na.rm = T),3))
  Trophic_biomass_year[i,"Mean_Invertivorous_Biomass"] <- paste0(round(mean(df$Invertivorous_Biomass,na.rm = T),3), " ± ", round(sd(df$Invertivorous_Biomass,na.rm = T),3))
  Trophic_biomass_year[i,"Mean_Omnivorous_Biomass"] <- paste0(round(mean(df$Omnivorous_Biomass,na.rm = T),3), " ± ", round(sd(df$Omnivorous_Biomass,na.rm = T),3))
  Trophic_biomass_year[i,"Mean_Piscivorous_Biomass"] <- paste0(round(mean(df$Piscivorous_Biomass,na.rm = T),3), " ± ", round(sd(df$Piscivorous_Biomass,na.rm = T),3))
 }



Trophic_biomass_year <- as.data.frame(t(Trophic_biomass_year))
colnames(Trophic_biomass_year) <- Trophic_biomass_year[1,]
Trophic_biomass_year <- Trophic_biomass_year[-1,]


write.csv(Trophic_biomass_year, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic_biomass_year.csv", row.names = T)
