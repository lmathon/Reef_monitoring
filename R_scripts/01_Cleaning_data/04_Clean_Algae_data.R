library(tidyverse)

Algae <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/2 - Raw_data/2 - CSV/GCRMN_Algae.csv", sep=";")

# Update 2017 to 2017_pre and 2017_post

for (i in 1:nrow(Algae)) {
  if(Algae[i,"Year"]=="2017" & Algae[i,"Month"]=="August"){
    Algae[i,"Year"] <- "2017_pre"
  }
  if(Algae[i,"Year"]=="2017" & Algae[i,"Month"]%in%c("November","December")){
    Algae[i,"Year"] <- "2017_post"
  }
}


# Create unique ID per site
Algae$Site_ID <- paste0(Algae$Year, "-", Algae$Site)

Algae$Macro.Height..cm. <- as.numeric(Algae$Macro.Height..cm.)

# Compute mean algae size per site
Algae_size_sites_mean <- Algae %>%
  select(Site_ID, Macro.Height..cm.)%>%
  group_by(Site_ID) %>% 
  summarise_all(funs(mean))
colnames(Algae_size_sites_mean) <- c("Site_ID", "MacroAlgae_MeanHeight")

Algae_size_sites_sd <- Algae %>%
  select(Site_ID, Macro.Height..cm.)%>%
  group_by(Site_ID) %>% 
  summarise_all(funs(sd))
colnames(Algae_size_sites_sd) <- c("Site_ID", "MacroAlgae_sdHeight")

# Compute mean turf size per site
Turf_size_sites_mean <- Algae %>%
  select(Site_ID, Turf.Height..mm.)%>%
  group_by(Site_ID) %>% 
  summarise_all(funs(mean))
colnames(Turf_size_sites_mean) <- c("Site_ID", "Turf_MeanHeight")

Turf_size_sites_sd <- Algae %>%
  select(Site_ID, Turf.Height..mm.)%>%
  group_by(Site_ID) %>% 
  summarise_all(funs(sd))
colnames(Turf_size_sites_sd) <- c("Site_ID", "Turf_sdHeight")

# Assemble all

Algae_size_site <- left_join(Algae_size_sites_mean, Algae_size_sites_sd)
Algae_size_site <- left_join(Algae_size_site, Turf_size_sites_mean)
Algae_size_site <- left_join(Algae_size_site, Turf_size_sites_sd)


# save data
save(Algae_size_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Algae_size_site.rdata")


# build table mean ± SD per year
Algae_year <- data.frame(matrix(ncol = 3, nrow = 0))
names(Algae_year) <- c("Year", "Mean_Macroalgae_height", "Mean_Turf_height")

year <- unique(Algae$Year)

for (i in 1:length(year)) {
  df <- Algae %>%
    filter(Year==year[[i]])
  Algae_year[i,"Year"] <- year[[i]]
  Algae_year[i,"Mean_Macroalgae_height"] <- paste0(round(mean(df$Macro.Height..cm.),3), " ± ", round(sd(df$Macro.Height..cm.),3))
  Algae_year[i,"Mean_Turf_height"] <- paste0(round(mean(df$Turf.Height..mm., na.rm = T),3), " ± ", round(sd(df$Turf.Height..mm., na.rm = T),3))
  
}


Algae_year <- as.data.frame(t(Algae_year))
colnames(Algae_year) <- Algae_year[1,]
Algae_year <- Algae_year[-1,]


write.csv(Algae_year, file = "c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Algae_metrics_year.csv", row.names = T)


# build table mean ± SD per site
Algae_site <- data.frame(matrix(ncol = 3, nrow = 0))
names(Algae_site) <- c("Site_ID", "Mean_Macroalgae_height", "Mean_Turf_height")

site_ID <- unique(Algae$Site_ID)

for (i in 1:length(site_ID)) {
  df <- Algae %>%
    filter(Site_ID==site_ID[[i]])
  Algae_site[i,"Site_ID"] <- site_ID[[i]]
  Algae_site[i,"Mean_Macroalgae_height"] <- paste0(round(mean(df$Macro.Height..cm.),3), " ± ", round(sd(df$Macro.Height..cm.),3))
  Algae_site[i,"Mean_Turf_height"] <- paste0(round(mean(df$Turf.Height..mm., na.rm = T),3), " ± ", round(sd(df$Turf.Height..mm., na.rm = T),3))
  
}

Algae_site[c('Year', 'Site')] <- str_split_fixed(Algae_site$Site_ID, '-', 2)

write.csv(Algae_site, file = "c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Algae_metrics_sites.csv", row.names = T)
