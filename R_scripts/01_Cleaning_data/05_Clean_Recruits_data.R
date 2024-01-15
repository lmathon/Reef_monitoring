library(tidyverse)

Recruits <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/2 - Raw_data/2 - CSV/GCRMN_Recruits.csv", sep=";")


# Update 2017 to 2017_pre and 2017_post

for (i in 1:nrow(Recruits)) {
  if(Recruits[i,"Year"]=="2017" & Recruits[i,"Month"]=="August"){
    Recruits[i,"Year"] <- "2017_pre"
  }
  if(Recruits[i,"Year"]=="2017" & Recruits[i,"Month"]%in%c("November","December")){
    Recruits[i,"Year"] <- "2017_post"
  }
}

# Create unique ID per site
Recruits$Site_ID <- paste0(Recruits$Year, "-", Recruits$Site)
Recruits <- Recruits %>%
  filter(Site_ID!="NA-")

# add a column to compute total recruit number per site
Recruits$Number <- 1

# Sum of abundance of each species per transect
Recruits_abund_site <- Recruits %>%
  select(Site_ID, GenusSN, Number)%>%
  group_by(Site_ID, GenusSN) %>% 
  summarise_all(funs(sum))

save(Recruits_abund_site, file="c://Users/mathonlocal/Desktop/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Recruit_abund.rdata")


# Transpose dataframe to have sites in lines and genus in columns
Recruits_abund_site <- spread(data=Recruits_abund_site, key="GenusSN", value="Number", fill = 0)
Recruits_abund_site <- Recruits_abund_site[,-c(2)]

# Compute recruit abundance per site
Recruits_abund_site$Recruits_abundance <- rowSums(Recruits_abund_site[,c(2:ncol(Recruits_abund_site))])

# Compute recruit density per site (area per site = 0.9375m2)
Recruits_abund_site$Recruits_density <- round(Recruits_abund_site$Recruits_abundance/0.9375,3)

# Compute mean recruit size per site
Recruit_size_sites_mean <- Recruits %>%
  select(Site_ID, Size_cm)%>%
  group_by(Site_ID) %>% 
  summarise_all(funs(mean))
colnames(Recruit_size_sites_mean) <- c("Site_ID", "Recruits_MeanSize")


# compute SD recruit size per site
Recruit_size_sites_sd <- Recruits %>%
  select(Site_ID,Size_cm)%>%
  group_by(Site_ID) %>% 
  summarise_all(funs(sd))
colnames(Recruit_size_sites_sd) <- c("Site_ID", "Recruits_sdSize")

Recruits_abund_site <- left_join(Recruits_abund_site, Recruit_size_sites_mean)
Recruits_abund_site <- left_join(Recruits_abund_site, Recruit_size_sites_sd)

# save data
save(Recruits_abund_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Recruits_abund_site.rdata")

# build table mean ± SD per site
Recruit_site <- data.frame(matrix(ncol = 2, nrow = 0))
names(Recruit_site) <- c("Site_ID", "Mean_Recruit_size")

site_ID <- unique(Recruits$Site_ID)

for (i in 1:length(site_ID)) {
  df <- Recruits %>%
    filter(Site_ID==site_ID[[i]])
  Recruit_site[i,"Site_ID"] <- site_ID[[i]]
  Recruit_site[i,"Mean_Recruit_size"] <- paste0(round(mean(df$Size_cm,na.rm = T),3), " ± ", round(sd(df$Size_cm,na.rm = T),3))
  
}

Recruit_site <- left_join(Recruit_site, Recruits_abund_site[,-c(18,19)])
Recruit_site[c('Year', 'Site')] <- str_split_fixed(Recruit_site$Site_ID, '-', 2)

write.csv(Recruit_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Recruit_metrics_sites.csv", row.names = F)

# build table mean ± SD per year

Recruits_abund_year_mean <- Recruit_site %>%
  select(Year,Recruits_abundance,Recruits_density)%>%
  group_by(Year) %>% 
  summarise_all(funs(mean))

Recruits_abund_year_sd <- Recruit_site %>%
  select(Year,Recruits_abundance,Recruits_density)%>%
  group_by(Year) %>% 
  summarise_all(funs(sd))

Recruit_year <- data.frame(matrix(ncol = 4, nrow = 0))
names(Recruit_year) <- c("Year", "Mean_Recruit_size", "Mean_Recruit_abundance", "Mean_Recruit_density")

year <- unique(Recruits$Year)

for (i in 1:length(year)) {
  df <- Recruits %>%
    filter(Year==year[[i]])
  Recruit_year[i,"Year"] <- year[[i]]
  Recruit_year[i,"Mean_Recruit_size"] <- paste0(round(mean(df$Size_cm,na.rm = T),3), " ± ", round(sd(df$Size_cm,na.rm = T),3))
  Recruit_year[i,"Mean_Recruit_abundance"] <- paste0(round(Recruits_abund_year_mean[i,"Recruits_abundance"],3), " ± ", round(Recruits_abund_year_sd[i,"Recruits_abundance"],3))
  Recruit_year[i,"Mean_Recruit_density"] <- paste0(round(Recruits_abund_year_mean[i,"Recruits_density"],3), " ± ", round(Recruits_abund_year_sd[i,"Recruits_density"],3))
}



Recruit_year <- as.data.frame(t(Recruit_year))
colnames(Recruit_year) <- Recruit_year[1,]
Recruit_year <- Recruit_year[-1,]


write.csv(Recruit_year, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Recruit_metrics_year.csv", row.names = T)
