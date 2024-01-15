library(tidyverse)

Invert_abund <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/2 - Raw_data/2 - CSV/GCRMN_Macro-invert.csv", sep=";")

# Update 2017 to 2017_pre and 2017_post
for (i in 1:nrow(Invert_abund)) {
  if(Invert_abund[i,"Year"]=="2017" & Invert_abund[i,"Month"]=="August"){
    Invert_abund[i,"Year"] <- "2017_pre"
  }
  if(Invert_abund[i,"Year"]=="2017" & Invert_abund[i,"Month"]%in%c("November","December")){
    Invert_abund[i,"Year"] <- "2017_post"
  }
}

# add Site_ID 
Invert_abund$Site_ID <- paste0(Invert_abund$Year, "-", Invert_abund$Site)
Invert_abund <- Invert_abund %>%
  filter(Site_ID!="NA-")


Invert_abund_site <- data.frame(matrix(ncol = 13, nrow = 0))
names(Invert_abund_site) <- c("Year", "Site", "Site_ID", "LongspineSeaUrchin_Abundance", "LongspineSeaUrchin_Density", "OtherSeaUrchin_Abundance", "OtherSeaUrchin_Density", "SeaCucumbers_Abundance", "SeaCucumbers_Density", "Lobster_Abundance", "Lobster_Density", "Conch_Abundance", "Conch_Density")

Sites <- unique(Invert_abund$Site_ID)

for (i in 1:length(Sites)) {
  df <- Invert_abund %>%
    filter(Site_ID==Sites[[i]])
  Invert_abund_site[i,"Year"] <- unique(df$Year)
  Invert_abund_site[i,"Site"] <- unique(df$Site)
  Invert_abund_site[i,"Site_ID"] <- unique(df$Site_ID)
  Invert_abund_site[i,"LongspineSeaUrchin_Abundance"] <- sum(df[,"Longspine.Sea.Urchin..n."])
  Invert_abund_site[i,"LongspineSeaUrchin_Density"] <- Invert_abund_site[i,"LongspineSeaUrchin_Abundance"]/60
  Invert_abund_site[i,"OtherSeaUrchin_Abundance"] <- sum(df[,"Other.Sea.Urchin..n."])
  Invert_abund_site[i,"OtherSeaUrchin_Density"] <- Invert_abund_site[i,"OtherSeaUrchin_Abundance"]/60
  Invert_abund_site[i,"SeaCucumbers_Abundance"] <- sum(df[,"Sea.Cucumbers..n."])
  Invert_abund_site[i,"SeaCucumbers_Density"] <- Invert_abund_site[i,"SeaCucumbers_Abundance"]/60
  Invert_abund_site[i,"Lobster_Abundance"] <- sum(df[,"Lobster..n."])
  Invert_abund_site[i,"Lobster_Density"] <- Invert_abund_site[i,"Lobster_Abundance"]/60
  Invert_abund_site[i,"Conch_Abundance"] <- sum(df[,"Conch..n."])
  Invert_abund_site[i,"Conch_Density"] <- Invert_abund_site[i,"Conch_Abundance"]/60
  
}

save(Invert_abund_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Invert_abund_density_site.rdata")
write.csv(Invert_abund_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Invertebrate_metrics_sites.csv", row.names = F)


Invert_abund_year <- data.frame(matrix(ncol = 11, nrow = 0))
names(Invert_abund_year) <- c("Year", "Mean_LongspineSeaUrchin_Abundance", "Mean_LongspineSeaUrchin_Density",
                              "Mean_OtherSeaUrchin_Abundance", "Mean_OtherSeaUrchin_Density", 
                              "Mean_SeaCucumbers_Abundance", "Mean_SeaCucumbers_Density", 
                              "Mean_Lobster_Abundance", "Mean_Lobster_Density", 
                              "Mean_Conch_Abundance", "Mean_Conch_Density")

year <- unique(Invert_abund_site$Year)

for (i in 1:length(year)) {
  df <- Invert_abund_site %>%
    filter(Year==year[[i]])
  Invert_abund_year[i,"Year"] <- year[[i]]
  Invert_abund_year[i,"Mean_LongspineSeaUrchin_Abundance"] <- paste0(round(mean(df$LongspineSeaUrchin_Abundance),3), " ± ", round(sd(df$LongspineSeaUrchin_Abundance),3))
  Invert_abund_year[i,"Mean_LongspineSeaUrchin_Density"] <- paste0(round(mean(df$LongspineSeaUrchin_Density),3), " ± ", round(sd(df$LongspineSeaUrchin_Density),3))
  Invert_abund_year[i,"Mean_OtherSeaUrchin_Abundance"] <- paste0(round(mean(df$OtherSeaUrchin_Abundance),3), " ± ", round(sd(df$OtherSeaUrchin_Abundance),3))
  Invert_abund_year[i,"Mean_OtherSeaUrchin_Density"] <- paste0(round(mean(df$OtherSeaUrchin_Density),3), " ± ", round(sd(df$OtherSeaUrchin_Density),3))
  Invert_abund_year[i,"Mean_SeaCucumbers_Abundance"] <- paste0(round(mean(df$SeaCucumbers_Abundance),3), " ± ", round(sd(df$SeaCucumbers_Abundance),3))
  Invert_abund_year[i,"Mean_SeaCucumbers_Density"] <- paste0(round(mean(df$SeaCucumbers_Density),3), " ± ", round(sd(df$SeaCucumbers_Density),3))
  Invert_abund_year[i,"Mean_Lobster_Abundance"] <- paste0(round(mean(df$Lobster_Abundance),3), " ± ", round(sd(df$Lobster_Abundance),3))
  Invert_abund_year[i,"Mean_Lobster_Density"] <- paste0(round(mean(df$Lobster_Density),3), " ± ", round(sd(df$Lobster_Density),3))
  Invert_abund_year[i,"Mean_Conch_Abundance"] <- paste0(round(mean(df$Conch_Abundance),3), " ± ", round(sd(df$Conch_Abundance),3))
  Invert_abund_year[i,"Mean_Conch_Density"] <- paste0(round(mean(df$Conch_Density),3), " ± ", round(sd(df$Conch_Density),3))
   
}

Invert_abund_year <- as.data.frame(t(Invert_abund_year))
colnames(Invert_abund_year) <- Invert_abund_year[1,]
Invert_abund_year <- Invert_abund_year[-1,]

write.csv(Invert_abund_year, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Invertebrate_metrics_year.csv", row.names = T)
