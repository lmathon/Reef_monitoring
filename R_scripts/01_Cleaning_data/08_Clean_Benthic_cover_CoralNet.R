library(tidyverse)

Benthic_cover <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/2 - Raw_data/2 - CSV/percent_covers.csv", sep=",")
Benthic_cover <- Benthic_cover[-nrow(Benthic_cover),]

Benthic_cover$Site_ID <- sub("_T.*", "",Benthic_cover$Image.name)
Benthic_cover$Site_ID <- gsub("_", "-", Benthic_cover$Site_ID)

names(Benthic_cover) <- gsub("\\.", " ", names(Benthic_cover))

# Correct site names
Benthic_cover$Site_ID <- gsub("2018-Mikes Maze", "2018-Mikes Mazes", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-Fishbowl", "2019-Fish Bowl", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-FishBowl", "2019-Fish Bowl", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-FrenchmanReef", "2019-Frenchman Reef", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-Long Bay Reef- T2P1000873.JPG", "2019-Long Bay Reef", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-Long Bay", "2019-Long Bay Reef", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-Long Bay Reef Reef", "2019-Long Bay Reef", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-Mikes Maze", "2019-Mikes Mazes", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-MikesMaze", "2019-Mikes Mazes", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-Mike'sMaze", "2019-Mikes Mazes", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2019-SharkHotel", "2019-Shark Hotel", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2020-Proselyte", "2020-Proselyte Reef", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2020-Mikes Maze", "2020-Mikes Mazes", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2023-LongBayReef", "2023-Long Bay Reef", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2023-FrenchmanReef", "2023-Frenchman Reef", Benthic_cover$Site_ID)
Benthic_cover$Site_ID <- gsub("2023-Mikes Maze", "2023-Mikes Mazes", Benthic_cover$Site_ID)

Benthic_cover_site2 <- Benthic_cover[,c(113,5:112)]

# Compute cover per site
Benthic_cover_site2 <- Benthic_cover_site2 %>%
  group_by(Site_ID)%>%
  summarise_all(funs(mean))

# Correct names to match with CPCe
colnames(Benthic_cover_site2) <- gsub(" spp ","", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Coral Juvenile","Coral juvenile", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub(" sp ","", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("CCA  crustose coralline algae ","Coralline algae", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Turf algae","Turf", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Turbinaria  algae ","Turbinaria", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Dead Gorgonian","Dead gorgonian", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Dead coral algae","Dead coral with algae", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("other disease","Other disease", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("white band disease","White Band Disease", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("White Plague  type II","Plague, Type II", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Old dead coral  encrusting ","Old dead coral", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Diseased Coral","Diseased coral", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Yellow band disease","Yellow Blotch Disease", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Other like" ,"OTHER LIVE" , colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Montastraea faveolata","Orbicella faveolata", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Montastrea cavernosa","Montastraea cavernosa", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Mycetophyllia lamarckana","Mycetophyllia lamarckiana", colnames(Benthic_cover_site2))
colnames(Benthic_cover_site2) <- gsub("Stephanocoenia michellini","Stephanocoenia intersepta", colnames(Benthic_cover_site2))

# Add column from CPCe missing in CoralNet
Benthic_cover_site2$Peyssonnelid <- 0
Benthic_cover_site2$Cyanobacteria <- 0
Benthic_cover_site2$Tunicate <- 0

# Remove unwanted columns
Benthic_cover_site2 <- Benthic_cover_site2 %>%
  dplyr::select(-c(Shadow, TAPE, Wand, `Diploria stigosa`))

# Compute general categories as with CPCe
Benthic_cover_site2$CORAL <- rowSums(Benthic_cover_site2[,c(2:21,23:40,42:47,49:56,64:66)])
Benthic_cover_site2$`DISEASED CORALS` <- rowSums(Benthic_cover_site2[,c(22,81,83,88:90)])
Benthic_cover_site2$`CORALLINE ALGAE` <- rowSums(Benthic_cover_site2[,92])
Benthic_cover_site2$`DEAD CORAL WITH ALGAE` <- rowSums(Benthic_cover_site2[,c(41,48,59,78)])
Benthic_cover_site2$SPONGES <- rowSums(Benthic_cover_site2[,75])
Benthic_cover_site2$ZOANTHIDS <- rowSums(Benthic_cover_site2[,c(69,76)])
Benthic_cover_site2$`SAND, PAVEMENT, RUBBLE` <- rowSums(Benthic_cover_site2[,c(77,79,85)])
Benthic_cover_site2$GORGONIANS <- rowSums(Benthic_cover_site2[,c(58,60:63,67,68,70:74)])
Benthic_cover_site2$MACROALGAE <- rowSums(Benthic_cover_site2[,c(91,93:106)])
Benthic_cover_site2$`OTHER LIVE` <- rowSums(Benthic_cover_site2[,c(57,80,82,84,107,108)])
Benthic_cover_site2$UNKNOWNS <- rowSums(Benthic_cover_site2[,87])

# Compute number of frames per site
unique_site <- unique(Benthic_cover_site2$Site_ID)

for (i in 1:length(unique_site)) {
  df <- Benthic_cover %>%
    filter(Site_ID==unique_site[[i]])
  Benthic_cover_site2[i,"Number of frames"] <- nrow(df)
}

# load CPCe data
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Benthic_cover_site_CPCe.rdata")

# Add columns from CoralNet missing in CPCe
Benthic_cover_site$`Agaricia humilis` <- 0
Benthic_cover_site$Rhodophyta <- 0
Benthic_cover_site <- Benthic_cover_site %>%
  dplyr::select(-c("Halophylla"))

Benthic_cover_site$Site_ID <- gsub("PRE 2017-Proselyte Reef", "2017_pre-Proselyte Reef", Benthic_cover_site$Site_ID)

# Check if all names match
names_CoralNet <- names(Benthic_cover_site2[,-1])
names_CPCe <- names(Benthic_cover_site[,-1])
common_names <- intersect(names_CoralNet, names_CPCe)

# Reorder columns and assemble both dataframes
Benthic_cover_site2 <- Benthic_cover_site2[,names(Benthic_cover_site)]

Benthic_cover_site_all <- rbind(Benthic_cover_site, Benthic_cover_site2)

# load bleaching / disease data
bleach_disease <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Bleaching_Disease.csv", sep = ";", na.strings = "")

Benthic_cover_site_all <- left_join(Benthic_cover_site_all, bleach_disease)

Benthic_cover_site_all$Bleaching_frequency <- Benthic_cover_site_all$Number_frames_bleaching/Benthic_cover_site_all$`Number of frames`
Benthic_cover_site_all$Disease_frequency <- Benthic_cover_site_all$Number_frames_disease/Benthic_cover_site_all$`Number of frames`


save(Benthic_cover_site_all, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Benthic_cover_site_all.rdata")


# Build table of metrics per site and year

Cover_site <- Benthic_cover_site_all[,c(1,3:13,122,123)]

Cover_site[c('Year', 'Site')] <- str_split_fixed(Cover_site$Site_ID, '-', 2)
Cover_site <- Cover_site[,-1]

write.csv(Cover_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Cover_site.csv", row.names = F)


Cover_year <- data.frame(matrix(ncol = 13, nrow = 0))
names(Cover_year) <- c("Year","Mean_Coral_cover","Mean_Gorgonians_cover","Mean_Sponge_cover","Mean_Zoanthid_cover",
                       "Mean_Macroalge_cover","Mean_Other_live_cover","Mean_Dead_Coral_With_Algae_cover",
                       "Mean_Coralline_Algae_cover","Mean_Diseased_Coral_cover","Mean_Sand_Pavement_Rubble_cover",
                       "Mean_Bleaching_frequency","Mean_Disease_frequency")

year <- unique(Cover_site$Year)

for (i in 1:length(year)) {
  df <- Cover_site %>%
    filter(Year==year[[i]])
  Cover_year[i,"Year"] <- year[[i]]
  Cover_year[i,"Mean_Coral_cover"] <- paste0(round(mean(df$CORAL,na.rm = T),3), " ± ", round(sd(df$CORAL,na.rm = T),3))
  Cover_year[i,"Mean_Gorgonians_cover"] <- paste0(round(mean(df$GORGONIANS,na.rm = T),3), " ± ", round(sd(df$GORGONIANS,na.rm = T),3))
  Cover_year[i,"Mean_Sponge_cover"] <- paste0(round(mean(df$SPONGES,na.rm = T),3), " ± ", round(sd(df$SPONGES,na.rm = T),3))
  Cover_year[i,"Mean_Zoanthid_cover"] <- paste0(round(mean(df$ZOANTHIDS,na.rm = T),3), " ± ", round(sd(df$ZOANTHIDS,na.rm = T),3))
  Cover_year[i,"Mean_Macroalge_cover"] <- paste0(round(mean(df$MACROALGAE,na.rm = T),3), " ± ", round(sd(df$MACROALGAE,na.rm = T),3))
  Cover_year[i,"Mean_Other_live_cover"] <- paste0(round(mean(df$`OTHER LIVE`,na.rm = T),3), " ± ", round(sd(df$`OTHER LIVE`,na.rm = T),3))
  Cover_year[i,"Mean_Dead_Coral_With_Algae_cover"] <- paste0(round(mean(df$`DEAD CORAL WITH ALGAE`,na.rm = T),3), " ± ", round(sd(df$`DEAD CORAL WITH ALGAE`,na.rm = T),3))
  Cover_year[i,"Mean_Coralline_Algae_cover"] <- paste0(round(mean(df$`CORALLINE ALGAE`,na.rm = T),3), " ± ", round(sd(df$`CORALLINE ALGAE`,na.rm = T),3))
  Cover_year[i,"Mean_Diseased_Coral_cover"] <- paste0(round(mean(df$`DISEASED CORALS`,na.rm = T),3), " ± ", round(sd(df$`DISEASED CORALS`,na.rm = T),3))
  Cover_year[i,"Mean_Sand_Pavement_Rubble_cover"] <- paste0(round(mean(df$`SAND, PAVEMENT, RUBBLE`,na.rm = T),3), " ± ", round(sd(df$`SAND, PAVEMENT, RUBBLE`,na.rm = T),3))
  Cover_year[i,"Mean_Bleaching_frequency"] <- paste0(round(mean(df$Bleaching_frequency,na.rm = T),3), " ± ", round(sd(df$Bleaching_frequency,na.rm = T),3))
  Cover_year[i,"Mean_Disease_frequency"] <- paste0(round(mean(df$Disease_frequency,na.rm = T),3), " ± ", round(sd(df$Disease_frequency,na.rm = T),3))
}



Cover_year <- as.data.frame(t(Cover_year))
colnames(Cover_year) <- Cover_year[1,]
Cover_year <- Cover_year[-1,]


write.csv(Cover_year, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Cover_year.csv", row.names = T)

