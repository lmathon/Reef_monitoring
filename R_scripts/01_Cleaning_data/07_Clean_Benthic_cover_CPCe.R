library(dplyr)
library(ggplot2)
library(grid)
library(scales)
library(RColorBrewer)
library(plyr)
library(purrr)

# load all csv files in a list
setwd("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/2 - Raw_data/2 - CSV/Benthic_cover/")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv,header = FALSE, sep=";")


# keep only relevant columns and rows
rawd = map(myfiles, ~ .x%>%
             select(1:2)%>%
             filter(row_number() %in% c(6:9,11:21,27:79,81:92,94,96:97,99:113,115:119,121:124,126,128,130:132,134,141:147)))

# join all dataframes from the list
rawd2<-rawd %>%
  reduce(left_join, by = "V1")

rawd2$V1 <- gsub("\\s*\\([^\\)]+\\)","",as.character(rawd2$V1))

# Transpose and clean big dataframe
Benthic_cover_site <- as.data.frame(t(rawd2))
colnames(Benthic_cover_site) <- Benthic_cover_site[1,]
Benthic_cover_site <- Benthic_cover_site[-1,]
rownames(Benthic_cover_site) <- seq(1,nrow(Benthic_cover_site))
Benthic_cover_site[,c(2:ncol(Benthic_cover_site))] <- as.numeric(unlist(Benthic_cover_site[,c(2:ncol(Benthic_cover_site))]))

Benthic_cover_site$`TRANSECT NAME` <- gsub(" FB", "-Fish Bowl", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub("FR", "-Frenchman Reef", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub(" LBR", "-Long Bay Reef", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub(" MM", "-Mikes Mazes", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub(" MSC", "-Moonscape", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub(" PR", "-Proselyte Reef", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub(" SH", "-Shark Hotel", Benthic_cover_site$`TRANSECT NAME`)

Benthic_cover_site$`TRANSECT NAME` <- gsub("2016 ", "2016", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub("POST I 2017", "2017_post", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub("2017_post ", "2017_post", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub("PRE I 2017", "2017_pre", Benthic_cover_site$`TRANSECT NAME`)
Benthic_cover_site$`TRANSECT NAME` <- gsub("2017_pre ", "2017_pre", Benthic_cover_site$`TRANSECT NAME`)

names(Benthic_cover_site)[1] <- "Site_ID"

Benthic_cover_site <- Benthic_cover_site[,-c(3,4)]

colnames(Benthic_cover_site) <- gsub("Millipora","Millepora", colnames(Benthic_cover_site))
colnames(Benthic_cover_site) <- gsub("Diploria strigosa","Pseudodiploria strigosa", colnames(Benthic_cover_site))
colnames(Benthic_cover_site) <- gsub("Palythoa sp.","Palythoa", colnames(Benthic_cover_site))
colnames(Benthic_cover_site) <- gsub("Montastrea faveolata","Orbicella faveolata", colnames(Benthic_cover_site))
colnames(Benthic_cover_site) <- gsub("Montastraea annularis","Orbicella annularis", colnames(Benthic_cover_site))
colnames(Benthic_cover_site) <- gsub("Montastrea franksi","Orbicella franksi", colnames(Benthic_cover_site))
colnames(Benthic_cover_site) <- gsub("Stephanocoenia michelinii","Stephanocoenia intersepta", colnames(Benthic_cover_site))

Benthic_cover_site$MACROALGAE <- Benthic_cover_site$MACROALGAE - Benthic_cover_site$Turf
Benthic_cover_site$TURF <- Benthic_cover_site$Turf

write.csv(Benthic_cover_site, file = "c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Benthic_cover_site.csv", row.names = FALSE)

save(Benthic_cover_site, file="c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Benthic_cover_site_CPCe.rdata")
