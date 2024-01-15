library(ggplot2)
library(vegan)
library(ade4)



#-------------------------------------------------------------------------------------------
## Prepare Fish trophic assemblage
#-------------------------------------------------------------------------------------------
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_trophic_site.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep=",", na.strings = "NA")


meta$Protection[is.na(meta$Protection)] <- "Unprotected"

Fish_trophic_site <- left_join(meta[,c("Site_ID","Protection","Latitude","Longitude","Year","Site")], Fish_trophic_site)

Fish_trophic_site$Year <- as.factor(Fish_trophic_site$Year)

Fish_trophic_site <- Fish_trophic_site %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))

Fish_trophic_site <- Fish_trophic_site %>%
  filter(!is.na(Herbivorous_Density))


troph_matrix <- Fish_trophic_site[,c(11:14)]
rownames(troph_matrix) <- Fish_trophic_site$Site_ID
identical(Fish_trophic_site$Site_ID, rownames(troph_matrix))

#------------------------------------------------------------------------------
### PCoA on fish trophic abundance per site
#------------------------------------------------------------------------------

troph.bray <- vegdist(troph_matrix, method="bray")
pcoa_troph <- dudi.pco(d=troph.bray, scannf = FALSE, nf = 2)

identical(Fish_trophic_site$Site_ID, rownames(pcoa_troph$li))

Fish_trophic_site <- cbind(Fish_trophic_site,pcoa_troph$li) 

A <- ggplot(Fish_trophic_site, aes(x=A1, y=A2))+
  geom_point(cex = 2, aes(col=Year, shape=Site))+
  geom_encircle(data=Fish_trophic_site, aes(group=Protection, linetype = Protection, fill= Protection), s_shape = 1, expand = 0,
                alpha = 0.4, show.legend = TRUE)+
  scale_shape_manual(values=c(0,1,2,15,16,17,18))+
  scale_fill_manual(values=c("#A6CEE3","#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#B15928"), aesthetics = "col")+
  xlab("PCoA1")+
  ylab("PCoA2")+
  ggtitle("Diet groups abundance")+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title.y = element_text(size=11),
        axis.title.x = element_text(size=11),
        panel.border = element_rect(fill = NA))




#-------------------------------------------------------------------------------------------
## Prepare Fish trophic assemblage
#-------------------------------------------------------------------------------------------
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_trophic_site.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep=",", na.strings = "NA")


meta$Protection[is.na(meta$Protection)] <- "Unprotected"

Fish_trophic_site <- left_join(meta[,c("Site_ID","Protection","Latitude","Longitude","Year","Site")], Fish_trophic_site)

Fish_trophic_site$Year <- as.factor(Fish_trophic_site$Year)

Fish_trophic_site <- Fish_trophic_site %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))

Fish_trophic_site <- Fish_trophic_site %>%
  filter(!is.na(Herbivorous_Density))


troph_matrix <- Fish_trophic_site[,c(15:18)]
rownames(troph_matrix) <- Fish_trophic_site$Site_ID
identical(Fish_trophic_site$Site_ID, rownames(troph_matrix))

#------------------------------------------------------------------------------
### PCoA on fish trophic abundance per site
#------------------------------------------------------------------------------

troph.bray <- vegdist(troph_matrix, method="bray")
pcoa_troph <- dudi.pco(d=troph.bray, scannf = FALSE, nf = 2)

identical(Fish_trophic_site$Site_ID, rownames(pcoa_troph$li))

Fish_trophic_site <- cbind(Fish_trophic_site,pcoa_troph$li) 

B <- ggplot(Fish_trophic_site, aes(x=A1, y=A2))+
  geom_point(cex = 2, aes(col=Year, shape=Site))+
  geom_encircle(data=Fish_trophic_site, aes(group=Protection, linetype = Protection, fill= Protection), s_shape = 1, expand = 0,
                alpha = 0.4, show.legend = TRUE)+
  scale_shape_manual(values=c(0,1,2,15,16,17,18))+
  scale_fill_manual(values=c("#A6CEE3","#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#B15928"), aesthetics = "col")+
  xlab("PCoA1")+
  ylab("PCoA2")+
  ggtitle("Diet groups biomass")+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title.y = element_text(size=11),
        axis.title.x = element_text(size=11),
        panel.border = element_rect(fill = NA))

ggarrange(A,B, ncol=2, common.legend = T, legend="right")

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/PCoA_fish_trophic.png")
