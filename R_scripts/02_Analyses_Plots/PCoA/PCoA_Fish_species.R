library(ggplot2)
library(vegan)
library(ade4)



#-------------------------------------------------------------------------------------------
## Prepare Fish species assemblage
#-------------------------------------------------------------------------------------------
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_species_abund_site.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep=",", na.strings = "NA")


Fish_species_abund_site$Year <- as.factor(Fish_species_abund_site$Year)

Fish_species_abund_site <- Fish_species_abund_site %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))


meta$Protection[is.na(meta$Protection)] <- "Unprotected"


Fish_species_abund_site <- left_join(meta[,c("Site_ID","Protection","Latitude","Longitude")], Fish_species_abund_site)


Fish_species_abund_site <- Fish_species_abund_site %>%
  filter(!is.na(`Abudefduf saxatilis`))

sp_matrix <- Fish_species_abund_site[,c(5:102)]
rownames(sp_matrix) <- Fish_species_abund_site$Site_ID


#------------------------------------------------------------------------------
### PCoA on fish family abundance per site
#------------------------------------------------------------------------------

sp.bray <- vegdist(sp_matrix, method="bray")
pcoa_sp <- dudi.pco(d=sp.bray, scannf = FALSE, nf = 2)
summary(pcoa_sp)

identical(Fish_species_abund_site$Site_ID, rownames(pcoa_sp$li))

Fish_species_abund_site <- cbind(Fish_species_abund_site,pcoa_sp$li) 

A <- ggplot(Fish_species_abund_site, aes(x=A1, y=A2))+
  geom_point(cex = 2, aes(col=Year, shape=Site))+
  geom_encircle(data=Fish_species_abund_site, aes(group=Protection, linetype = Protection, fill= Protection), s_shape = 1, expand = 0,
                alpha = 0.4, show.legend = TRUE)+
  scale_shape_manual(values=c(0,1,2,15,16,17,18))+
  scale_fill_manual(values=c("#A6CEE3","#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#B15928"), aesthetics = "col")+
  xlab("PCoA1")+
  ylab("PCoA2")+
  ggtitle("Species abundance")+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title.y = element_text(size=11),
        axis.title.x = element_text(size=11),
        panel.border = element_rect(fill = NA))





#-------------------------------------------------------------------------------------------
## Prepare Fish species assemblage
#-------------------------------------------------------------------------------------------
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Fish_species_biomass_site.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep=",", na.strings = "NA")


meta$Protection[is.na(meta$Protection)] <- "Unprotected"


Fish_species_biomass_site <- left_join(meta[,c("Site_ID","Protection","Latitude","Longitude","Site","Year")], Fish_species_biomass_site)

Fish_species_biomass_site$Year <- as.factor(Fish_species_biomass_site$Year)

Fish_species_biomass_site <- Fish_species_biomass_site %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))

Fish_species_biomass_site <- Fish_species_biomass_site %>%
  filter(!is.na(`Abudefduf saxatilis`))


sp_matrix <- Fish_species_biomass_site[,c(7:89)]
rownames(sp_matrix) <- Fish_species_biomass_site$Site_ID


#------------------------------------------------------------------------------
### PCoA on fish family biomass per site
#------------------------------------------------------------------------------

sp.bray <- vegdist(sp_matrix, method="bray")
pcoa_sp <- dudi.pco(d=sp.bray, scannf = FALSE, nf = 2)
summary(pcoa_sp)

identical(Fish_species_biomass_site$Site_ID, rownames(pcoa_sp$li))

Fish_species_biomass_site <- cbind(Fish_species_biomass_site,pcoa_sp$li) 

B <- ggplot(Fish_species_biomass_site, aes(x=A1, y=A2))+
  geom_point(cex = 2, aes(col=Year, shape=Site))+
  geom_encircle(data=Fish_species_biomass_site, aes(group=Protection, linetype = Protection, fill= Protection), s_shape = 1, expand = 0,
                alpha = 0.4, show.legend = TRUE)+
  scale_shape_manual(values=c(0,1,2,15,16,17,18))+
  scale_fill_manual(values=c("#A6CEE3","#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#B15928"), aesthetics = "col")+
  xlab("PCoA1")+
  ylab("PCoA2")+
  ggtitle("Species biomass")+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title.y = element_text(size=11),
        axis.title.x = element_text(size=11),
        panel.border = element_rect(fill = NA))

ggarrange(A,B, ncol=2, common.legend = T, legend="right")

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/PCoA_fish_species.png")
