library(ggplot2)
library(vegan)
library(ade4)
library(ggalt)

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
rownames(cover_matrix) <- Benthic_cover_site_all$Site_ID
identical(Benthic_cover_site_all$Site_ID, rownames(cover_matrix))


#------------------------------------------------------------------------------
### PCoA on benthic cover per site
#------------------------------------------------------------------------------

cover.bray <- vegdist(cover_matrix, method="bray")
pcoa_cover <- dudi.pco(cover.bray)
summary(pcoa_cover)

identical(Benthic_cover_site_all$Site_ID, rownames(pcoa_cover$li))

Benthic_cover_site_all <- cbind(Benthic_cover_site_all,pcoa_cover$li) 

ggplot(Benthic_cover_site_all, aes(x=A1, y=A2))+
  geom_encircle(data=Benthic_cover_site_all, aes(group=Protection, linetype = Protection, fill= Protection), s_shape = 1, expand = 0,
                alpha = 0.4, show.legend = TRUE)+
  geom_point(cex = 2, aes(col=Year, shape=Site))+
  scale_shape_manual(values=c(0,1,2,15,16,17,18))+
  scale_fill_manual(values=c("#A6CEE3","#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#B15928"), aesthetics = "col")+
  xlab("PCoA1 (36.7 %)")+
  ylab("PCoA2 (25.9 %)")+
  ggtitle("Benthic community composition")+
  theme(legend.position = "right",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.title.y = element_text(size=11),
        axis.title.x = element_text(size=11),
        panel.border = element_rect(fill = NA))

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/PCoA_benthic_cover.png")

