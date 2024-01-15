library(tidyverse)
library(ggplot2)
library(ggpubr)

# Load data
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Site_biodiversity_metrics.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep = ",", na.strings = "NA")

meta$Protection[is.na(meta$Protection)] <- "Unprotected"
Site_biodiv$Year <- as.factor(Site_biodiv$Year)

Site_biodiv <- left_join(meta[,c("Site_ID", "Protection")], Site_biodiv)

# add cyclone information
Site_biodiv <- Site_biodiv %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))

Site_biodiv$cyclone <- as.factor(Site_biodiv$cyclone)
Site_biodiv$Protection <- as.factor(Site_biodiv$Protection)

Site_biodiv <- Site_biodiv %>%
  filter(!is.na(Site))

###########################################################################
#### Longspine Sea Urchin density ####

# test normality of LongspineSeaUrchin_Density
ggqqplot(Site_biodiv$LongspineSeaUrchin_Density)
shapiro.test(Site_biodiv$LongspineSeaUrchin_Density) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_density_year <- kruskal.test(LongspineSeaUrchin_Density~Year, Site_biodiv)

# Kruskal-Wallis mean comparison test (Cyclone)
KW_density_cyclone <- kruskal.test(LongspineSeaUrchin_Density~cyclone, Site_biodiv)


# plot mean Longspine Sea Urchin density per year 
ggplot(Site_biodiv, aes(x=Year, y=LongspineSeaUrchin_Density))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Longspine Sea Urchin density (ind/m²)", title = "Density of Longspine Sea Urchins")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_Longspine_density.png")


# Kruskal-Wallis mean comparison test (Site)
KW_density_site <- kruskal.test(LongspineSeaUrchin_Density~Site, Site_biodiv)


# plot density per year for each site
ggplot(Site_biodiv, aes(x=Year, y=LongspineSeaUrchin_Density, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Longspine Sea Urchin density (ind/m²)", title = "Density of Longspine Sea Urchins per site")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle=90,hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Lineplot_Longspine_density_site.png")


# Kruskal-Wallis mean comparison test (MPA/outside)
KW_density_mpa <- kruskal.test(LongspineSeaUrchin_Density~Protection, Site_biodiv)


# plot mean Longspine Sea Urchin density per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=LongspineSeaUrchin_Density, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=0.42, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Longspine Sea Urchin density (ind/m²)", title = "Density of Longspine Sea Urchins per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_Longspine_density_protection.png")


###########################################################################
#### Other Sea Urchin density ####

# test normality of OtherSeaUrchin_Density
ggqqplot(Site_biodiv$OtherSeaUrchin_Density)
shapiro.test(Site_biodiv$OtherSeaUrchin_Density) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_density_year <- kruskal.test(OtherSeaUrchin_Density~Year, Site_biodiv)

# Kruskal-Wallis mean comparison test (Cyclone)
KW_density_cyclone <- kruskal.test(OtherSeaUrchin_Density~cyclone, Site_biodiv)


# plot mean Other Sea Urchin densityper year 
ggplot(Site_biodiv, aes(x=Year, y=OtherSeaUrchin_Density))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Other Sea Urchin density (ind/m²)", title = "Density of other Sea Urchins")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_OtherUrchins_density.png")


# Kruskal-Wallis mean comparison test (Site)
KW_density_site <- kruskal.test(OtherSeaUrchin_Density~Site, Site_biodiv)


# plot density per year for each site
ggplot(Site_biodiv, aes(x=Year, y=OtherSeaUrchin_Density, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Other Sea Urchin density (ind/m²)", title = "Density of other Sea Urchins per site")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle=90, hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Lineplot_OtherUrchins_density_site.png")


# Kruskal-Wallis mean comparison test (MPA/outside)
KW_density_mpa <- kruskal.test(OtherSeaUrchin_Density~Protection, Site_biodiv)


# plot mean Other Sea Urchin density per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=OtherSeaUrchin_Density, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=0.17, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Other Sea Urchin density (ind/m²)", title = "Density of other Sea Urchins per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_OtherUrchins_density_protection.png")



###########################################################################
#### Sea Cucumbers density ####

# test normality of SeaCucumbers_Density
ggqqplot(Site_biodiv$SeaCucumbers_Density)
shapiro.test(Site_biodiv$SeaCucumbers_Density) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_density_year <- kruskal.test(SeaCucumbers_Density~Year, Site_biodiv)

# Kruskal-Wallis mean comparison test (Cyclone)
KW_density_cyclone <- kruskal.test(SeaCucumbers_Density~cyclone, Site_biodiv)


# plot mean Sea Cucumbers densityper year 
ggplot(Site_biodiv, aes(x=Year, y=SeaCucumbers_Density))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Sea Cucumbers density (ind/m²)", title = "Density of Sea Cucumbers")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_Cucumbers_density.png")



# Kruskal-Wallis mean comparison test (Site)
KW_density_site <- kruskal.test(SeaCucumbers_Density~Site, Site_biodiv)


# plot density per year for each site
ggplot(Site_biodiv, aes(x=Year, y=SeaCucumbers_Density, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Sea Cucumbers density (ind/m²)", title = "Density of Sea Cucumbers per site")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle=90, hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Lineplot_Cucumbers_density_site.png")

# Kruskal-Wallis mean comparison test (MPA/outside)
KW_density_mpa <- kruskal.test(SeaCucumbers_Density~Protection, Site_biodiv)


# plot mean Sea Cucumbers density per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=SeaCucumbers_Density, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Sea Cucumbers density (ind/m²)", title = "Density of Sea Cucumbers per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_Cucumbers_density_protection.png")

###########################################################################
#### Lobster density ####

# test normality of Lobster_Density
ggqqplot(Site_biodiv$Lobster_Density)
shapiro.test(Site_biodiv$Lobster_Density) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_density_year <- kruskal.test(Lobster_Density~Year, Site_biodiv)

# Kruskal-Wallis mean comparison test (Cyclone)
KW_density_cyclone <- kruskal.test(Lobster_Density~cyclone, Site_biodiv)


# plot mean Lobster densityper year 
ggplot(Site_biodiv, aes(x=Year, y=Lobster_Density))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Lobster density (ind/m²)", title = "Density of Lobsters")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_Lobster_density.png")


# Kruskal-Wallis mean comparison test (Site)
KW_density_site <- kruskal.test(Lobster_Density~Site, Site_biodiv)


# plot density per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Lobster_Density, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Lobster density (ind/m²)", title = "Density of Lobsters per site")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90, hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Lineplot_Lobster_density_site.png")

# Kruskal-Wallis mean comparison test (MPA/outside)
KW_density_mpa <- kruskal.test(Lobster_Density~Protection, Site_biodiv)

# plot mean Lobster density per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Lobster_Density, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=0.1, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Lobster density (ind/m²)", title = "Density of Lobsters per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_Lobster_density_protection.png")

###########################################################################
#### Conch density ####

# test normality of Conch_Density
ggqqplot(Site_biodiv$Conch_Density)
shapiro.test(Site_biodiv$Conch_Density) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_density_year <- kruskal.test(Conch_Density~Year, Site_biodiv)

# Kruskal-Wallis mean comparison test (Cyclone)
KW_density_cyclone <- kruskal.test(Conch_Density~cyclone, Site_biodiv)


# plot mean Conch density per year 
ggplot(Site_biodiv, aes(x=Year, y=Conch_Density))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Conch density (ind/m²)", title = "Density of Conchs")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_Conch_density.png")



# Kruskal-Wallis mean comparison test (Site)
KW_density_site <- kruskal.test(Conch_Density~Site, Site_biodiv)

# plot density per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Conch_Density, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Conch density (ind/m²)", title = "Density of Conchs per site")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle=90, hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Lineplot_Conch_density_site.png")


# Kruskal-Wallis mean comparison test (MPA/outside)
KW_density_mpa <- kruskal.test(Conch_Density~Protection, Site_biodiv)


# plot mean Conch density per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Conch_Density, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Conch density (ind/m²)", title = "Density of Conchs per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Macroinvertebrate/Boxplot_Conch_density_protection.png")


