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
#### Macroalgae Height ####

# test normality of MacroAlgae_MeanHeight
ggqqplot(Site_biodiv$MacroAlgae_MeanHeight)
shapiro.test(Site_biodiv$MacroAlgae_MeanHeight) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_height_year <- kruskal.test(MacroAlgae_MeanHeight~Year, Site_biodiv)

# Kruskal-Wallis mean comparison test (Cyclone)
KW_height_cyclone <- kruskal.test(MacroAlgae_MeanHeight~cyclone, Site_biodiv)


# plot mean Macroalgae Height per year 
ggplot(Site_biodiv, aes(x=Year, y=MacroAlgae_MeanHeight))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Macroalgae Height (cm)", title = "Evolution of Macroalgae height between 2016 and 2023")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Boxplot_Macroalgae_height.png")

# Kruskal-Wallis mean comparison test (Site)
KW_height_site <- kruskal.test(MacroAlgae_MeanHeight~Site, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$MacroAlgae_MeanHeight, Site_biodiv$Site,
                     p.adjust.method = "BH")


# plot height per year for each site
ggplot(Site_biodiv, aes(x=Year, y=MacroAlgae_MeanHeight, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Macroalgae Height (cm)", title = "Macroalgae height per site between 2016 and 2023")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Lineplot_Macroalgae_height_sites.png")

# Kruskal-Wallis mean comparison test (MPA/outside)
KW_height_mpa <- kruskal.test(MacroAlgae_MeanHeight~Protection, Site_biodiv)

# Compute mean and sd Fish abundance per year and protection status
mean_height_year_mpa <- Site_biodiv %>%
  select(Year, Protection, MacroAlgae_MeanHeight)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_height_year_mpa <- Site_biodiv %>%
  select(Year, Protection, MacroAlgae_MeanHeight)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_height_year_mpa) <- c("Year", "Protection", "SD_height")

height_year_mpa <- cbind(mean_height_year_mpa[,c("Year", "MacroAlgae_MeanHeight")], sd_height_year_mpa[,c("SD_height","Protection")])

# plot mean Macroalgae Height per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=MacroAlgae_MeanHeight, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=4, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Macroalgae Height (cm)", title = "Macroalgae height per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Boxplot_Macroalgae_height_protection.png")

ggplot(height_year_mpa, aes(x=Year, y=MacroAlgae_MeanHeight, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=MacroAlgae_MeanHeight-SD_height, ymax=MacroAlgae_MeanHeight+SD_height, color=Protection), width=.2)+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Macroalgae Mean Height (cm)", title = "Macroalgae height per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Lineplot_Macroalgae_height_protection.png")

###########################################################################
#### Turf Height ####

# test normality of Turf_MeanHeight
ggqqplot(Site_biodiv$Turf_MeanHeight)
shapiro.test(Site_biodiv$Turf_MeanHeight) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_height_year <- kruskal.test(Turf_MeanHeight~Year, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$Turf_MeanHeight, Site_biodiv$Year,
                     p.adjust.method = "BH")

# Kruskal-Wallis mean comparison test (Cyclone)
KW_height_cyclone <- kruskal.test(Turf_MeanHeight~cyclone, Site_biodiv)


# plot mean Turf Height per year 
ggplot(Site_biodiv, aes(x=Year, y=Turf_MeanHeight))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=1, y=3, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  annotate(geom="text", x=2.5, y=10, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Turf Height (mm)", title = "Evolution of Turf height between 2016 and 2023")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Boxplot_Turf_height.png")


# Kruskal-Wallis mean comparison test (Site)
KW_height_site <- kruskal.test(Turf_MeanHeight~Site, Site_biodiv)


# plot height per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Turf_MeanHeight, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Turf Height (mm)", title = "Turf height per site between 2016 and 2023")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Lineplot_Turf_height_sites.png")

# Kruskal-Wallis mean comparison test (MPA/outside)
KW_height_mpa <- kruskal.test(Turf_MeanHeight~Protection, Site_biodiv)

# Compute mean and sd Fish abundance per year and protection status
mean_height_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Turf_MeanHeight)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_height_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Turf_MeanHeight)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_height_year_mpa) <- c("Year", "Protection", "SD_height")

height_year_mpa <- cbind(mean_height_year_mpa[,c("Year", "Turf_MeanHeight")], sd_height_year_mpa[,c("SD_height","Protection")])

# plot mean Turf Height per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Turf_MeanHeight, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=10, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Turf Height (mm)", title = "Turf height per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Boxplot_Turf_height_protection.png")

ggplot(height_year_mpa, aes(x=Year, y=Turf_MeanHeight, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=Turf_MeanHeight-SD_height, ymax=Turf_MeanHeight+SD_height, color=Protection), width=.2)+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Turf Mean Height (cm)", title = "Turf height per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Algae/Lineplot_Turf_height_protection.png")
