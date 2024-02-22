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
#### Coral recruits density ####

# test normality of Recruits_density
ggqqplot(Site_biodiv$Recruits_density)
shapiro.test(Site_biodiv$Recruits_density) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_density_year <- kruskal.test(Recruits_density~Year, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$Recruits_density, Site_biodiv$Year,
                     p.adjust.method = "BH")

# Kruskal-Wallis mean comparison test (Cyclone)
KW_density_cyclone <- kruskal.test(Recruits_density~cyclone, Site_biodiv)


# plot mean Coral recruits density per year 
ggplot(Site_biodiv, aes(x=Year, y=Recruits_density))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=9, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Coral recruits density (ind/m²)", title = "Density of coral recruits")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Boxplot_Recruit_density.png")


# Kruskal-Wallis mean comparison test (Site)
KW_density_site <- kruskal.test(Recruits_density~Site, Site_biodiv)


# plot density per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Recruits_density, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral recruits density (ind/m²)", title = "Density of coral recruits per site")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle=90,hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Lineplot_Recruit_density_sites.png")


# Kruskal-Wallis mean comparison test (MPA/outside)
KW_density_mpa <- kruskal.test(Recruits_density~Protection, Site_biodiv)

# Compute mean and sd Fish abundance per year and protection status
mean_density_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Recruits_density)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_density_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Recruits_density)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_density_year_mpa) <- c("Year", "Protection", "SD_density")

density_year_mpa <- cbind(mean_density_year_mpa[,c("Year", "Recruits_density")], sd_density_year_mpa[,c("SD_density","Protection")])

# plot mean Coral recruits density per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Recruits_density, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=29, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Coral recruits density (ind/m²)", title = "Density of coral recruits per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Boxplot_Recruit_density_protection.png")



ggplot(density_year_mpa, aes(x=Year, y=Recruits_density, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=Recruits_density-SD_density, ymax=Recruits_density+SD_density, color=Protection), width=.2)+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Coral recruits density (ind/m²)", title = "Density of coral recruits per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Lineplot_Recruit_density_protection.png")


###########################################################################
#### Coral recruits size ####

# test normality of Recruits_MeanSize
ggqqplot(Site_biodiv$Recruits_MeanSize)
shapiro.test(Site_biodiv$Recruits_MeanSize) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_size_year <- kruskal.test(Recruits_MeanSize~Year, Site_biodiv)

# Kruskal-Wallis mean comparison test (Cyclone)
KW_size_cyclone <- kruskal.test(Recruits_MeanSize~cyclone, Site_biodiv)


# plot mean Coral recruits size per year 
ggplot(Site_biodiv, aes(x=Year, y=Recruits_MeanSize))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral recruits size (cm)", title = "Size of Coral recruits")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Boxplot_Recruit_size.png")


# Kruskal-Wallis mean comparison test (Site)
KW_density_site <- kruskal.test(Recruits_MeanSize~Site, Site_biodiv)


# plot size per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Recruits_MeanSize, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral recruits size (cm)", title = "Size of Coral recruits per site")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle=90,hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Lineplot_Recruit_size_sites.png")


# Kruskal-Wallis mean comparison test (MPA/outside)
KW_size_mpa <- kruskal.test(Recruits_MeanSize~Protection, Site_biodiv)

# Compute mean and sd Fish abundance per year and protection status
mean_size_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Recruits_MeanSize)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_size_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Recruits_MeanSize)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_size_year_mpa) <- c("Year", "Protection", "SD_size")

size_year_mpa <- cbind(mean_size_year_mpa[,c("Year", "Recruits_MeanSize")], sd_size_year_mpa[,c("SD_size","Protection")])

# plot mean Coral recruits size per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Recruits_MeanSize, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral recruits size (cm)", title = "Size of Coral recruits per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Boxplot_Recruit_size_protection.png")



ggplot(size_year_mpa, aes(x=Year, y=Recruits_MeanSize, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=Recruits_MeanSize-SD_size, ymax=Recruits_MeanSize+SD_size, color=Protection), width=.2)+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral recruits size (cm)", title = "Size of Coral recruits per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Lineplot_Recruit_size_protection.png")




###########################################################################
#### Recruit genus proportions ####

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Recruit_abund.rdata")

Recruits_abund_site <- left_join(Recruits_abund_site, Site_biodiv[,c("Year","Site_ID", "Site")])
Recruits_abund_site <- Recruits_abund_site%>%
  filter(!is.na(Site))

prop <- Recruits_abund_site %>%
  group_by(Site_ID)%>%
  mutate(perc = Number/sum(Number))

# plot mean Fish family proportion per year
ggplot(Recruits_abund_site, aes(x=Year,fill=GenusSN))+
  geom_bar(position="fill")+
  labs(x="", y="Recruit proportion", title="Coral recruit genus proportion per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Recruits/Recruit_proportion_sites.png")
