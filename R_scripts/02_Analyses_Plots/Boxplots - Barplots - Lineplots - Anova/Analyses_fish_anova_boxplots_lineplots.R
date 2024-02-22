library(tidyverse)
library(ggplot2)
library(ggpubr)

# Load fish data
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Site_biodiversity_metrics.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep = ",", na.strings = "NA")

meta$Protection[is.na(meta$Protection)] <- "Unprotected"
Site_biodiv$Year <- as.factor(Site_biodiv$Year)

Site_biodiv <- left_join(meta[,c("Site_ID", "Protection", "Fish_observer")], Site_biodiv)

Site_biodiv <- Site_biodiv %>%
  filter(!is.na(Site))

# add cyclone information
Site_biodiv <- Site_biodiv %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))

###########################################################################
#### Fish abundance ####

# test normality of Fish_abundance
ggqqplot(Site_biodiv$Fish_abundance)
shapiro.test(Site_biodiv$Fish_abundance) # -> not normal

# Kruskal-wallis mean comparison test (Observer)
KW_abundance_obs <- kruskal.test(Fish_abundance~Fish_observer, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$Fish_abundance, Site_biodiv$Fish_observer,
                     p.adjust.method = "BH")


# Kruskal-wallis mean comparison test (Year)
KW_abundance_year <- kruskal.test(Fish_abundance~Year, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$Fish_abundance, Site_biodiv$Year,
                     p.adjust.method = "BH")

# Kruskal-wallis mean comparison test (Cyclone)
KW_abundance_cyclone <- kruskal.test(Fish_abundance~cyclone, Site_biodiv)

# plot mean Fish abundance per year 
ggplot(Site_biodiv, aes(x=Year, y=Fish_abundance))+
  geom_boxplot(fill="lightgrey")+
  annotate(geom="text", x=1, y=850, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  annotate(geom="text", x=2.5, y=1600, label="*", hjust=0.5, size=6, color="red", fontface = "bold")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish abundance", title="Abundance of all fish")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_fish_abundance.png")


# Kruskal-wallis mean comparison test (Site)
KW_abundance_site <- kruskal.test(Fish_abundance~Site, Site_biodiv)

# plot abundance per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Fish_abundance, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish abundance", title="Abundance of all fish per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_fish_abundance_site.png")

# Kruskal-wallis mean comparison test (MPA/outside)
KW_abundance_mpa <- kruskal.test(Fish_abundance~Protection, Site_biodiv)


# Compute mean and sd Fish abundance per year and protection status
mean_abundance_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Fish_abundance)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_abundance_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Fish_abundance)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_abundance_year_mpa) <- c("Year", "Protection", "SD_abundance")

abundance_year_mpa <- cbind(mean_abundance_year_mpa[,c("Year", "Fish_abundance")], sd_abundance_year_mpa[,c("SD_abundance","Protection")])


# plot mean Fish abundance per year and protection status

ggplot(Site_biodiv, aes(x=Year, y=Fish_abundance, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish abundance", title="Abundance of all fish per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_fish_abundance_protection.png")


ggplot(abundance_year_mpa, aes(x=Year, y=Fish_abundance, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=Fish_abundance-SD_abundance, ymax=Fish_abundance+SD_abundance, color=Protection), width=.2)+
  labs(x="", y="Mean Fish abundance", title="Abundance of all fish per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_fish_abundance_protection.png")



###########################################################################
#### Fish richness ####

# test normality of Fish_richness
ggqqplot(Site_biodiv$Fish_richness)
shapiro.test(Site_biodiv$Fish_richness) # -> Normal


# anova mean comparison test (Observer)
aov_richness_obs <- aov(Fish_richness~Fish_observer, Site_biodiv)
summary(aov_richness_obs)

# anova mean comparison test (Year)
aov_richness_year <- aov(Fish_richness~Year, Site_biodiv)
summary(aov_richness_year)

# anova mean comparison test (Cyclone)
aov_richness_cyclone <- aov(Fish_richness~cyclone, Site_biodiv)
summary(aov_richness_cyclone)

# plot mean Fish richness per year 
ggplot(Site_biodiv, aes(x=Year, y=Fish_richness))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish richness", title="Richness of all fish")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_fish_richness.png")


# anova mean comparison test (Site)
aov_richness_site <- aov(Fish_richness~Site, Site_biodiv)
summary(aov_richness_site)

# plot richness per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Fish_richness, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish richness", title="Richness of all fish per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_fish_richness_site.png")


# anova mean comparison test (MPA/outside)
aov_richness_mpa <- aov(Fish_richness~Protection, Site_biodiv)
summary(aov_richness_mpa)

# plot mean Fish richness per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Fish_richness, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish richness", title="Richness of all fish per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_fish_richness_protection.png")


# Compute mean and sd Fish richness per year and protection status
mean_richness_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Fish_richness)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_richness_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Fish_richness)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_richness_year_mpa) <- c("Year", "Protection", "SD_richness")

richness_year_mpa <- cbind(mean_richness_year_mpa[,c("Year", "Fish_richness")], sd_richness_year_mpa[,c("SD_richness","Protection")])

# plot mean Fish richness per year and protection status

ggplot(richness_year_mpa, aes(x=Year, y=Fish_richness, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=Fish_richness-SD_richness, ymax=Fish_richness+SD_richness,color=Protection), width=.2)+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Fish species richness", title="Richness of all fish per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_fish_richness_protection.png")



###########################################################################
#### Fish density ####

# test normality of Fish_density
ggqqplot(Site_biodiv$Fish_density)
shapiro.test(Site_biodiv$Fish_density) # -> not normal

# Kruskal-Wallis mean comparison test (obs)
KW_density_obs <- kruskal.test(Fish_density~Fish_observer, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$Fish_density, Site_biodiv$Fish_observer,
                     p.adjust.method = "BH")

# Kruskal-Wallis mean comparison test (Year)
KW_density_year <- kruskal.test(Fish_density~Year, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$Fish_density, Site_biodiv$Year,
                     p.adjust.method = "BH")

# Kruskal-Wallis mean comparison test (Cyclone)
KW_density_cyclone <- kruskal.test(Fish_density~cyclone, Site_biodiv)


# plot mean Fish density per year 
ggplot(Site_biodiv, aes(x=Year, y=Fish_density))+
  geom_boxplot(fill="lightgrey")+
  annotate(geom="text", x=1, y=2.9, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  annotate(geom="text", x=2.5, y=5.5, label="*", hjust=0.5, size=6, color="red", fontface = "bold")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish density (ind/m2)", title="Density of all fish")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_fish_density.png")


# Kruskal-Wallis mean comparison test (Site)
KW_density_site <- kruskal.test(Fish_density~Site, Site_biodiv)


# plot density per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Fish_density, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish density (ind/m2)", title="Density of all fish per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_fish_density_site.png")


# Kruskal-Wallis mean comparison test (MPA/outside)
KW_density_mpa <- kruskal.test(Fish_density~Protection, Site_biodiv)

# anova Fish density between protection zones
res.aov <- aov(Fish_density~Protection, density_year_mpa)
summary(res.aov)

# Compute mean and sd Fish density per year and protection status
mean_density_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Fish_density)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_density_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Fish_density)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_density_year_mpa) <- c("Year", "Protection", "SD_density")

density_year_mpa <- cbind(mean_density_year_mpa[,c("Year", "Fish_density")], sd_density_year_mpa[,c("SD_density","Protection")])

# plot mean Fish density per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Fish_density, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish density (ind/m2)", title="Density of all fish per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_fish_density_protection.png")



ggplot(density_year_mpa, aes(x=Year, y=Fish_density, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=Fish_density-SD_density, ymax=Fish_density+SD_density, color=Protection), width=.2)+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Fish density (ind/m2)", title="Density of all fish per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Barplot_fish_density.png")



###########################################################################
#### Fish biomass ####

# test normality of Fish_biomass
ggqqplot(Site_biodiv$Fish_biomass)
shapiro.test(Site_biodiv$Fish_biomass) # -> not normal

# Kruskal-Wallis mean comparison test (obs)
KW_biomass_obs <- kruskal.test(Fish_biomass~Fish_observer, Site_biodiv)

# Kruskal-Wallis mean comparison test (Year)
KW_biomass_year <- kruskal.test(Fish_biomass~Year, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$Fish_biomass, Site_biodiv$Year,
                     p.adjust.method = "BH")

# Kruskal-Wallis mean comparison test (Cyclone)
KW_biomass_cyclone <- kruskal.test(Fish_biomass~cyclone, Site_biodiv)
pairwise.wilcox.test(Site_biodiv$Fish_biomass, Site_biodiv$cyclone,
                     p.adjust.method = "BH")


# plot mean Fish biomass per year 
ggplot(Site_biodiv, aes(x=Year, y=Fish_biomass))+
  geom_boxplot(fill="lightgrey")+
  annotate(geom="text", x=5, y=200, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  annotate(geom="text", x=2.5, y=300, label="**", hjust=0.5, size=6, color="red", fontface = "bold")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish biomass (Kg)", title="Biomass of all fish")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_fish_biomass.png")


# Kruskal-Wallis mean comparison test (Site)
KW_biomass_site <- kruskal.test(Fish_biomass~Site, Site_biodiv)


# plot biomass per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Fish_biomass, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish biomass (Kg)", title="Biomass of all fish per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_fish_biomass_site.png")



# Kruskal-Wallis mean comparison test (MPA/outside)
KW_biomass_mpa <- kruskal.test(Fish_biomass~Protection, Site_biodiv)

# anova Fish biomass between protection zones
res.aov <- aov(Fish_biomass~Protection, biomass_year_mpa)
summary(res.aov)

# Compute mean and sd Fish biomass per year and protection status
mean_biomass_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Fish_biomass)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_biomass_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Fish_biomass)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_biomass_year_mpa) <- c("Year", "Protection", "SD_biomass")

biomass_year_mpa <- cbind(mean_biomass_year_mpa[,c("Year", "Fish_biomass")], sd_biomass_year_mpa[,c("SD_biomass","Protection")])

# plot mean Fish biomass per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Fish_biomass, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish biomass (Kg)", title="Biomass of all fish per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_fish_biomass_protection.png")



ggplot(biomass_year_mpa, aes(x=Year, y=Fish_biomass, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=Fish_biomass-SD_biomass, ymax=Fish_biomass+SD_biomass, color=Protection), width=.2)+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Fish biomass (Kg)", title="Biomass of all fish per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_fish_biomass_protection.png")



###########################################################################
#### Family richness ####

# test normality of Family_richness
ggqqplot(Site_biodiv$Family_richness)
shapiro.test(Site_biodiv$Family_richness) # -> normal

# anova mean comparison test (obs)
aov_fam_richness_obs <- aov(Family_richness~Fish_observer, Site_biodiv)
summary(aov_fam_richness_obs)

# anova mean comparison test (Year)
aov_fam_richness_year <- aov(Family_richness~Year, Site_biodiv)
summary(aov_fam_richness_year)

# anova mean comparison test (Cyclone)
aov_fam_richness_cyclone <- aov(Family_richness~cyclone, Site_biodiv)
summary(aov_fam_richness_cyclone)

# plot mean Fish fam_richness per year 
ggplot(Site_biodiv, aes(x=Year, y=Family_richness))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish family richness", title="Richness of fish families")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_family_richness.png")



# anova mean comparison test (Site)
aov_fam_richness_site <- aov(Family_richness~Site, Site_biodiv)
summary(aov_fam_richness_site)

# plot fam_richness per year for each site
ggplot(Site_biodiv, aes(x=Year, y=Family_richness, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish family richness", title="Richness of fish families per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_family_richness_site.png")


# anova mean comparison test (MPA/outside)
aov_fam_richness_mpa <- aov(Family_richness~Protection, Site_biodiv)
summary(aov_fam_richness_mpa)

# Compute mean and sd Fish family richness per year and protection status
mean_fam_richness_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Family_richness)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_fam_richness_year_mpa <- Site_biodiv %>%
  select(Year, Protection, Family_richness)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_fam_richness_year_mpa) <- c("Year", "Protection", "SD_richness")

fam_richness_year_mpa <- cbind(mean_fam_richness_year_mpa[,c("Year", "Family_richness")], sd_fam_richness_year_mpa[,c("SD_richness","Protection")])

# plot mean Fish family richness per year and protection status
ggplot(Site_biodiv, aes(x=Year, y=Family_richness, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Fish family richness", title="Richness of fish families per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Boxplot_family_richness_protection.png")


ggplot(fam_richness_year_mpa, aes(x=Year, y=Family_richness, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_errorbar(aes(ymin=Family_richness-SD_richness, ymax=Family_richness+SD_richness,color=Protection), width=.2)+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Fish family richness", title="Richness of fish families per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Total/Lineplot_family_richness_protection.png")



###########################################################################
#### Family proportions ####

families <- c("Acanthuridae", "Labridae", "Lutjanidae", "Serranidae", "Epinephelidae", "Carcharhinidae", "Dasyatidae", "Scaridae")

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Family_abundance.rdata")

Fish_family_abund_site <- left_join(Fish_family_abund_site, Site_biodiv[,c("Year","Site_ID", "Site")])
Fish_family_abund_site <- Fish_family_abund_site%>%
  filter(Family%in%families)%>%
  filter(!is.na(Site))

prop <- Fish_family_abund_site %>%
  group_by(Site_ID)%>%
  mutate(perc = Number/sum(Number))

# plot mean Fish family proportion per year
plot <- ggplot(Fish_family_abund_site, aes(x=Year,fill=Family,y=Number))+
  geom_bar(position="fill",stat="identity")+
  labs(x="", y="Family proportion", title="Proportion of each family per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Plot_family_proportion_site.png")




###########################################################################
#### Important families abundance ####

families <- c("Acanthuridae", "Labridae", "Lutjanidae", "Serranidae", "Epinephelidae", "Carcharhinidae", "Dasyatidae", "Scaridae")

KW_imp_fam_abundance_obs <- vector("list")
KW_imp_fam_abundance_year <- vector("list")
KW_imp_fam_abundance_cyclone <- vector("list")
plot_fam_abundance_year <- vector("list")
KW_imp_fam_abundance_site <- vector("list")
plot_fam_abundance_year_site <- vector("list")
KW_imp_fam_abundance_mpa <- vector("list")
plot_fam_abundance_year_mpa <- vector("list")
plot_fam_abundance_year_mpa3 <- vector("list")
imp_fam_abundance_year_mpa <- vector("list")

for (i in 1:length(families)) {
  # create table with one family column
  fam_df <- Site_biodiv%>%select("Year",paste0(families[[i]],"_Abundance"),"Site", "Protection", "cyclone")%>% as.data.frame()
  fam_df2 <- data.frame(Year=fam_df$Year, Mean_abundance=fam_df[,2], Protection=fam_df$Protection, Site=fam_df$Site, cyclone=fam_df$cyclone)
  
  # Kruskal-Wallis mean comparison (obs)
  #KW_imp_fam_abundance_obs[[i]] <- kruskal.test(Mean_abundance~Fish_observer, fam_df2)
  
  # Kruskal-Wallis mean comparison (Year)
  KW_imp_fam_abundance_year[[i]] <- kruskal.test(Mean_abundance~Year, fam_df2)
  
  # Kruskal-Wallis mean comparison (Cyclone)
  KW_imp_fam_abundance_cyclone[[i]] <- kruskal.test(Mean_abundance~cyclone, fam_df2)
  
  # plot mean abundance of important families per year 
  plot_fam_abundance_year[[i]] <- ggplot(fam_df2, aes(x=Year, y=Mean_abundance))+
    geom_boxplot(fill="lightgrey")+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Abundance", title=paste0("Abundance of ", families[[i]]))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  
    # Kruskal-Wallis mean comparison (Site)
  KW_imp_fam_abundance_site[[i]] <- kruskal.test(Mean_abundance~Site, fam_df2)
  
  # plot mean abundance of important families per year for each site
  plot_fam_abundance_year_site[[i]] <- ggplot(data=fam_df2, aes(x=Year, y=Mean_abundance, group=1))+
    geom_line()+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Abundance", title=paste0("Abundance of ", families[[i]]," per site"))+
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

  # Kruskal-Wallis mean comparison (MPA)
  KW_imp_fam_abundance_mpa[[i]] <- kruskal.test(Mean_abundance~Protection, fam_df2)
  
  # Compute mean and sd important fish families abundance per year and protection status
  mean_imp_fam_abundance_year_mpa <- Site_biodiv %>%
    select(Year, Protection, paste0(families[[i]],"_Abundance"))%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(mean))
  names(mean_imp_fam_abundance_year_mpa) <- c("Year", "Protection", "Mean_Abundance")
  
  sd_imp_fam_abundance_year_mpa <- Site_biodiv %>%
    select(Year, Protection, paste0(families[[i]],"_Abundance"))%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(sd))
  names(sd_imp_fam_abundance_year_mpa) <- c("Year", "Protection", "SD_Abundance")
  
  imp_fam_abundance_year_mpa[[i]] <- cbind(mean_imp_fam_abundance_year_mpa[,c("Year", "Mean_Abundance")], sd_imp_fam_abundance_year_mpa[,c("SD_Abundance","Protection")])
  
  
  # plot mean important Fish family abundance per year and protection status
  plot_fam_abundance_year_mpa3[[i]] <- ggplot(imp_fam_abundance_year_mpa[[i]], aes(x=Year, y=Mean_Abundance, group=Protection))+
    geom_line(aes(color=Protection))+
    geom_point(aes(color=Protection))+
    geom_errorbar(aes(ymin=Mean_Abundance-SD_Abundance, ymax=Mean_Abundance+SD_Abundance, color=Protection), width=.2)+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Mean abundance", title=paste0("Abundance of ", families[[i]], " per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  plot_fam_abundance_year_mpa[[i]] <- ggplot(fam_df2, aes(x=Year, y=Mean_abundance, fill=Protection))+
    geom_boxplot()+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Abundance", title=paste0("Abundance of ", families[[i]], " per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
}


# add significance on graphs
pairwise.wilcox.test(Site_biodiv$Lutjanidae_Abundance, Site_biodiv$Year,
                     p.adjust.method = "BH")

pairwise.wilcox.test(Site_biodiv$Scaridae_Abundance, Site_biodiv$Year,
                     p.adjust.method = "BH")


plot_fam_abundance_year[[1]] <- plot_fam_abundance_year[[1]] + annotate(geom="text", x=1, y=90, label="*", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_abundance_year[[2]] <- plot_fam_abundance_year[[2]] + annotate(geom="text", x=1, y=500, label="**", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_abundance_year[[3]] <- plot_fam_abundance_year[[3]] + annotate(geom="text", x=1, y=10, label="**", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_abundance_year[[8]] <- plot_fam_abundance_year[[8]] + annotate(geom="text", x=5, y=42, label="**", hjust=0.5, size=6, color="red", fontface = "bold")

plot_fam_abundance_year[[2]] <- plot_fam_abundance_year[[2]] + annotate(geom="text", x=2.5, y=1000, label="*", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_abundance_year[[3]] <- plot_fam_abundance_year[[3]] + annotate(geom="text", x=2.5, y=130, label="*", hjust=0.5, size=6, color="red", fontface = "bold")

plot_fam_abundance_year_mpa[[6]] <- plot_fam_abundance_year_mpa[[6]] + annotate(geom="text", x=9, y=2.9, label="**", hjust=0.5, size=7, color="red", fontface = "bold")
plot_fam_abundance_year_mpa[[7]] <- plot_fam_abundance_year_mpa[[7]] + annotate(geom="text", x=9, y=1.9, label="*", hjust=0.5, size=7, color="red", fontface = "bold")
plot_fam_abundance_year_mpa[[8]] <- plot_fam_abundance_year_mpa[[8]] + annotate(geom="text", x=9, y=70, label="*", hjust=0.5, size=7, color="red", fontface = "bold")



for (i in 1:length(families)) {
  ggsave(plot_fam_abundance_year[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Boxplot_",families[[i]],"_abundance.png"))
  ggsave(plot_fam_abundance_year_site[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Lineplot_",families[[i]],"_abundance_site.png"))
  ggsave(plot_fam_abundance_year_mpa[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Boxplot_",families[[i]],"_abundance_protection.png"))
  ggsave(plot_fam_abundance_year_mpa3[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Lineplot_",families[[i]],"_abundance_protection.png"))
  
}

###########################################################################
#### Important families density ####

families <- c("Acanthuridae", "Labridae", "Lutjanidae", "Serranidae", "Epinephelidae", "Carcharhinidae", "Dasyatidae", "Scaridae")

KW_imp_fam_density_obs <- vector("list")
KW_imp_fam_density_year <- vector("list")
KW_imp_fam_density_cyclone <- vector("list")
plot_fam_density_year <- vector("list")
KW_imp_fam_density_site <- vector("list")
plot_fam_density_year_site <- vector("list")
KW_imp_fam_density_mpa <- vector("list")
plot_fam_density_year_mpa <- vector("list")
plot_fam_density_year_mpa3 <- vector("list")
imp_fam_density_year_mpa <- vector("list")

for (i in 1:length(families)) {
  # create table with one family column
  fam_df <- Site_biodiv%>%select("Year",paste0(families[[i]],"_Density"),"Site", "Protection", "cyclone")%>% as.data.frame()
  fam_df2 <- data.frame(Year=fam_df$Year, Mean_density=fam_df[,2], Protection=fam_df$Protection, Site=fam_df$Site, cyclone=fam_df$cyclone)
  
  # Kruskal-Wallis mean comparison (obs)
  #KW_imp_fam_density_obs[[i]] <- kruskal.test(Mean_density~Fish_observer, fam_df2)
  
  # Kruskal-Wallis mean comparison (Year)
  KW_imp_fam_density_year[[i]] <- kruskal.test(Mean_density~Year, fam_df2)
  
  # Kruskal-Wallis mean comparison (cyclone)
  KW_imp_fam_density_cyclone[[i]] <- kruskal.test(Mean_density~cyclone, fam_df2)
  
  # plot mean density of important families per year 
  plot_fam_density_year[[i]] <- ggplot(fam_df2, aes(x=Year, y=Mean_density))+
    geom_boxplot(fill="lightgrey")+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Density (ind/m2)", title=paste0("Density of ", families[[i]]))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  
  # Kruskal-Wallis mean comparison (Site)
  KW_imp_fam_density_site[[i]] <- kruskal.test(Mean_density~Site, fam_df2)
  
  # plot mean density of important families per year for each site
  plot_fam_density_year_site[[i]] <- ggplot(data=fam_df2, aes(x=Year, y=Mean_density, group=1))+
    geom_line()+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Density (ind/m2)", title=paste0("Density of ", families[[i]], " per site"))+
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

  # Kruskal-Wallis mean comparison (MPA)
  KW_imp_fam_density_mpa[[i]] <- kruskal.test(Mean_density~Protection, fam_df2)
  
  # Compute mean and sd important fish families density per year and protection status
  mean_imp_fam_density_year_mpa <- Site_biodiv %>%
    select(Year, Protection, paste0(families[[i]],"_Density"))%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(mean))
  names(mean_imp_fam_density_year_mpa) <- c("Year", "Protection", "Mean_density")
  
  sd_imp_fam_density_year_mpa <- Site_biodiv %>%
    select(Year, Protection, paste0(families[[i]],"_Density"))%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(sd))
  names(sd_imp_fam_density_year_mpa) <- c("Year", "Protection", "SD_density")
  
  imp_fam_density_year_mpa[[i]] <- cbind(mean_imp_fam_density_year_mpa[,c("Year", "Mean_density")], sd_imp_fam_density_year_mpa[,c("SD_density","Protection")])
  
  
  # plot mean important Fish family density per year and protection status
  plot_fam_density_year_mpa3[[i]] <- ggplot(imp_fam_density_year_mpa[[i]], aes(x=Year, y=Mean_density, group=Protection))+
    geom_line(aes(color=Protection))+
    geom_point(aes(color=Protection))+
    geom_errorbar(aes(ymin=Mean_density-SD_density, ymax=Mean_density+SD_density,color=Protection), width=.2)+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Mean density (ind/m2)", title=paste0("Density of ", families[[i]], " per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  plot_fam_density_year_mpa[[i]] <- ggplot(fam_df2, aes(x=Year, y=Mean_density, fill=Protection))+
    geom_boxplot()+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Density (ind/m2)", title=paste0("Density of ", families[[i]], " per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
}


# add significance on graphs
pairwise.wilcox.test(Site_biodiv$Lutjanidae_Density, Site_biodiv$Year,
                     p.adjust.method = "BH")

pairwise.wilcox.test(Site_biodiv$Scaridae_Density, Site_biodiv$Year,
                     p.adjust.method = "BH")


plot_fam_density_year[[1]] <- plot_fam_density_year[[1]] + annotate(geom="text", x=1, y=0.3, label="*", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_density_year[[2]] <- plot_fam_density_year[[2]] + annotate(geom="text", x=1, y=1.6, label="**", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_density_year[[3]] <- plot_fam_density_year[[3]] + annotate(geom="text", x=1, y=0.03, label="**", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_density_year[[8]] <- plot_fam_density_year[[8]] + annotate(geom="text", x=5, y=0.14, label="**", hjust=0.5, size=6, color="red", fontface = "bold")

plot_fam_density_year[[2]] <- plot_fam_density_year[[2]] + annotate(geom="text", x=2.5, y=3.2, label="*", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_density_year[[3]] <- plot_fam_density_year[[3]] + annotate(geom="text", x=2.5, y=0.41, label="*", hjust=0.5, size=6, color="red", fontface = "bold")

plot_fam_density_year_mpa[[6]] <- plot_fam_density_year_mpa[[6]] + annotate(geom="text", x=9, y=0.0095, label="**", hjust=0.5, size=7, color="red", fontface = "bold")
plot_fam_density_year_mpa[[7]] <- plot_fam_density_year_mpa[[7]] + annotate(geom="text", x=9, y=0.0065, label="*", hjust=0.5, size=7, color="red", fontface = "bold")
plot_fam_density_year_mpa[[8]] <- plot_fam_density_year_mpa[[8]] + annotate(geom="text", x=9, y=0.22, label="*", hjust=0.5, size=7, color="red", fontface = "bold")




for (i in 1:length(families)) {
  ggsave(plot_fam_density_year[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Boxplot_",families[[i]],"_density.png"))
  ggsave(plot_fam_density_year_site[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Lineplot_",families[[i]],"_density_site.png"))
  ggsave(plot_fam_density_year_mpa[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Boxplot_",families[[i]],"_density_protection.png"))
  ggsave(plot_fam_density_year_mpa3[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Lineplot_",families[[i]],"_density_protection.png"))
  
}

###########################################################################
#### Important families biomass ####
families <- c("Acanthuridae", "Labridae", "Lutjanidae", "Serranidae", "Epinephelidae", "Carcharhinidae", "Dasyatidae", "Scaridae")

KW_imp_fam_biomass_obs <- vector("list")
KW_imp_fam_biomass_year <- vector("list")
KW_imp_fam_biomass_cyclone <- vector("list")
plot_fam_biomass_year <- vector("list")
KW_imp_fam_biomass_site <- vector("list")
plot_fam_biomass_year_site <- vector("list")
KW_imp_fam_biomass_mpa <- vector("list")
plot_fam_biomass_year_mpa <- vector("list")
plot_fam_biomass_year_mpa3 <- vector("list")
imp_fam_biomass_year_mpa <- vector("list")

for (i in 1:length(families)) {
  # create table with one family column
  fam_df <- Site_biodiv%>%select("Year",paste0(families[[i]],"_Biomass"),"Site", "Protection", "cyclone")%>% as.data.frame()
  fam_df2 <- data.frame(Year=fam_df$Year, Mean_biomass=fam_df[,2], Protection=fam_df$Protection, Site=fam_df$Site, cyclone=fam_df$cyclone)
  
  # Kruskal-Wallis mean comparison (Year)
  KW_imp_fam_biomass_year[[i]] <- kruskal.test(Mean_biomass~Year, fam_df2)
  
  # Kruskal-Wallis mean comparison (cyclone)
  KW_imp_fam_biomass_cyclone[[i]] <- kruskal.test(Mean_biomass~cyclone, fam_df2)
  
  # plot mean biomass of important families per year 
  plot_fam_biomass_year[[i]] <- ggplot(fam_df2, aes(x=Year, y=Mean_biomass))+
    geom_boxplot(fill="lightgrey")+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Biomass (kg)", title=paste0("Biomass of ", families[[i]]))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
 
  # Kruskal-Wallis mean comparison (Site)
  KW_imp_fam_biomass_site[[i]] <- kruskal.test(Mean_biomass~Site, fam_df2)
  
  # plot mean biomass of important families per year for each site
  plot_fam_biomass_year_site[[i]] <- ggplot(data=fam_df2, aes(x=Year, y=Mean_biomass, group=1))+
    geom_line()+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Biomass (kg)", title=paste0("Biomass of ", families[[i]]," per site"))+
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
  
 
  # Kruskal-Wallis mean comparison (MPA)
  KW_imp_fam_biomass_mpa[[i]] <- kruskal.test(Mean_biomass~Protection, fam_df2)
  
  # Compute mean and sd important fish families biomass per year and protection status
  mean_imp_fam_biomass_year_mpa <- Site_biodiv %>%
    select(Year, Protection, paste0(families[[i]],"_Biomass"))%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(mean))
  names(mean_imp_fam_biomass_year_mpa) <- c("Year", "Protection", "Mean_biomass")
  
  sd_imp_fam_biomass_year_mpa <- Site_biodiv %>%
    select(Year, Protection, paste0(families[[i]],"_Biomass"))%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(sd))
  names(sd_imp_fam_biomass_year_mpa) <- c("Year", "Protection", "SD_biomass")
  
  imp_fam_biomass_year_mpa[[i]] <- cbind(mean_imp_fam_biomass_year_mpa[,c("Year", "Mean_biomass")], sd_imp_fam_biomass_year_mpa[,c("SD_biomass","Protection")])
  
  
  # plot mean important Fish family biomass per year and protection status
  plot_fam_biomass_year_mpa3[[i]] <- ggplot(imp_fam_biomass_year_mpa[[i]], aes(x=Year, y=Mean_biomass, group=Protection))+
    geom_line(aes(color=Protection))+
    geom_point(aes(color=Protection))+
    geom_errorbar(aes(ymin=Mean_biomass-SD_biomass, ymax=Mean_biomass+SD_biomass,color=Protection), width=.2)+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Biomass (kg)", title=paste0("Biomass of ", families[[i]]," per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  plot_fam_biomass_year_mpa[[i]] <- ggplot(fam_df2, aes(x=Year, y=Mean_biomass, fill=Protection))+
    geom_boxplot()+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Biomass (kg)", title=paste0("Biomass of ", families[[i]]," per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"))
  
}


# add significance on graphs
pairwise.wilcox.test(Site_biodiv$Acanthuridae_Biomass, Site_biodiv$Year, p.adjust.method = "BH")
pairwise.wilcox.test(Site_biodiv$Labridae_Biomass, Site_biodiv$Year, p.adjust.method = "BH")
pairwise.wilcox.test(Site_biodiv$Scaridae_Biomass, Site_biodiv$Year, p.adjust.method = "BH")

plot_fam_biomass_year[[1]] <- plot_fam_biomass_year[[1]] + annotate(geom="text", x=1, y=7.5, label="***", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_biomass_year[[2]] <- plot_fam_biomass_year[[2]] + annotate(geom="text", x=1, y=1.5, label="**", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_biomass_year[[4]] <- plot_fam_biomass_year[[4]] + annotate(geom="text", x=5, y=0.14, label="**", hjust=0.5, size=6, color="red", fontface = "bold")
plot_fam_biomass_year[[8]] <- plot_fam_biomass_year[[8]] + annotate(geom="text", x=5, y=9, label="**", hjust=0.5, size=6, color="red", fontface = "bold")


plot_fam_biomass_year_mpa[[5]] <- plot_fam_biomass_year_mpa[[5]] + annotate(geom="text", x=9, y=7.5, label="**", hjust=0.5, size=7, color="red", fontface = "bold")
plot_fam_biomass_year_mpa[[6]] <- plot_fam_biomass_year_mpa[[6]] + annotate(geom="text", x=9, y=270, label="**", hjust=0.5, size=7, color="red", fontface = "bold")
plot_fam_biomass_year_mpa[[7]] <- plot_fam_biomass_year_mpa[[7]] + annotate(geom="text", x=9, y=90, label="*", hjust=0.5, size=7, color="red", fontface = "bold")

plot_fam_biomass_year[[2]] <- plot_fam_biomass_year[[2]] + annotate(geom="text", x=2.5, y=1.75, label="*", hjust=0.5, size=7, color="red", fontface = "bold")
plot_fam_biomass_year[[3]] <- plot_fam_biomass_year[[3]] + annotate(geom="text", x=2.5, y=20, label="**", hjust=0.5, size=7, color="red", fontface = "bold")
plot_fam_biomass_year[[8]] <- plot_fam_biomass_year[[8]] + annotate(geom="text", x=2.5, y=9, label="*", hjust=0.5, size=7, color="red", fontface = "bold")



for (i in 1:length(families)) {
  ggsave(plot_fam_biomass_year[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Boxplot_",families[[i]],"_biomass.png"))
  ggsave(plot_fam_biomass_year_site[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Lineplot_",families[[i]],"_biomass_site.png"))
  ggsave(plot_fam_biomass_year_mpa[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Boxplot_",families[[i]],"_biomass_protection.png"))
  ggsave(plot_fam_biomass_year_mpa3[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Family/Lineplot_",families[[i]],"_biomass_protection.png"))
  
}

###########################################################################
#### Trophic group proportions ####

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Trophic_abundance.rdata")


Fish_trophic_abund_site <- left_join(Fish_trophic_abund_site, Site_biodiv[,c("Year","Site_ID", "Site")])
Fish_trophic_abund_site <- Fish_trophic_abund_site%>%
  filter(!is.na(Site))
Fish_trophic_abund_site$Site <- as.factor(Fish_trophic_abund_site$Site)

# plot mean Fish family proportion per year
ggplot(Fish_trophic_abund_site, aes(x=Year,fill=trophic_group,y=Number))+
  geom_bar(position="fill",stat="identity")+
  labs(x="", y="Diet group proportion", title="Proportion of diet groups per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Plot_trophic_proportion_site.png")


###########################################################################
#### Important trophic group abundance ####

trophic <- c("Herbivorous", "Invertivorous", "Omnivorous", "Piscivorous")

KW_troph_abundance_obs <- vector("list")
KW_troph_abundance_year <- vector("list")
KW_troph_abundance_cyclone <- vector("list")
plot_troph_abundance_year <- vector("list")
KW_troph_abundance_site <- vector("list")
plot_troph_abundance_year_site <- vector("list")
KW_troph_abundance_mpa <- vector("list")
plot_troph_abundance_year_mpa <- vector("list")
plot_troph_abundance_year_mpa2 <- vector("list")
troph_abund_mpa <- vector("list")

for (i in 1:length(trophic)) {
  # create table with one trophic column
  troph_df <- Site_biodiv%>%select("Year",paste0(trophic[[i]],"_Abundance"),"Site", "Protection", "cyclone")%>% as.data.frame()
  troph_df2 <- data.frame(Year=troph_df$Year, Mean_abundance=troph_df[,2], Protection=troph_df$Protection, Site=troph_df$Site, cyclone=troph_df$cyclone)
  
  # Kruskal-Wallis mean comparison (obs)
  #KW_troph_abundance_obs[[i]] <- kruskal.test(Mean_abundance~Fish_observer, troph_df2)
  
  # Kruskal-Wallis mean comparison (Year)
  KW_troph_abundance_year[[i]] <- kruskal.test(Mean_abundance~Year, troph_df2)
  
  # Kruskal-Wallis mean comparison (cyclone)
  KW_troph_abundance_cyclone[[i]] <- kruskal.test(Mean_abundance~cyclone, troph_df2)
  
  # plot mean abundance of trophic group per year 
  plot_troph_abundance_year[[i]] <- ggplot(troph_df2, aes(x=Year, y=Mean_abundance))+
    geom_boxplot(fill="lightgrey")+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Abundance", title=paste0("Abundance of ", trophic[[i]]," fish"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  
  # Kruskal-Wallis mean comparison (Site)
  KW_troph_abundance_site[[i]] <- kruskal.test(Mean_abundance~Site, troph_df2)
  
  # plot abundance of trophic group per year for each site
  plot_troph_abundance_year_site[[i]] <- ggplot(data=troph_df2, aes(x=Year, y=Mean_abundance, group=1))+
    geom_line()+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Abundance", title=paste0("Abundance of ", trophic[[i]]," fish per site"))+
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

    # Kruskal-Wallis mean comparison (MPA)
  KW_troph_abundance_mpa[[i]] <- kruskal.test(Mean_abundance~Protection, troph_df2)
  
  # plot trophic abundance per year and protection status
  plot_troph_abundance_year_mpa[[i]] <- ggplot(troph_df2, aes(x=Year, y=Mean_abundance, fill=Protection))+
    geom_boxplot()+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Abundance", title=paste0("Abundance of ", trophic[[i]]," fish per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  # Compute mean and sd important trophic groups abundance per year and protection status
  mean_troph_abund_mpa <- troph_df2 %>%
    select(Year, Protection, Mean_abundance)%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(mean))
  names(mean_troph_abund_mpa) <- c("Year", "Protection", "Mean_Abundance")
  
  sd_troph_abund_mpa <- troph_df2 %>%
    select(Year, Protection, Mean_abundance)%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(sd))
  names(sd_troph_abund_mpa) <- c("Year", "Protection", "SD_Abundance")
  
  troph_abund_mpa[[i]] <- cbind(mean_troph_abund_mpa[,c("Year", "Mean_Abundance")], sd_troph_abund_mpa[,c("SD_Abundance","Protection")])
  
  plot_troph_abundance_year_mpa2[[i]] <- ggplot(data=troph_abund_mpa[[i]], aes(x=Year, y=Mean_Abundance, group=Protection))+
    geom_line(aes(color=Protection))+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    geom_errorbar(aes(ymin=Mean_Abundance-SD_Abundance, ymax=Mean_Abundance+SD_Abundance, color=Protection), width=.2)+
    labs(x="", y="Abundance", title=paste0("Abundance of ", trophic[[i]]," fish per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
}


# add significance on graphs

plot_troph_abundance_year[[3]] <- plot_troph_abundance_year[[3]] + annotate(geom="text", x=1, y=500, label="***", hjust=0.5, size=6, color="red", fontface = "bold")
plot_troph_abundance_year[[4]] <- plot_troph_abundance_year[[4]] + annotate(geom="text", x=5, y=100, label="**", hjust=0.5, size=6, color="red", fontface = "bold")

plot_troph_abundance_year[[3]] <- plot_troph_abundance_year[[3]] + annotate(geom="text", x=2.5, y=1000, label="*", hjust=0.5, size=6, color="red", fontface = "bold")

plot_troph_abundance_year_mpa[[2]] <- plot_troph_abundance_year_mpa[[2]] + annotate(geom="text", x=9, y=255, label="**", hjust=0.5, size=7, color="red", fontface = "bold")



for (i in 1:length(trophic)) {
  ggsave(plot_troph_abundance_year[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Boxplot_",trophic[[i]],"_abundance.png"))
  ggsave(plot_troph_abundance_year_site[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Lineplot_",trophic[[i]],"_abundance_site.png"))
  ggsave(plot_troph_abundance_year_mpa[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Boxplot_",trophic[[i]],"_abundance_protection.png"))
  ggsave(plot_troph_abundance_year_mpa2[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Lineplot_",trophic[[i]],"_abundance_protection.png"))
  
}

###########################################################################
#### Important trophic group density ####

trophic <- c("Herbivorous", "Invertivorous", "Omnivorous", "Piscivorous")

KW_troph_density_obs <- vector("list")
KW_troph_density_year <- vector("list")
KW_troph_density_cyclone <- vector("list")
plot_troph_density_year <- vector("list")
KW_troph_density_site <- vector("list")
plot_troph_density_year_site <- vector("list")
KW_troph_density_mpa <- vector("list")
plot_troph_density_year_mpa <- vector("list")
plot_troph_density_year_mpa2 <- vector("list")
troph_density_mpa <- vector("list")

for (i in 1:length(trophic)) {
  # create table with one trophic column
  troph_df <- Site_biodiv%>%select("Year",paste0(trophic[[i]],"_Density"),"Site", "Protection", "cyclone")%>% as.data.frame()
  troph_df2 <- data.frame(Year=troph_df$Year, Mean_density=troph_df[,2], Protection=troph_df$Protection, Site=troph_df$Site, cyclone=troph_df$cyclone)
  
  # Kruskal-Wallis mean comparison (obs)
  #KW_troph_density_obs[[i]] <- kruskal.test(Mean_density~Fish_observer, troph_df2)
  
  # Kruskal-Wallis mean comparison (Year)
  KW_troph_density_year[[i]] <- kruskal.test(Mean_density~Year, troph_df2)
  
  # Kruskal-Wallis mean comparison (cyclone)
  KW_troph_density_cyclone[[i]] <- kruskal.test(Mean_density~cyclone, troph_df2)
  
  # plot mean density of trophic per year 
  plot_troph_density_year[[i]] <- ggplot(troph_df2, aes(x=Year, y=Mean_density))+
    geom_boxplot(fill="lightgrey")+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Density (ind/m2)", title=paste0("Density of ", trophic[[i]]," fish"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
   
  # Kruskal-Wallis mean comparison (Site)
  KW_troph_density_site[[i]] <- kruskal.test(Mean_density~Site, troph_df2)
  
  # plot density of trophic per year for each site
  plot_troph_density_year_site[[i]] <- ggplot(data=troph_df2, aes(x=Year, y=Mean_density, group=1))+
    geom_line()+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Density (ind/m2)", title=paste0("Density of ", trophic[[i]]," fish per site"))+
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
  
   
  # Kruskal-Wallis mean comparison (MPA)
  KW_troph_density_mpa[[i]] <- kruskal.test(Mean_density~Protection, troph_df2)
  
  # plot trophic density per year and protection status
  plot_troph_density_year_mpa[[i]] <- ggplot(troph_df2, aes(x=Year, y=Mean_density, fill=Protection))+
    geom_boxplot()+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Density (ind/m2)", title=paste0("Density of ", trophic[[i]]," fish per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  # Compute mean and sd important fish families density per year and protection status
  mean_troph_density_mpa <- troph_df2 %>%
    select(Year, Protection, Mean_density)%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(mean))
  names(mean_troph_density_mpa) <- c("Year", "Protection", "Mean_density")
  
  sd_troph_density_mpa <- troph_df2 %>%
    select(Year, Protection, Mean_density)%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(sd))
  names(sd_troph_density_mpa) <- c("Year", "Protection", "SD_density")
  
  troph_density_mpa[[i]] <- cbind(mean_troph_density_mpa[,c("Year", "Mean_density")], sd_troph_density_mpa[,c("SD_density","Protection")])
  
  plot_troph_density_year_mpa2[[i]] <- ggplot(data=troph_density_mpa[[i]], aes(x=Year, y=Mean_density, group=Protection))+
    geom_line(aes(color=Protection))+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    geom_errorbar(aes(ymin=Mean_density-SD_density, ymax=Mean_density+SD_density, color=Protection), width=.2)+
    labs(x="", y="Density (ind/m2)", title=paste0("Density of ", trophic[[i]]," fish per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
}


# add significance on graphs

plot_troph_density_year[[3]] <- plot_troph_density_year[[3]] + annotate(geom="text", x=1, y=1.7, label="***", hjust=0.5, size=6, color="red", fontface = "bold")
plot_troph_density_year[[4]] <- plot_troph_density_year[[4]] + annotate(geom="text", x=5, y=0.3, label="**", hjust=0.5, size=6, color="red", fontface = "bold")

plot_troph_density_year[[3]] <- plot_troph_density_year[[3]] + annotate(geom="text", x=2.5, y=3.3, label="*", hjust=0.5, size=6, color="red", fontface = "bold")

plot_troph_density_year_mpa[[2]] <- plot_troph_density_year_mpa[[2]] + annotate(geom="text", x=9, y=0.85, label="**", hjust=0.5, size=7, color="red", fontface = "bold")



for (i in 1:length(trophic)) {
  ggsave(plot_troph_density_year[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Boxplot_",trophic[[i]],"_density.png"))
  ggsave(plot_troph_density_year_site[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Lineplot_",trophic[[i]],"_density_site.png"))
  ggsave(plot_troph_density_year_mpa[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Boxplot_",trophic[[i]],"_density_protection.png"))
  ggsave(plot_troph_density_year_mpa2[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Lineplot_",trophic[[i]],"_density_protection.png"))
  
}

###########################################################################
#### Important trophic group biomass ####

trophic <- c("Herbivorous", "Invertivorous", "Omnivorous", "Piscivorous")

KW_troph_biomass_obs <- vector("list")
KW_troph_biomass_year <- vector("list")
KW_troph_biomass_cyclone <- vector("list")
plot_troph_biomass_year <- vector("list")
KW_troph_biomass_site <- vector("list")
plot_troph_biomass_year_site <- vector("list")
KW_troph_biomass_mpa <- vector("list")
plot_troph_biomass_year_mpa <- vector("list")
plot_troph_biomass_year_mpa2 <- vector("list")
troph_biomass_mpa <- vector("list")

for (i in 1:length(trophic)) {
  # create table with one trophic column
  troph_df <- Site_biodiv%>%select("Year",paste0(trophic[[i]],"_Biomass"),"Site", "Protection", "cyclone")%>% as.data.frame()
  troph_df2 <- data.frame(Year=troph_df$Year, Mean_biomass=troph_df[,2], Protection=troph_df$Protection, Site=troph_df$Site, cyclone=troph_df$cyclone)
  
  # Kruskal-Wallis mean comparison (Year)
  KW_troph_biomass_year[[i]] <- kruskal.test(Mean_biomass~Year, troph_df2)
  
  # Kruskal-Wallis mean comparison (cyclone)
  KW_troph_biomass_cyclone[[i]] <- kruskal.test(Mean_biomass~cyclone, troph_df2)
  
  # plot mean biomass of trophic per year 
  plot_troph_biomass_year[[i]] <- ggplot(troph_df2, aes(x=Year, y=Mean_biomass))+
    geom_boxplot(fill="lightgrey")+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Biomass (Kg)", title=paste0("Biomass of ", trophic[[i]]," fish"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  
  # Kruskal-Wallis mean comparison (Site)
  KW_troph_biomass_site[[i]] <- kruskal.test(Mean_biomass~Site, troph_df2)
  
  # plot biomass of trophic per year for each site
  plot_troph_biomass_year_site[[i]] <- ggplot(data=troph_df2, aes(x=Year, y=Mean_biomass, group=1))+
    geom_line()+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Biomass (Kg)", title=paste0("Biomass of ", trophic[[i]]," fish per site"))+
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
  
  
  
  # Kruskal-Wallis mean comparison (MPA)
  KW_troph_biomass_mpa[[i]] <- kruskal.test(Mean_biomass~Protection, troph_df2)
  
  # plot trophic biomass per year and protection status
  plot_troph_biomass_year_mpa[[i]] <- ggplot(troph_df2, aes(x=Year, y=Mean_biomass, fill=Protection))+
    geom_boxplot()+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    labs(x="", y="Biomass (Kg)", title=paste0("Biomass of ", trophic[[i]], " fish per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
  # Compute mean and sd important fish families biomass per year and protection status
  mean_troph_biomass_mpa <- troph_df2 %>%
    select(Year, Protection, Mean_biomass)%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(mean))
  names(mean_troph_biomass_mpa) <- c("Year", "Protection", "Mean_biomass")
  
  sd_troph_biomass_mpa <- troph_df2 %>%
    select(Year, Protection, Mean_biomass)%>%
    group_by(Year, Protection)%>%
    summarise_all(funs(sd))
  names(sd_troph_biomass_mpa) <- c("Year", "Protection", "SD_biomass")
  
  troph_biomass_mpa[[i]] <- cbind(mean_troph_biomass_mpa[,c("Year", "Mean_biomass")], sd_troph_biomass_mpa[,c("SD_biomass","Protection")])
  
  plot_troph_biomass_year_mpa2[[i]] <- ggplot(data=troph_biomass_mpa[[i]], aes(x=Year, y=Mean_biomass, group=Protection))+
    geom_line(aes(color=Protection))+
    geom_point(aes(color=Protection))+
    geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
    geom_errorbar(aes(ymin=Mean_biomass-SD_biomass, ymax=Mean_biomass+SD_biomass, color=Protection), width=.2)+
    labs(x="", y="Biomass (Kg)", title=paste0("Biomass of ", trophic[[i]], " fish per protection status"))+
    scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"),
          panel.grid.minor = element_blank())
  
}


# add significance on graphs
pairwise.wilcox.test(Site_biodiv$Piscivorous_Biomass, Site_biodiv$Year, p.adjust.method = "BH")

plot_troph_biomass_year[[1]] <- plot_troph_biomass_year[[1]] + annotate(geom="text", x=2, y=6, label="**", hjust=0.5, size=6, color="red", fontface = "bold")
plot_troph_biomass_year[[4]] <- plot_troph_biomass_year[[4]] + annotate(geom="text", x=5, y=70, label="*", hjust=0.5, size=6, color="red", fontface = "bold")

plot_troph_biomass_year[[2]] <- plot_troph_biomass_year[[2]] + annotate(geom="text", x=2.5, y=100, label="*", hjust=0.5, size=6, color="red", fontface = "bold")
plot_troph_biomass_year[[4]] <- plot_troph_biomass_year[[4]] + annotate(geom="text", x=2.5, y=275, label="*", hjust=0.5, size=6, color="red", fontface = "bold")

plot_troph_biomass_year_mpa[[2]] <- plot_troph_biomass_year_mpa[[2]] + annotate(geom="text", x=9, y=100, label="*", hjust=0.5, size=7, color="red", fontface = "bold")
plot_troph_biomass_year_mpa[[4]] <- plot_troph_biomass_year_mpa[[4]] + annotate(geom="text", x=9, y=275, label="*", hjust=0.5, size=7, color="red", fontface = "bold")

for (i in 1:length(trophic)) {
  ggsave(plot_troph_biomass_year[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Boxplot_",trophic[[i]],"_biomass.png"))
  ggsave(plot_troph_biomass_year_site[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Lineplot_",trophic[[i]],"_biomass_site.png"))
  ggsave(plot_troph_biomass_year_mpa[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Boxplot_",trophic[[i]],"_biomass_protection.png"))
  ggsave(plot_troph_biomass_year_mpa2[[i]], file=paste0("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Fish/Trophic/Lineplot_",trophic[[i]],"_biomass_protection.png"))
  
}
