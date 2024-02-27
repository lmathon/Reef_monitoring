library(tidyverse)
library(ggplot2)
library(ggpubr)
library(reshape2)

# Load data
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Benthic_cover_site_all.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep = ",", na.strings = "NA")

meta$Protection[is.na(meta$Protection)] <- "Unprotected"

Benthic_cover <- left_join(meta[,c("Site_ID", "Protection", "Year", "Site")], Benthic_cover_site_all)

Benthic_cover$Year <- as.factor(Benthic_cover$Year)


# add cyclone information
Benthic_cover <- Benthic_cover %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))


# keep only important columns
Cover <- Benthic_cover[,c(1:4,128,6:16,121,126,127)]


#########################################################################################
#### Cover proportion per site ####

df <- Cover[,c(1,6:10,17,11:16)]
df <- melt(df)
df <- left_join(df, Cover[,c("Site_ID","Year","Site")])
# plot mean benthos proportion per year
ggplot(df, aes(x=Year,fill=variable, y=value))+
  geom_bar(position="fill", stat = "identity")+
  labs(x="", y="Cover proportion", title="Coverage of benthic categories")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle = 90, hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())+ 
  guides(fill=guide_legend(title=""))

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Plot_cover_proportion.png")

#########################################################################################
#### Coral cover ####


# test normality of Coral cover
ggqqplot(Benthic_cover$CORAL)
shapiro.test(Benthic_cover$CORAL) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_Coral_year <- kruskal.test(CORAL~Year, Benthic_cover) # --> pas d'effet annee

# Kruskal-Wallis mean comparison test (Cyclone)
KW_Coral_cyclone <- kruskal.test(CORAL~cyclone, Benthic_cover) # --> pas d'effet cyclone


# plot mean Coral cover per year 
ggplot(Benthic_cover, aes(x=Year, y=CORAL))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral cover (%)", title="Percentage of coral cover")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_coral_cover.png")

# Kruskal-Wallis mean comparison test (Site)
KW_Coral_site <- kruskal.test(CORAL~Site, Benthic_cover) # --> differences between sites
pairwise.wilcox.test(Benthic_cover$CORAL, Benthic_cover$Site,
                     p.adjust.method = "BH") # --> Mikes Maze different

# plot Coral cover per year for each site
ggplot(Benthic_cover, aes(x=Year, y=CORAL, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral cover (%)", title="Percentage of coral cover per site")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle = 90, hjust=1),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_coral_cover_site.png")

# Kruskal-Wallis mean comparison test (MPA/outside)
KW_Coral_mpa <- kruskal.test(CORAL~Protection, Benthic_cover) # --> significant difference between MPA and unprotected

# Compute mean and sd Coral cover per year and protection status
mean_Coral_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, CORAL)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_Coral_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, CORAL)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_Coral_year_mpa) <- c("Year", "Protection", "SD_cover")

coral_year_mpa <- cbind(mean_Coral_year_mpa[,c("Year", "CORAL")], sd_Coral_year_mpa[,c("SD_cover","Protection")])

# plot mean Coral cover per year and protection status
ggplot(Benthic_cover, aes(x=Year, y=CORAL, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=15, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Coral cover (%)", title="Percentage of coral cover per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_coral_cover_protection.png")


ggplot(coral_year_mpa, aes(x=Year, y=CORAL, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Coral cover (%)", title="Percentage of coral cover per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_coral_cover_protection.png")

#########################################################################################
#### Gorgonian cover ####

# test normality of Gorgonian cover
ggqqplot(Benthic_cover$GORGONIANS)
shapiro.test(Benthic_cover$GORGONIANS) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_Gorgonian_year <- kruskal.test(GORGONIANS~Year, Benthic_cover) # --> pas d'effet annee

# Kruskal-Wallis mean comparison test (Cyclone)
KW_Gorgonian_cyclone <- kruskal.test(GORGONIANS~cyclone, Benthic_cover) # --> pas d'effet cyclone


# plot mean Gorgonian cover per year 
ggplot(Benthic_cover, aes(x=Year, y=GORGONIANS))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Gorgonian cover (%)", title="Percentage of gorgonian cover")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_gorgonian_cover.png")

# Kruskal-Wallis mean comparison test (Site)
KW_Gorgonian_site <- kruskal.test(GORGONIANS~Site, Benthic_cover) # Differences between sites
pairwise.wilcox.test(Benthic_cover$GORGONIANS, Benthic_cover$Site,
                     p.adjust.method = "BH") # --> Tous differents


# plot Gorgonian cover per year for each site
ggplot(Benthic_cover, aes(x=Year, y=GORGONIANS, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Gorgonian cover (%)", title="Percentage of gorgonian cover per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_gorgonian_cover_site.png")


# Kruskal-Wallis mean comparison test (MPA/outside)
KW_Gorgonian_mpa <- kruskal.test(GORGONIANS~Protection, Benthic_cover) # Effet significatif de la protection


# Compute mean and sd Gorgonian cover per year and protection status
mean_Gorgonian_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, GORGONIANS)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_Gorgonian_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, GORGONIANS)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_Gorgonian_year_mpa) <- c("Year", "Protection", "SD_cover")

Gorgonian_year_mpa <- cbind(mean_Gorgonian_year_mpa[,c("Year", "GORGONIANS")], sd_Gorgonian_year_mpa[,c("SD_cover","Protection")])

# plot mean Gorgonian cover per year and protection status
ggplot(Benthic_cover, aes(x=Year, y=GORGONIANS, fill=Protection))+
  geom_boxplot()+
  annotate(geom="text", x=9, y=40, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Gorgonian cover (%)", title="Percentage of gorgonian cover per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_gorgonian_cover_protection.png")

ggplot(Gorgonian_year_mpa, aes(x=Year, y=GORGONIANS, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Gorgonian cover (%)", title="Percentage of gorgonian cover per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_gorgonian_cover_protection.png")


#########################################################################################
#### Sponge cover ####

# test normality of Sponge cover
ggqqplot(Benthic_cover$SPONGES)
shapiro.test(Benthic_cover$SPONGES) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_Sponge_year <- kruskal.test(SPONGES~Year, Benthic_cover) # --> Effet annee
pairwise.wilcox.test(Benthic_cover$SPONGES, Benthic_cover$Year,
                     p.adjust.method = "BH") # --> 2019 different

# Kruskal-Wallis mean comparison test (Cyclone)
KW_Sponge_cyclone <- kruskal.test(SPONGES~cyclone, Benthic_cover) # --> pas d'effet cyclone


# plot mean Sponge cover per year 
ggplot(Benthic_cover, aes(x=Year, y=SPONGES))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=5, y=8, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Sponge cover (%)", title="Percentage of sponge cover")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_sponge_cover.png")

# Kruskal-Wallis mean comparison test (Site)
KW_Sponge_site <- kruskal.test(SPONGES~Site, Benthic_cover) # Differences between sites
pairwise.wilcox.test(Benthic_cover$SPONGES, Benthic_cover$Site,
                     p.adjust.method = "BH") # --> Tous differents


# plot Sponge cover per year for each site
ggplot(Benthic_cover, aes(x=Year, y=SPONGES, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Sponge cover (%)", title="Percentage of sponge cover per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_sponge_cover_site.png")


# Kruskal-Wallis mean comparison test (MPA/outside)
KW_Sponge_mpa <- kruskal.test(SPONGES~Protection, Benthic_cover) # Effet significatif de la protection


# Compute mean and sd Sponge cover per year and protection status
mean_Sponge_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, SPONGES)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_Sponge_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, SPONGES)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_Sponge_year_mpa) <- c("Year", "Protection", "SD_cover")

Sponge_year_mpa <- cbind(mean_Sponge_year_mpa[,c("Year", "SPONGES")], sd_Sponge_year_mpa[,c("SD_cover","Protection")])

# plot mean Sponge cover per year and protection status
ggplot(Benthic_cover, aes(x=Year, y=SPONGES, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=30, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Sponge cover (%)", title="Percentage of sponge cover per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_sponge_cover_protection.png")


ggplot(Sponge_year_mpa, aes(x=Year, y=SPONGES, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Sponge cover (%)", title="Percentage of sponge cover per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_sponge_cover_protection.png")

#########################################################################################
#### Macroalgae cover ####

# test normality of Macroalgae cover
ggqqplot(Benthic_cover$MACROALGAE)
shapiro.test(Benthic_cover$MACROALGAE) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_Macroalgae_year <- kruskal.test(MACROALGAE~Year, Benthic_cover) # --> effet annee

# Kruskal-Wallis mean comparison test (Cyclone)
KW_Macroalgae_cyclone <- kruskal.test(MACROALGAE~cyclone, Benthic_cover) # --> effet cyclone


# plot mean Macroalgae cover per year 
ggplot(Benthic_cover, aes(x=Year, y=MACROALGAE))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=2.5, y=48, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Macroalgae cover (%)", title="Percentage of macroalgae cover")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_macroalgae_cover.png")


# Kruskal-Wallis mean comparison test (Site)
KW_Macroalgae_site <- kruskal.test(MACROALGAE~Site, Benthic_cover) # Differences between sites
pairwise.wilcox.test(Benthic_cover$MACROALGAE, Benthic_cover$Site,
                     p.adjust.method = "BH") # --> Tous differents


# plot Macroalgae cover per year for each site
ggplot(Benthic_cover, aes(x=Year, y=MACROALGAE, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Macroalgae cover (%)", title="Percentage of macroalgae cover per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_macroalgae_cover_site.png")

# Kruskal-Wallis mean comparison test (MPA/outside)
KW_Macroalgae_mpa <- kruskal.test(MACROALGAE~Protection, Benthic_cover) # Effet significatif de la protection


# Compute mean and sd Macroalgae cover per year and protection status
mean_Macroalgae_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, MACROALGAE)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_Macroalgae_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, MACROALGAE)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_Macroalgae_year_mpa) <- c("Year", "Protection", "SD_cover")

Macroalgae_year_mpa <- cbind(mean_Macroalgae_year_mpa[,c("Year", "MACROALGAE")], sd_Macroalgae_year_mpa[,c("SD_cover","Protection")])

# plot mean Macroalgae cover per year and protection status
ggplot(Benthic_cover, aes(x=Year, y=MACROALGAE, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=60, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Macroalgae cover (%)", title="Percentage of macroalgae cover per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_macroalgae_cover_protection.png")

ggplot(Macroalgae_year_mpa, aes(x=Year, y=MACROALGAE, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Mean Macroalgae cover (%)", title="Percentage of macroalgae cover per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_macroalgae_cover_protection.png")

#########################################################################################
#### Bleaching frequency ####

# test normality of Bleaching frequency
ggqqplot(Benthic_cover$Bleaching_frequency)
shapiro.test(Benthic_cover$Bleaching_frequency) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_Bleaching_year <- kruskal.test(Bleaching_frequency~Year, Benthic_cover) # --> Pas d'effet annee

# Kruskal-Wallis mean comparison test (Cyclone)
KW_Bleaching_cyclone <- kruskal.test(Bleaching_frequency~cyclone, Benthic_cover) # --> pas d'effet cyclone


# plot mean Bleaching frequency per year 
ggplot(Benthic_cover, aes(x=Year, y=Bleaching_frequency))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Proportion of frames", title = "Coral bleaching frequency")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_bleaching_frequency.png")


# Kruskal-Wallis mean comparison test (Site)
KW_Bleaching_site <- kruskal.test(Bleaching_frequency~Site, Benthic_cover) # Differences between sites
pairwise.wilcox.test(Benthic_cover$Bleaching_frequency, Benthic_cover$Site,
                     p.adjust.method = "BH") # --> Sites proteges


# plot Bleaching frequency per year for each site
ggplot(Benthic_cover, aes(x=Year, y=Bleaching_frequency, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Proportion of frames", title = "Coral bleaching frequency per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_bleaching_frequency_site.png")




# Kruskal-Wallis mean comparison test (MPA/outside)
KW_Bleaching_mpa <- kruskal.test(Bleaching_frequency~Protection, Benthic_cover) # Effet significatif de la protection


# Compute mean and sd Bleaching frequency per year and protection status
mean_Bleaching_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, Bleaching_frequency)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_Bleaching_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, Bleaching_frequency)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_Bleaching_year_mpa) <- c("Year", "Protection", "SD_cover")

Bleaching_year_mpa <- cbind(mean_Bleaching_year_mpa[,c("Year", "Bleaching_frequency")], sd_Bleaching_year_mpa[,c("SD_cover","Protection")])

# plot mean Bleaching frequency per year and protection status
ggplot(Benthic_cover, aes(x=Year, y=Bleaching_frequency, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  annotate(geom="text", x=9, y=0.3, label="***", hjust=0.5, size=6, color="red", fontface = "bold")+
  labs(x="", y="Proportion of frames", title = "Coral bleaching frequency per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_bleaching_frequency_protection.png")


ggplot(Bleaching_year_mpa, aes(x=Year, y=Bleaching_frequency, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Proportion of frames", title = "Coral bleaching frequency per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_bleaching_frequency_protection.png")


#########################################################################################
#### Disease frequency ####

# test normality of Disease frequency
ggqqplot(Benthic_cover$Disease_frequency)
shapiro.test(Benthic_cover$Disease_frequency) # -> not normal

# Kruskal-Wallis mean comparison test (Year)
KW_Disease_year <- kruskal.test(Disease_frequency~Year, Benthic_cover) # --> Pas d'effet annee

# Kruskal-Wallis mean comparison test (Cyclone)
KW_Disease_cyclone <- kruskal.test(Disease_frequency~cyclone, Benthic_cover) # --> pas d'effet cyclone


# plot mean Disease frequency per year 
ggplot(Benthic_cover, aes(x=Year, y=Disease_frequency))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Proportion of frames", title = "Coral disease frequency")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_disease_frequency.png")


# Kruskal-Wallis mean comparison test (Site)
KW_Disease_site <- kruskal.test(Disease_frequency~Site, Benthic_cover) # Pas d'effet site


# plot Disease frequency per year for each site
ggplot(Benthic_cover, aes(x=Year, y=Disease_frequency, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Proportion of frames", title = "Coral disease frequency per site")+
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

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_disease_frequency_site.png")



# Kruskal-Wallis mean comparison test (MPA/outside)
KW_Disease_mpa <- kruskal.test(Disease_frequency~Protection, Benthic_cover) # Pas d'effet protection


# Compute mean and sd Disease frequency per year and protection status
mean_Disease_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, Disease_frequency)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(mean))

sd_Disease_year_mpa <- Benthic_cover %>%
  dplyr::select(Year, Protection, Disease_frequency)%>%
  group_by(Year, Protection)%>%
  summarise_all(funs(sd))
names(sd_Disease_year_mpa) <- c("Year", "Protection", "SD_cover")

Disease_year_mpa <- cbind(mean_Disease_year_mpa[,c("Year", "Disease_frequency")], sd_Disease_year_mpa[,c("SD_cover","Protection")])

# plot mean Disease frequency per year and protection status
ggplot(Benthic_cover, aes(x=Year, y=Disease_frequency, fill=Protection))+
  geom_boxplot()+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Proportion of frames", title = "Coral disease frequency per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Boxplot_disease_frequency_protection.png")


ggplot(Disease_year_mpa, aes(x=Year, y=Disease_frequency, group=Protection))+
  geom_line(aes(color=Protection))+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Proportion of frames", title = "Coral disease frequency per protection status")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())

ggsave("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Outputs/Cover/Lineplot_disease_frequency_protection.png")




#########################################################################################
#### Acropora cover

acropora <- Benthic_cover[,c(1:4,17:19,127)]
acropora$acropora <- rowSums(acropora[,c(5:7)])


# Kruskal-Wallis mean comparison test (Cyclone)
KW_acropora_cyclone <- kruskal.test(acropora~cyclone, acropora) # --> pas d'effet cyclone


# plot mean Coral cover per year 
ggplot(acropora, aes(x=Year, y=acropora))+
  geom_boxplot(fill="lightgrey")+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral cover (%)")+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()

# plot Coral cover per year for each site
ggplot(acropora, aes(x=Year, y=acropora, group=1))+
  geom_line()+
  geom_point(aes(color=Protection))+
  geom_vline(xintercept = 2.5, linetype="twodash", color="grey30")+
  labs(x="", y="Coral cover (%)")+
  facet_wrap(~Site)+
  scale_x_discrete(limits=c("2016", "2017_pre", "2017_post", "2018", "2019","2020","2021","2022","2023"))+
  theme_bw()
