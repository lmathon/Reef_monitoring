#load libraries
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(jtools)
library(ggstance)
library(rms)
library(fitdistrplus)
library(huxtable)
library(parallel)
library(foreach)
library(doParallel)
library(dplyr)
library(here)
library(raster)

setwd("C:/Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/R_scripts/02_Analyses_Plots/BRT/BRT_Coral_cover")

# Load BRT functions
source("BRT_Functions.R")



# Load fish data
load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Site_biodiversity_metrics.rdata")
meta <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/1 - Metadata/GCRMN_metadata.csv", sep = ",", na.strings = "NA")

meta$Protection[is.na(meta$Protection)] <- "Unprotected"
Site_biodiv$Year <- as.factor(Site_biodiv$Year)

myData <- left_join(meta[,c("Site_ID", "Protection", "Depth_m", "SST", "DHW", "dist_to_coast")], Site_biodiv)

myData <- myData %>%
  filter(!is.na(Site))

myData <- myData %>%
  mutate(cyclone = case_when(
    Year%in%c("2016", "2017_pre") ~ "Pre-cyclone",
    Year%in%c("2017_post", "2018","2019","2020","2021","2022","2023")  ~ "Post-cyclone"))

load("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Benthic_cover_site_all.rdata")
bleach_disease <- read.csv("c://Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/Data/3 - Clean_data/Bleaching_Disease.csv", sep = ";", na.strings = "")

myData <- left_join(myData, bleach_disease)

myData <- left_join(myData, Benthic_cover_site_all[,c(1,3,4,5,7,9)])

myData$Site <- as.factor(myData$Site)
myData$Protection <- as.factor(myData$Protection)
myData$cyclone <- as.factor(myData$cyclone)

# Define response variable and predictors
myResponse=c("CORAL")

myPredictor=c("Year","Protection","cyclone", "SST",
              "DHW", "dist_to_coast", "Fish_density", "GORGONIANS", "SPONGES", "MACROALGAE",
              "Acanthuridae_Density", "Labridae_Density","Scaridae_Density", 
              "Herbivorous_Density", "Piscivorous_Density", 
              "Recruits_density")

myPredictorNumeric=c("SST", "DHW", "dist_to_coast", "Fish_density", 
                     "GORGONIANS", "SPONGES", "MACROALGAE",
                     "Acanthuridae_Density", "Labridae_Density", "Lutjanidae_Density", "Scaridae_Density", 
                     "Herbivorous_Density", "Invertivorous_Density", "Omnivorous_Density", "Piscivorous_Density", 
                     "Recruits_density")



# verifier les correlations entre predicteurs numeriques
cort = cor(na.omit(myData[,myPredictorNumeric]))
cort


# calcul parallele

cores = detectCores()
cores    # ma machine a 8 coeurs


# Setting the different parameters to combine
tree.complexity = c(1:5)
learning.rate = c(0.01, 0.005, 0.001)
bag.fraction = c(1,2)


#Make cluster for parallel processing
cores = detectCores()
cl = makeCluster(cores-1)
registerDoParallel(cl)


responseName=myResponse # in case there is only one response variable
print(paste("----------------------", responseName))


### Optimisation

# define empty output matrix
output <- matrix(ncol=9)

# Run the function through the for loop to test all parameter combinations

par_output =  foreach(i = tree.complexity, .packages=c("foreach")) %dopar% {
  foreach(j = learning.rate, .packages=c("foreach")) %dopar% {
    foreach(k = bag.fraction, .packages=c("foreach")) %dopar% {
      #need to load package within foreach loop
      source("BRT_Functions.R")
      # model name
      nam = paste0("Model_",responseName,"_tc_", i, "_lr_", j, "_bf_", k)
      # model optimization
      t = optimize_gaussian_brts(tree.com = i,learn = j, bag.f = k, myData, responseName, myPredictor)
      # assign results to output matrix
      if(!is.null(t$interaction.depth)){ output = rbind(output, c(nam, unlist(t))) }
      output
    }
  }
}




# extract best brts parameters
best_parameters = extract_best_parameters_par(par_output, responseName, "gaussian")
best_parameters


### Fit best gaussian BRT with fixed nb of trees

# Fit best model
mod_best_fixed = fit_best_gaussian_brt_fixed(myData, responseName, best_parameters, myPredictor)
mod_best_fixed
summary(mod_best_fixed)
names(mod_best_fixed)
mod_best_fixed$contributions

# Make plot of variable contributions best fixed model
make_contribution_reduced_plot(mod_best_fixed, responseName, "poisson")


# Get variables with contributions > 5%
var_sup5_best_fixed = get_variables_contrib_sup5(mod_best_fixed)


### Fit best gaussian BRT REDUCED (same as brt)

# Refit after dropping predictors with contributions < 5%
mod_best_fixed_reduced = fit_best_reduced_gaussian_brt_fixed(myData, responseName, best_parameters,
                                                             preds = var_sup5_best_fixed)
mod_best_fixed_reduced
summary(mod_best_fixed_reduced)
names(mod_best_fixed_reduced)
mod_best_fixed_reduced$contributions
mod_best_fixed_reduced$var.names

# Make plot of variable contributions reduced model
make_contribution_reduced_plot(mod_best_fixed_reduced, responseName, "poisson")

# Partial dependance plots reduced model
partial_dependance_plots3(mod_best_fixed_reduced, responseName, "poisson")

# Refit a gbmStep after dropping predictors with contributions < 5%
mod_best_gbmStep_reduced = fit_best_reduced_gaussian_brt_gbmStep(myData, responseName, best_parameters,
                                                                 preds = var_sup5_best_fixed)

names(mod_best_gbmStep_reduced)
mod_best_gbmStep_reduced$cv.statistics
mod_best_gbmStep_reduced$shrinkage
mod_best_gbmStep_reduced$n.trees
mod_best_gbmStep_reduced$contributions
mod_best_fixed_reduced$contributions


#explore interactions for best reduced model (must be a gbmStep - dont work with gbmfixed)
find.int <- dismo::gbm.interactions(mod_best_gbmStep_reduced)
find.int$interactions
find.int$rank.list

#dev.new()

png(paste0("InteractionPlotsBestModel.png"), width = 1200, height = 600)

par(mfrow=c(1,2))
dismo::gbm.perspec(mod_best_gbmStep_reduced, 4, 2, z.range=c(0,2))
dismo::gbm.perspec(mod_best_gbmStep_reduced, 1, 3, z.range=c(1,3.5))

dev.off()

par(mfrow=c(1,1))




png(paste0("InteractionPlotsBestModelDismo.png"), width = 1200, height = 600)

dismo::gbm.plot(mod_best_gbmStep_reduced, n.plots=5, plot.layout=c(3, 2), write.title = FALSE)

dev.off()



gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1))
gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1,2),level.plot=FALSE)




#Stop cluster
stopCluster(cl)
