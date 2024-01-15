setwd("C:/Users/mathonlocal/Desktop/Nature Foundation/CORENA Project/Reef_Monitoring/R_scripts/02_Analyses_Plots/BRT/BRT_Fish_biomass")

#########################################################################################################
#########################################################################################################
#' Optimize BRT parameters by running through multiple iterations of BRT, changing tree.complexity,
#' learning rate and bag.fraction (from Fraser J.)
#' for Gaussian BRTs
#' @param tree.com
#' @param learn
#' @param bag.f
#' @param data_brts
#' @param responseName_brts
#'
#' @return
#' @export
#'

optimize_gaussian_brts <- function(tree.com, learn, bag.f, data_brts, responseName_brts, myPredictor_brts){
  
  # set seed for reproducibility
  set.seed(12345)
  
  # remove na in response variable
  data_brts2 = subset(data_brts, !is.na(data_brts[,responseName_brts]))
  
  # get column response number
  col_response = which(colnames(data_brts2) == responseName_brts)
  
  # assess optimal number of trees using k-fold cross validation
  # from help: The data is divided into 10 subsets, with stratification by prevalence if required for presence/absence data.
  # The function then fits a gbm model of increasing complexity along the sequence from n.trees to n.trees + (n.steps * step.size),
  # calculating the residual deviance at each step along the way. After each fold processed, the function calculates the average
  # holdout residual deviance and its standard error and then identifies the optimal number of trees as that at which the holdout
  # deviance is minimised. It fits a model with this number of trees, returning it as a gbm model along with additional information
  # from the cross-validation selection process.
  
  k1 = dismo::gbm.step(data = data_brts2,
                       gbm.x = myPredictor_brts,
                       gbm.y = col_response,
                       family = "gaussian",
                       tree.complexity = tree.com,
                       learning.rate = learn,
                       bag.fraction = bag.f,
                       prev.stratify = FALSE,
                       n.folds = 10,
                       n.trees = 700,
                       step.size = 25,
                       silent = TRUE,
                       plot.main = FALSE,
                       n.cores = cores)
  
  k.out = list(interaction.depth = k1$interaction.depth,
               shrinkage = k1$shrinkage,
               n.trees = k1$n.trees,
               deviance = k1$self.statistics$mean.resid,
               cv.deviance = k1$cv.statistics$deviance.mean,
               cvse = k1$cv.statistics$deviance.se,
               cvcorr = k1$cv.statistics$correlation.mean,
               cvcorrse = k1$cv.statistics$correlation.se)
  
  return(k.out)
}


#########################################################################################################
#########################################################################################################


#' Extract best brts parameters for parallel processing output
#'
#' @param output
#' @param responseName_brts
#' @param distrib
#'
#' @return
#' @export
#'

extract_best_parameters_par <- function(output, responseName_brts, distrib){
  
  # convert matrix to dataframe
  df <- na.omit(rbind(data.frame(matrix(unlist(output[[1]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[1]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[1]][[3]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[2]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[2]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[2]][[3]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[3]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[3]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[3]][[3]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[4]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[4]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[4]][[3]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[5]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[5]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[5]][[3]][[2]]), ncol = 9, byrow = F))
  ))
  
  # Change column names
  names(df) = c("model", "TC", "LR", "n.trees", "dev", "cv.dev", "cv.se", "corr", "corr.se")
  
  # change column class
  df[,1:9] <- lapply(df[,1:9], as.character) # laurent
  df[,2:9] <- lapply(df[,2:9], as.numeric) # laurent
  
  # Round columns
  df[,5:9]  =  round(df[,5:9],digits=3)
  
  # Sort by cv.dev and cv.se (used to assess the model, the lowest the better)
  #nb low cv.dev corresponds to high corr
  df  =  df[order(df$cv.dev, df$cv.se),]
  
  # add bag fraction
  df %>%
    dplyr::mutate(bag.fract = as.numeric(substr(df$model, nchar(as.character(df$model))-2, nchar(as.character(df$model))))) -> df
  
  #write results
  write.csv(list(df = df),file=paste0("train_results_", responseName_brts, "_", distrib, ".csv"))
  
  # extract best parameters
  best_parameters = df[1,]
  
  return(best_parameters)
  
}


#########################################################################################################
#########################################################################################################


#' Fits optimal Gaussian BRT fixed nb of trees
#'
#' @param data_brts
#' @param responseName_brts
#' @param best_params
#' @param myPredictor_brts
#'
#' @return
#' @export
#'

fit_best_gaussian_brt_fixed <- function(data_brts, responseName_brts, best_params, myPredictor_brts){
  
  # remove na in response variable
  data_brts2 = subset(data_brts, !is.na(data_brts[,responseName_brts]))
  
  # get column response number
  col_response = which(colnames(data_brts2) == responseName_brts)
  
  
  #fit model with fixed number of trees
  
  tbmod = dismo::gbm.fixed(data = data_brts2, gbm.x = myPredictor_brts,
                           gbm.y = col_response,
                           family = "gaussian",
                           tree.complexity = as.numeric(best_params[,2]),
                           learning.rate = as.numeric(best_params[,3]),
                           bag.fraction = as.numeric(best_params[,10]),
                           n.trees = as.numeric(best_params[,4]))
  
  save(tbmod, file = paste0("best_fixed_gaussian_brt_", responseName_brts, ".RData"))
  
  return(tbmod)
}


#########################################################################################################
#########################################################################################################


#' Get variables with model contribution > 5%
#'
#' @param model
#'
#' @return
#' @export
#'

get_variables_contrib_sup5 <- function(model){
  
  contrib = summary(model)
  
  contrib %>% dplyr::filter(rel.inf > 5) -> contrib_sup5
  
  variables = contrib_sup5$var
  
  return(variables)
  
}

#########################################################################################################
#########################################################################################################


#' Fits reduced optimal Gaussian BRT fixed nb of trees after dropping predictors with contributions < 5%
#'
#' @param data_brts
#' @param responseName_brts
#' @param best_params
#' @param preds
#'
#' @return
#' @export
#'

fit_best_reduced_gaussian_brt_fixed <- function(data_brts, responseName_brts,  best_params, preds){
  
  # remove na in response variable
  data_brts2 = subset(data_brts, !is.na(data_brts[,responseName_brts]))
  
  # get column response number
  col_response = which(colnames(data_brts2) == responseName_brts)
  
  
  
  #fit model with fixed number of trees
  tbmod = dismo::gbm.fixed(data = data_brts2, gbm.x = as.character(preds),
                           gbm.y = col_response,
                           family = "gaussian",
                           tree.complexity = as.numeric(best_params[,2]),
                           learning.rate = as.numeric(best_params[,3]),
                           bag.fraction = as.numeric(best_params[,10]),
                           n.trees = as.numeric(best_params[,4]))
  
  save(tbmod, file = paste0("best_fixed_reduced_gaussian_brt_", responseName_brts, ".RData"))
  
  return(tbmod)
}



#########################################################################################################
#########################################################################################################


#' Fits reduced optimal Gaussian BRT gbm.step after dropping predictors with contributions < 5%
#'
#' @param data_brts
#' @param responseName_brts
#' @param best_params
#' @param preds
#'
#' @return
#' @export
#'

fit_best_reduced_gaussian_brt_gbmStep <- function(data_brts, responseName_brts,  best_params, preds){
  
  # set seed for reproducibility
  set.seed(12345)
  
  # remove na in response variable
  data_brts2 = subset(data_brts, !is.na(data_brts[,responseName_brts]))
  
  # get column response number
  col_response = which(colnames(data_brts2) == responseName_brts)
  
  
  
  #fit model with step-variable number of trees
  tbmod = dismo::gbm.step(data = data_brts2, gbm.x = as.character(preds),
                          gbm.y = col_response,
                          family = "gaussian",
                          tree.complexity = as.numeric(best_params[,2]),
                          learning.rate = as.numeric(best_params[,3]),
                          bag.fraction = as.numeric(best_params[,10]),
                          prev.stratify = FALSE,
                          n.folds = 10,
                          n.trees = 700,
                          step.size = 25,
                          silent = TRUE,
                          plot.main = FALSE,
                          n.cores = cores)
  
  k.out = data.frame (BestGbmStep=c("TreeComplexity",
                                    "LearningRate",
                                    "n.trees",
                                    "deviance",
                                    "cv.deviance.mean",
                                    "cv.deviance.se",
                                    "cv.correlation.mean",
                                    "cv.correlation.se"),
                      Value=c(tbmod$interaction.depth,
                              tbmod$shrinkage,
                              tbmod$n.trees,
                              tbmod$self.statistics$mean.resid,
                              tbmod$cv.statistics$deviance.mean,
                              tbmod$cv.statistics$deviance.se,
                              tbmod$cv.statistics$correlation.mean,
                              tbmod$cv.statistics$correlation.se))
  
  
  write.csv(list(BestModel = k.out),file=paste0("final_results_mod_best_gbmStep_reduced_", responseName_brts, "_gaussian_", ".csv"))
  save(tbmod, file = paste0("best_gbmStep_reduced_gaussian_brt_", responseName_brts, ".RData"))
  
  return(tbmod)
}



#########################################################################################################
#########################################################################################################


#' Make plot of variable contributions for reduced model
#'
#' @param model
#' @param responseName_brts
#' @param distrib
#'
#' @return
#' @export
#'

make_contribution_reduced_plot <- function(model, responseName_brts, distrib){
  
  contrib = summary(model)
  
  plot = ggplot(data = contrib, aes(x = reorder(var, rel.inf), y = rel.inf))+
    geom_bar(stat="identity", width = 0.5) +
    ylab("Relative contribution (%)") +
    xlab("") +
    coord_flip()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          axis.text=element_text(size=14))
  
  ggplot2::ggsave(paste0(deparse(substitute(model)),"_", responseName_brts,"_", distrib , ".png"), plot, width = 6, height = 7)
  write_xlsx(list(contrib = contrib),path=paste0("contrib_",deparse(substitute(model)),"_", responseName_brts, "_", distrib, ".xlsx"))
  
}

#########################################################################################################
#########################################################################################################


#' Partial dependance plots version 3
#'
#' @param model
#' @param responseName_brts
#' @param distrib
#'
#' @return
#' @export
#'

partial_dependance_plots3 <- function(model, responseName_brts, distrib){
  
  predictors = model$var.names
  
  if (length(predictors)>=7){
    width = 1200
    height = 1200
  }
  
  if (length(predictors)==5 | length(predictors)==6){
    width = 500
    height = 600
  }
  
  if (length(predictors)==3 | length(predictors)==4){
    width = 600
    height = 600
  }
  
  if (length(predictors)==2){
    width = 600
    height = 300
  }
  
  png(paste0("partial_plots3_", deparse(substitute(model)),"_", responseName_brts, "_", distrib, ".png"), width = width, height = height)
  
  
  if (length(predictors)>=7){
    par(mfrow=c(3,3))
  }
  
  if (length(predictors)==5 | length(predictors)==6){
    par(mfrow=c(3,2))
  }
  
  if (length(predictors)==3 | length(predictors)==4){
    par(mfrow=c(2,2))
  }
  
  if (length(predictors)==2){
    par(mfrow=c(1,2))
  }
  
  for (j in 1:length(predictors)){
    
    #extract fitted value for predictor
    a=gbm::plot.gbm(model, i.var=j, return.grid=T)
    
    #plot
    
    plot(a, ylab="Fish biomass", pch = 16, cex.axis=1.2, cex.lab=1.5, cex = 1)
    
    
    if (!predictors[j] %in%  c("Year", "Site", "Protection", "cyclone")){
      #Fit a Smoothing Spline
      smoothingSpline = smooth.spline(a[,1], a[,2], spar=0.55)
      #add to plot
      lines(smoothingSpline, col= "blue", lwd=3)
    }
    
  }
  
  dev.off()
  
}

