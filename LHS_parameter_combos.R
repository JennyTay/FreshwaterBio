# This code was written by: G. V. DiRenzo
# If you have any questions, please send them to: grace.direnzo@gmail.com


# Code Objective:
  # To use a Latin hyper cube sampler to evenly sample distributions for parameter combinations used in simulations





##################################
###### Table of Contents #########
##################################

# 1. Set working directory & Load libraries
# 2. Format the parameter estimates
# 3. Set up the information for the simulation 
# 4. Set up storage
# 5. Indicate MCMC settings 
# 6. Save the parameters=
# 7. Create empty arrays to save outputs 
# 8. Simulate the data
# 9. Bundle everything for the model run 
# 10. Run the models 
# 11. Save the model output
# 12. Process the output 
# 13. Save one file with all the relevant information


##################################
##################################
##################################



# 1. Load libraries ---------------------------------------------------------------


library(lhs)

### Writing simulation parameters to tab delimited text file so that a BASH 
## script can read this file and set up jobs on cluster






# 2. Set up information for sampler ---------------------------------------------------------------



# Number of samples in the parameter space
n_samples <- 10000


# Total number of parameters
n_params <- 15
  



# 3. Initialize sampler ---------------------------------------------------------------


lhs_raw <- randomLHS(n = n_samples, # Number of rows / samples
                     k = n_params)  # Number of columns or parameter variables

head(lhs_raw)

# Initialize data frame



param_combos <- setNames(data.frame(matrix(ncol = n_params, nrow = n_samples)),
                         c("annual_mean_summer_temp", 
                           "BFI_HIST",  
                           "LO7Q1DT_HIST",
                           "W95_HIST", 
                           "BFIWs", 
                           "WtDepWs", 
                           "PctOw_Ws", 
                           "PctImp_Ws", 
                           "pctAg_Ws",
                           "pctWetland_Cat", 
                           "logMJJA_HIST",
                           "logRdCrsCat", 
                           "logPctOw_Cat",
                           "huc12_damden_sqkm",
                           "huc8_damcount"))

head(param_combos)



# 4. Use LHS  ---------------------------------------------------------------



#get the range of each variable after it's been scaled
# load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_covariates_2080.RData")
test <- NHDplusV2_NewEngCrop_2080 %>% 
  data.frame() %>% 
  select(7:8, 10:39) %>% 
  pivot_longer(1:32, names_to = "variable", values_to = "value") %>% 
  group_by(variable) %>%
  summarise(min = round(min(value, na.rm = T),2),
            max = round(max(value, na.rm = T),2))
  



# 
# fishcovariates <- fishcovariates[complete.cases(fishcovariates),]
# test <- fishcovariates[,c(2:3, 9:38)]
# test <- data.frame(scale(test))
# 
# test2 <- test %>% 
#   pivot_longer(1:32, names_to = "variable", values_to = "value") %>% 
#   group_by(variable) %>% 
#   summarise(min = min(value),
#             max = max(value))
# write.csv(test2, ("tmpfigures/LHSparameters.csv"))


# Set variables that will change

param_combos$annual_mean_summer_temp <- qunif(lhs_raw[, 1], min = test$min[test$variable == "annual_mean_summer_temp_pls_4"], 
                                              max = test$max[test$variable == "annual_mean_summer_temp_pls_4"])

param_combos$BFI_HIST <- qunif(lhs_raw[, 2], min = test$min[test$variable == "BFI_2080"], 
                               max = test$max[test$variable == "BFI_2080"])


param_combos$LO7Q1DT_HIST <- qunif(lhs_raw[, 3], min = test$min[test$variable == "LO7Q1DT_2080"], 
                                   max = test$max[test$variable == "LO7Q1DT_2080"])

param_combos$W95_HIST <- qunif(lhs_raw[, 4], min = test$min[test$variable == "W95_2080"], 
                               max = test$max[test$variable == "W95_2080"])

param_combos$BFIWs <- qunif(lhs_raw[, 5], min = test$min[test$variable == "BFIWs"], 
                            max = test$max[test$variable == "BFIWs"])

param_combos$WtDepWs <- qunif(lhs_raw[, 6], min = test$min[test$variable == "WtDepWs"], 
                              max = test$max[test$variable == "WtDepWs"])

param_combos$PctOw_Ws <- qunif(lhs_raw[, 7], min = test$min[test$variable == "PctOw_Ws"], 
                               max = test$max[test$variable == "PctOw_Ws"])

param_combos$PctImp_Ws <- qunif(lhs_raw[, 8], min = test$min[test$variable == "PctImp_Ws"], 
                                max = test$max[test$variable == "PctImp_Ws"])

param_combos$pctAg_Ws <- qunif(lhs_raw[, 9], min = test$min[test$variable == "pctAg_Ws"], 
                               max = test$max[test$variable == "pctAg_Ws"])

param_combos$pctWetland_Cat <- qunif(lhs_raw[, 10], min = test$min[test$variable == "pctWetland_Cat"], 
                                     max = test$max[test$variable == "pctWetland_Cat"])

param_combos$logMJJA_HIST <- qunif(lhs_raw[, 11], min = test$min[test$variable == "logMJJA_2080"], 
                                   max = test$max[test$variable == "logMJJA_2080"])

param_combos$logRdCrsCat <- qunif(lhs_raw[, 12], min = test$min[test$variable == "logRdCrsCat"], 
                                  max = test$max[test$variable == "logRdCrsCat"])

param_combos$logPctOw_Cat <- qunif(lhs_raw[, 13], min = test$min[test$variable == "logPctOw_Cat"], 
                                   max = test$max[test$variable == "logPctOw_Cat"])

param_combos$huc12_damden_sqkm <- qunif(lhs_raw[, 14], min = test$min[test$variable == "huc12_damden_sqkm"], 
                                        max = test$max[test$variable == "huc12_damden_sqkm"])

param_combos$huc8_damcount <- qunif(lhs_raw[, 14], min = test$min[test$variable == "huc8_damcount"], 
                                    max = test$max[test$variable == "huc8_damcount"])


param_combos <- param_combos[rep(seq_len(nrow(param_combos)), each = 10), ]

#some variables dont need to vary randomly because they are not management or climate affected (lat, long, elevation, watershed area)\
#for these, we will create 16 scenarios (high lat, high long, high elev, large area, etc etc using the quantiles of each of these four parameters)

df <- data.frame(
  "lat" = rep(quantile(NHDplusV2_NewEngCrop_2080$lat, na.rm = T, probs = c(.05, .25, .5, .75, .95)), 2),
  "long" = rep(-71.27087, 10),
  "ElevCat" = rep(c(85.39257, 324.99975), each = 5),
  "logWsAreaSqKm" = rep(c(1.2259128, 3.2559038), each = 5))
df <- df[rep(seq_len(nrow(df)), times = 10000), ]


param_combos <- cbind(df, param_combos)


head(param_combos)

#need to scale the file using the means and sd of the original data.
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fishcovariates_scaling_mean.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fishcovariates_scaling_sd.RData")


names(means)
names(sd)
names(param_combos)
means <- means[names(param_combos)]
sd <- sd[names(param_combos)]

param_combos <- param_combos %>% 
  select(names(means))

param_combos <- sweep(param_combos, 2, means, FUN = '-') 
param_combos <- sweep(param_combos, 2, sd, FUN = '/')  

head(param_combos)

# 5. Write file  ---------------------------------------------------------------



save(param_combos, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/param_combos.RData")

# End Script