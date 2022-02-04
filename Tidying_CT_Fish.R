#########################################################

################### Connecticut ABM  ###################

#########################################################

#read in sample data
ct <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/CT EEP Fish Data/ABM_FishData_2016_2021_012422.xlsx",
                 col_names = TRUE)
str(ct)
hist(as.numeric(ct$ReachLengthMeasureValue))