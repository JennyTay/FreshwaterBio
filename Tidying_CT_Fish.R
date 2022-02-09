
library(RODBC)    
library(tidyverse)
library(lubridate)
library(sf)
library(readxl)


#########################################################

################### Connecticut ABM  ###################

#########################################################

#read in sample data
ct <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/CT EEP Fish Data/ABM_FishData_2016_2021_012422.xlsx",
                 col_names = TRUE)
str(ct)
hist(as.numeric(ct$ReachLengthMeasureValue))

#make shapefile the same crs as NHD
#load NHD data
MAflowline <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NDH/Shape/NHDFlowline.shp")


#make the fish dataframe an sf object so it can be plotted spatially
shp <- st_as_sf(x = ct,                         
                 coords = c("xlong", "ylat"),
                 crs = st_crs(MAflowline))
st_write(shp, dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/CT EEP Fish Data/ABM_FishData_2016_2021_012422.shp")
