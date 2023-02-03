
library(readxl)
library(caret)
library(tidyverse)


#streamCat data tidying





#####################################################################

############# This first part tidies streamcat data for the fish model

#####################################################################




#load the NE COMIDs so we can filter out the COMIDs that are outside of our region
comids <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")
comids <- comids$COMID

#create initial file


file.list <- list.files("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat", pattern='Region01')


streamcat1 <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat/BFI_Region01.csv")

#first start with the region 1's and join those together based on the COMID

for (i in 2:14) { #start at 2 because we already made the first file above)
  
  
  tmp <- read.csv(paste("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat/", file.list[i], sep = ""))
  
  tmp <- tmp %>% 
    select(-c(2,3,4,5))

  
  streamcat1 <- full_join(streamcat1, tmp, by = "COMID") %>% 
    filter(COMID %in% comids,
           WsPctFull>85)
  
}




#region 2's and join those together based on the COMID

file.list <- list.files("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat", pattern='Region02')
streamcat2 <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat/BFI_Region02.csv")


for (i in 2:14) { #start at 2 because we already made the first file above)
  
  
  tmp <- read.csv(paste("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat/", file.list[i], sep = ""))
  
  tmp <- tmp %>% 
    select(-c(2,3,4,5))
  
  streamcat2 <- full_join(streamcat2, tmp, by = "COMID") %>% 
    filter(COMID %in% comids,
           WsPctFull>85)
  
  
}


streamcat <- rbind(streamcat1, streamcat2)
colSums(is.na(streamcat))


str(streamcat)




#separate the streamcat dataset into variables that will be joined by by comid and variables that will be join by COMID and year

#variables joined only by COMID
strmcat_byCOMID <- streamcat %>% 
  select(COMID, !contains("20")) %>% 
  select(-c(OmWs, PermWs, RckDepWs, # remove mean organic matter content of soil, mean permeablity of soils, mean depth to bedrock
            OmCat, PermCat, RckDepCat,
            WsPctFull, CatPctFull, CatAreaSqKm,
            ))
save(strmcat_byCOMID, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcat_byCOMID.RData")


#variables joined by COMID and timeperiod most likely
strmcatByyear <- streamcat %>% 
  select(COMID, contains("20")) %>% 
  pivot_longer(2:133, names_to = "variable") %>% 
  separate(variable, into = c("variable", "test2"), sep = "2") %>% 
  separate(test2, into = c("year", "region"), sep = 3) %>% 
  mutate(year = paste("2", year, sep = "")) %>% 
  unite(col = "variable", variable, region, sep = "_") %>% 
  pivot_wider(names_from = "variable", values_from = "value")

#not every variable has every year, so we will split the data into various groups

strmcatByyr_imp <- strmcatByyear %>% 
  select(1:6) %>% 
  filter(!is.na(PctImp_Ws)) 
save(strmcatByyr_imp, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_imp.RData")

strmcatByyr_landcov <- strmcatByyear %>% 
  select(1,2, 7:38) %>% 
  filter(!is.na(PctOw_Ws))
save(strmcatByyr_landcov, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_landcov.RData")


strmcatByyr_census <- strmcatByyear %>% 
  select(1,2, 39:42) %>% 
  filter(!is.na(PopDen_Ws))
save(strmcatByyr_census, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_census.RData")










#####################################################################

############# This second part tidies streamcat data for the fish model

#####################################################################




#load the NE COMIDs so we can filter out the COMIDs that are outside of our region
comids <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")
comids <- comids$COMID

#create initial file


file.list <- list.files("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/streamCat_musselCov", pattern='Region01')


streamcat1 <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/streamCat_musselCov/AgriculturalNitrogen_Region01.csv")

#first start with the region 1's and join those together based on the COMID

for (i in 2:14) { #start at 2 because we already made the first file above)
  
  
  tmp <- read.csv(paste("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/streamCat_musselCov/", file.list[i], sep = ""))
  
  tmp <- tmp %>% 
    select(-c(2,3,4,5))
  
  
  streamcat1 <- full_join(streamcat1, tmp, by = "COMID") %>% 
    filter(COMID %in% comids,
           WsPctFull>85)
  
}




#region 2's and join those together based on the COMID

file.list <- list.files("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/streamCat_musselCov", pattern='Region02')
streamcat2 <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/streamCat_musselCov/AgriculturalNitrogen_Region02.csv")


for (i in 2:14) { #start at 2 because we already made the first file above)
  
  
  tmp <- read.csv(paste("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/streamCat_musselCov/", file.list[i], sep = ""))
  
  tmp <- tmp %>% 
    select(-c(2,3,4,5))
  
  streamcat2 <- full_join(streamcat2, tmp, by = "COMID") %>% 
    filter(COMID %in% comids,
           WsPctFull>85)
  
  
}


streamcat <- rbind(streamcat1, streamcat2)
colSums(is.na(streamcat))


str(streamcat)

