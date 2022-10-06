
library(readxl)
library(caret)


#streamCat data tidying


#load the NE COMIDs so we can filter out the COMIDs that are outside of our region
comids <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")
comids <- comids$COMID

#create initial file


file.list <- list.files("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat", pattern='Region01')


streamcat1 <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat/BFI_Region01.csv")

#first start with the region 1's and join those together based on the COMID

for (i in 2:9) { #start at 2 because we already made the first file above)
  
  
  tmp <- read.csv(paste("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat/", file.list[i], sep = ""))
  
  tmp <- tmp %>% 
    select(-c(2,3,4,5))

  
  streamcat1 <- full_join(streamcat1, tmp, by = "COMID") %>% 
    filter(COMID %in% comids)
  
}




#region 2's and join those together based on the COMID

file.list <- list.files("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat", pattern='Region02')
streamcat2 <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat/BFI_Region02.csv")


for (i in 2:9) { #start at 2 because we already made the first file above)
  
  
  tmp <- read.csv(paste("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StreamCat/", file.list[i], sep = ""))
  
  tmp <- tmp %>% 
    select(-c(2,3,4,5))
  
  streamcat2 <- full_join(streamcat2, tmp, by = "COMID") %>% 
    filter(COMID %in% comids)
  
  
}


streamcat <- rbind(streamcat1, streamcat2)
colSums(is.na(streamcat))


str(streamcat)

nzv <- nearZeroVar(streamcat, saveMetrics = T)


