
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




#prepare the 4 files:; ct_event, ct_fish, ct_method, ct_species

#ct_event
ct_event <- ct %>% 
  select(ActivityIdentifier, ActivityStartDate, ylat, xlong, ProjectIdentifier) %>% 
  mutate(state = "CT", 
         data_source = "ChrisBellucci - CTDEEP",
         UID = paste("CT", ActivityIdentifier, sep = "_")) %>% 
  rename(date = ActivityStartDate, 
         latitude = ylat, 
         longitude = xlong, 
         project = ProjectIdentifier) %>% 
  select(UID, state, date, latitude, longitude, project, data_source,
         -ActivityIdentifier, )

ct_fish <- ct %>% 
  select(ActivityIdentifier, SubjectTaxonomicName, ResultMeasureValue, FrequencyClassDescriptorUnitCode, LowerClassBoundValue, Pass) %>% 
  rename(scientific_name = SubjectTaxonomicName, 
         count = ResultMeasureValue, 
         length = LowerClassBoundValue, 
         run_num = Pass) %>% 
  mutate(length = as.numeric(length),
         length_mm = ifelse(FrequencyClassDescriptorUnitCode == "cm", length*10, length),
         UID = paste("CT", ActivityIdentifier, by = "_"),
         count = as.numeric(count)) %>% 
  select(-length, - FrequencyClassDescriptorUnitCode)

ct_fish$length_mm[ct_fish$length_mm == -990] <- NA #some were recorded as -99 (-990 mm), which means no length was measured, so I made these NA


ct_methods <- ct %>% 
  select(ActivityIdentifier, SampleCollectionEquipmentName, 
         SampleCollectionMethodIdentifier, 
         ReachLengthMeasureValue, ReachLengthMeasureUnitCode, 
         ReachWidthMeasureValue, ReachWidthMeasureUnitCode,
         CollectionDurationMeasureValue, CollectionDurationMeasureUnitCode) %>% 
  separate(SampleCollectionMethodIdentifier, into = c("efish_run_num", "goal"), sep = 9) %>% 
  mutate(UID = paste("CT", ActivityIdentifier, sep = "_"),
         target = NA,
         reach_length_m = ifelse(ReachLengthMeasureUnitCode == "m", 
                                 ReachLengthMeasureValue, NA),
         reach_width_m = ifelse(ReachWidthMeasureUnitCode == "m", 
                                ReachWidthMeasureValue, NA),
         efish_duration = as.numeric(CollectionDurationMeasureValue),
         efish_duration_s = ifelse(CollectionDurationMeasureUnitCode == "min", 
                                   efish_duration*60, NA),
         goal = ifelse(goal == "AllSp", "Total Pick Up", "Selective Pick Up"),
         efish_run_num = ifelse(efish_run_num == "fish1pass", 1 , efish_run_num),
         SampleCollectionEquipmentName = ifelse(SampleCollectionEquipmentName == "Backpack Electroshock", "backpack", SampleCollectionEquipmentName))%>% 
  rename(gear = SampleCollectionEquipmentName) %>% 
  select(-c(ActivityIdentifier, ReachLengthMeasureValue,
            ReachLengthMeasureUnitCode, ReachWidthMeasureValue, ReachWidthMeasureUnitCode,
            CollectionDurationMeasureValue, CollectionDurationMeasureUnitCode,
            efish_duration))

