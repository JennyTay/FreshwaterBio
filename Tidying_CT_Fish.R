

#Install Libraries


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
         source = "ChrisBellucci - CTDEEP",
         UID = paste("CT", ActivityIdentifier, sep = "_")) %>% 
  rename(date = ActivityStartDate, 
         latitude = ylat, 
         longitude = xlong, 
         project = ProjectIdentifier) %>% 
  select(UID, state, date, latitude, longitude, project, source,
         -ActivityIdentifier, ) %>% 
  unique()

ct_fish <- ct %>% 
  select(ActivityIdentifier, 
         SubjectTaxonomicName, 
         ResultMeasureValue, #this is the count
         FrequencyClassDescriptorUnitCode,  #this is the unit of the length measurement
         LowerClassBoundValue, #the upper and lower class bound values are equal
         Pass) %>% 
  rename(scientific_name = SubjectTaxonomicName, 
         count = ResultMeasureValue, 
         length = LowerClassBoundValue, 
         run_num = Pass) %>% 
  mutate(length = as.numeric(length),
         length_mm = ifelse(FrequencyClassDescriptorUnitCode == "cm", length*10, length),
         UID = paste("CT", ActivityIdentifier, by = "_"),
         count = as.numeric(count)) %>% 
  select(UID, scientific_name, length_mm, count, run_num)

ct_fish$length_mm[ct_fish$length_mm == -990] <- NA #some were recorded as -99 (-990 mm), which means no length was measured, so I made these NA
# in this dataset, count refers to the number of each spp in a certain length class. This is differnt than the other datasets when count is the total number of spp.
#to make comprable to other datasets, repeat rows with lengths the number of times based on the count value. 
#then create a count field that is the sum of the spp observed per site.

n <-  ct_fish$count
ct_fish <- ct_fish[rep(seq_len(nrow(ct_fish)), n),]
tmp <- ct_fish %>% 
  group_by(UID, scientific_name, run_num) %>% 
  summarise(countnew = n()) %>% 
  ungroup() 

ct_fish <- ct_fish %>% 
  left_join(tmp, by = c("UID", "scientific_name", "run_num")) %>% 
  select(-count) %>% 
  rename(count = countnew) %>% 
  mutate(scientific_name = tolower(scientific_name))

#need to get the common names. Chris didnt give a look up file, so I will use the MA one
unique(ct_fish$scientific_name)

ct_method <- ct %>% 
  select(ActivityIdentifier, 
         SampleCollectionEquipmentName, #gear
         SampleCollectionMethodIdentifier,  #of passes and target vs all
         ReachLengthMeasureValue, #reach length
         ReachLengthMeasureUnitCode,  #reach length unit
         ReachWidthMeasureValue, #reach width
         ReachWidthMeasureUnitCode, #reach width unit
         CollectionDurationMeasureValue, #survey duration
         CollectionDurationMeasureUnitCode) %>% #survey duration unit 
  separate(SampleCollectionMethodIdentifier, into = c("efish_runs", "goal"), sep = 9) %>% 
  mutate(UID = paste("CT", ActivityIdentifier, sep = "_"),
         target = NA,
         reach_length_m = as.numeric(ifelse(ReachLengthMeasureUnitCode == "m",  #in this dataframe, all are 'm' but i did this in case in the futre we read in data with other units
                                 ReachLengthMeasureValue, NA)),
         avg_reach_width_m = as.numeric(ifelse(ReachWidthMeasureUnitCode == "m", #in this dataframe, all are 'm' but i did this in case in the futre we read in data with other units
                                ReachWidthMeasureValue, NA)),
         efish_duration = as.numeric(CollectionDurationMeasureValue),
         efish_duration_s = ifelse(CollectionDurationMeasureUnitCode == "min", #in this dataframe, all are 'm' but i did this in case in the futre we read in data with other units
                                   efish_duration*60, NA),
         goal = ifelse(goal == "AllSp", "Total Pick Up", "Selective Pick Up"),
         efish_runs = ifelse(efish_runs == "fish1pass", 1 , efish_runs),
         SampleCollectionEquipmentName = ifelse(SampleCollectionEquipmentName == "Backpack Electroshock", "backpack", SampleCollectionEquipmentName))%>% 
  rename(gear = SampleCollectionEquipmentName) %>% 
  select(UID, gear, goal, target, reach_length_m, avg_reach_width_m, efish_duration_s, efish_runs)



####################################

#save dataframe
save(ct_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ct_fish_method.RData")
save(ct_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ct_fish_event.RData")
save(ct_fish, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ct_fish_fish.RData")

