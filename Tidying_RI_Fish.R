

#Install Libraries


library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(measurements)
library(stringi)


#########################################################

############ Rhode Island Dept of Env Mgmt  #############

#########################################################

#in the raw data from Alan, the date column was offset by one (ie there is a blank cell in the first row for date only) I fixed this in the excel spreadsheet.


#read in sample data
ri <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Fish Data/jenrogers data request.xls",
                 skip = 56, col_names = FALSE)

#assign column names
names(ri) <- c("basin", "subbasin", "station_number", "date", "station_length", "station_width", "spp_code", "length_mm", 
               "total_num_spp_collected_per_station", "subsample", "total_num_specimens_for_each_spp_collected", 
               "gear", "efish_duration_s", "no_name", "waterbody_roadcrossing", "town", "latitude", "longitude")

ri$spp_code <- tolower(ri$spp_code)
ri$spp_code[ri$spp_code == "smb yoy"] <- "smb"
ri$spp_code[ri$spp_code == "ybull"] <- "ylbull"
ri$spp_code[ri$spp_code == "brktrt yoy"] <- "brktrt"


#assign a UID
ri$UID <- paste("RI", ri$basin, ri$subbasin, ri$station_number, ri$date, sep = "_")



code <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Fish Data/Species Codes.xls")
names(code) <- c("common_name", "scientific_name", "spp_code")
code$spp_code <- tolower(code$spp_code)



#left join the fish name abbreviation table so we have the common and scientific names
ri <- left_join(ri, code, by = "spp_code")
unique(ri$spp_code[is.na(ri$common_name)])   #need to assign scientific names to these
# "lepomis", "no fish", "shiner", "dace", "salmonid", "ictalurid sp"           
# "no fish collected", "alosids", "river herring", "unknown", "sunfish", "kingfish"               
# "summer flounder (fluke)", "silversides", "bullhead", "not flowing"       

#added scientific names to all except shiner and silversides because those are too general

ri$scientific_name[ri$spp_code == "lepomis"] <- "lepomis spp."
ri$scientific_name[ri$spp_code == "dace"] <- "cyprinidae"
ri$scientific_name[ri$spp_code == "salmonid"] <- "salmonidae"
ri$scientific_name[ri$spp_code == "ictalurid sp"] <- "ictaluridae"
ri$scientific_name[ri$spp_code == "alosids"] <- "alosa spp."
ri$scientific_name[ri$spp_code == "river herring"] <- "alosa spp."
ri$scientific_name[ri$spp_code == "sunfish"] <- "centrarchidae"
ri$scientific_name[ri$spp_code == "kingfish"] <- "scomberomorus cavalla"
ri$scientific_name[ri$spp_code == "summer flounder (fluke)"] <- "paralichthys dentatus"
ri$scientific_name[ri$spp_code == "bullhead"] <- "ictaluridae"
ri$scientific_name[ri$spp_code == "no fish"] <- "no fish"
ri$scientific_name[ri$spp_code == "unknown"] <- "unknown"



#prepare the 3 files:; ri_event, ri_fish, ri_method

#ri_event
ri_event <- ri %>% 
  select(UID, date, latitude, longitude) %>% 
  mutate(state = "RI", 
         source = "Allan Liby - RIDEM",
         project = "fish_community_RI_streams_Rivers",
         waterbody = "lotic", #reviewed the water bodies and all were river, brook, stream, or tributary. no lentic waterbodies listed
         date2 = round(date, 0),
         date3 = ifelse(date2 < 1000000, paste(19, date2, sep = ""), 
                        ifelse(date2>999999, sub(".", "20", date2), date2)),
         date4 = ymd(date3)) %>% 
  select(-date, -date2, -date3) %>% 
  rename(date = date4) %>% 
  select(UID, state, date, waterbody, latitude, longitude, project, source) %>% 
  unique()


#make all lat long decimal degrees
tmp <- ri_event %>% 
  filter(latitude <100) %>%  #these are the ones that already are in decimal degrees
  mutate(longitude = ifelse(longitude >0, -longitude, longitude)) #make the longitudes all negative

tmp2 <- ri_event %>% #these are the ones in degree min sec that need to be converted
  filter(latitude > 100)

tmp3 <- ri_event %>% 
  filter(is.na(latitude))

tmp2 <- tmp2 %>% 
  mutate(latitude2 = stri_sub_replace(latitude, 3, 2, value = " "), #to use the conv_unit, the dms  needs to have a space in between
         latitude3 = stri_sub_replace(latitude2, 6, 5, value = " "),
         longitude2 = stri_sub_replace(longitude, 3, 2, value = " "),
         longitude3 = stri_sub_replace(longitude2, 6, 5, value = " "))


tmp2$latitudefinal <- conv_unit(tmp2$latitude3, from = "deg_min_sec", to= "dec_deg") #convert to decimal degrees
tmp2$longitudefinal <- conv_unit(tmp2$longitude3, from = "deg_min_sec", to= "dec_deg") #convert to decimal degrees

tmp2 <- tmp2 %>% #keep and rename the final latitude and longitude columns
  select(UID, state, date, waterbody, latitudefinal, longitudefinal, project, source) %>% 
  rename(latitude = latitudefinal, 
         longitude = longitudefinal) %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         longitude = ifelse(longitude > 0, -longitude, longitude))

ri_event <- rbind(tmp, tmp2, tmp3) #rbind the converted coordinate df to the df that was orignally in dec degrees
rm(tmp, tmp2, tm3, code)

#fix longitude
ri_event$longitude[ri_event$UID == "RI_2_1_14_930723"] <- -71.79167 #this was a typo and was originally -41.79167
#fix latitude based on the cross street and the brook crossing
ri_event$latitude[ri_event$UID == "RI_4_4_5_980716"] <- 41.588950 #I got this from google mapoing Fisherville B./Pardon Joslin Rd.

#RI fish
ri_fish <- ri %>% 
  select(UID, 
         scientific_name, 
         total_num_specimens_for_each_spp_collected, #this is the count
         subsample,
         length_mm) %>% #fish measurement
  mutate(scientific_name = tolower(scientific_name)) %>% 
  rename(count = total_num_specimens_for_each_spp_collected) %>% 
  select(UID, scientific_name, length_mm, subsample, count) %>% 
  filter(scientific_name != "no fish")

ri_fish$subsample <- gsub("~", "", ri_fish$subsample) #remove the ~approx symbol 
ri_fish$subsample[is.na(as.numeric(ri_fish$subsample))] <- NA #turn the non numerical entires into NA
ri_fish$subsample <- as.numeric(ri_fish$subsample)
ri_fish$subsample[ri_fish$subsample == 0] <- NA


ri_fish$length_mm <- gsub("~", "", ri_fish$length_mm) #remove the ~approx symbol from the lengths, becuase exact measurement doesnt matter for us
ri_fish$length_mm[is.na(as.numeric(ri_fish$length_mm))] <- NA #turn the non numerical entires into NA
ri_fish$length_mm <- as.numeric(ri_fish$length_mm)
ri_fish$length_mm[ri_fish$length_mm == 0] <- NA

ri_fish$count <- gsub("~", "", ri_fish$count) #remove the ~approx symbol from the lengths, becuase exact measurement doesnt matter for us
ri_fish$count[ri_fish$count == "45 obs"] <- 45
ri_fish$count[is.na(as.numeric(ri_fish$count))] <- NA #turn the non numerical entires into NA 
ri_fish$count <- as.numeric(ri_fish$count)
ri_fish$count[ri_fish$count == 21579] <- NA



# in this dataset, they measure up to 30 fish, and then count the rest.we want to get it into a format of one row per indidividual - so for the 
#spp that had a greater count then the number measured, we need to replcate those rows with NA for the spp  length


tmp <- ri_fish %>% 
  group_by(UID, scientific_name) %>% 
  summarise(sample = n())
  
tmp2 <- ri_fish %>% 
  select(UID, scientific_name, count) %>% 
  unique() 

tmp3 <- left_join(tmp, tmp2, by = c("UID", "scientific_name")) %>% 
  mutate(notmeasured = count - sample) %>% #calculate how many rows need to be added to make the number of rows = to the count
  filter(notmeasured > 0) %>% #filter for the samples that had a higher count than the number measured
  select(UID, scientific_name, notmeasured, count) #select just the uid, name, and number of additional times we need to replicate the row
  
n <-  tmp3$notmeasured
tmp4 <- tmp3[rep(seq_len(nrow(tmp3)), n),] #replicate the rows

ri_fish <- bind_rows(ri_fish, tmp4) %>% #bind the replicated rows with no fish lenght information back into the ri_fish dataframe
  arrange(UID, scientific_name) %>% 
  select(-subsample, -notmeasured)
  
#thre were some rows where the count was not correct (the count was smallwer than the sample (so the count- sample was negative))
tmp5 <- ri_fish %>% 
  group_by(UID, scientific_name) %>% 
  summarise(countnew = n()) #calucate the count by summing the rows per fish spp per UID
ri_fish <- left_join(ri_fish, tmp5, by = c("UID", "scientific_name")) %>% 
  select(-count) %>%  #remove the original count
  rename(count = countnew) #rename the new count to the be count

ri_fish$run_num <- 1


#ri_method

#clean up the gear column
table(ri$gear)
ri$gear[ri$gear == "backpack shocker"] <- "efish_backpack"
ri$gear[ri$gear == "boat shocking"] <- "efish_boat"
ri$gear[ri$gear == "backpack"] <- "efish_backpack"
ri$gear[ri$gear == "boat shocker"] <- "efish_boat"
ri$gear[ri$gear == "backpack shocking"] <- "efish_backpack"
ri$gear[ri$gear == "boat schcking"] <- "efish_boat"
ri$gear[ri$gear == "boatshocker"] <- "efish_boat"
ri$gear[ri$gear == "backpack(2)"] <- "efish_backpack"
ri$gear[ri$gear == "boatshocking"] <- "efish_boat"
ri$gear[ri$gear == "backpackshocker"] <- "efish_backpack"
ri$gear[ri$gear == "backpack on boat"] <- "remove"
ri$gear[ri$gear == "trapnetting"] <- "trapnet"
ri$gear[ri$gear == "trap"] <- "trapnet"
ri$gear[ri$gear == "shocker"] <- "efish_backpack"
ri$gear[ri$gear == "shkr"] <- "efish_backpack"
ri$gear[ri$gear == "shocking"] <- "efish_backpack"
ri$gear[ri$gear == "shock"] <- "efish_backpack"
ri$gear[ri$gear == "dive"] <- "snorkel"
ri$gear[ri$gear == "observations"] <- "observed"
ri$gear[ri$gear == "seine,electro"] <- "efish_backpack/seine"
ri$gear[ri$gear == "seine/shock"] <- "efish_backpack/seine"

ri_method <- ri %>% 
  select(UID, 
         gear, #gear
         station_length, #reach length - alan confirmed
         station_width, #reach width - - alan confirmed
         efish_duration_s) %>%  #survey duration 
  filter(gear != "remove") %>%  #I removed the gear type backpack on boat bc alan said this was not an efficient survey
  rename(reach_length_m = station_length,
         avg_reach_width_m = station_width) %>% 
  mutate(target = NA,
         goal = "Total Pick Up", #Alan said they do all total pick up
         efish_runs = 1, #Alan said mostly do single pass
         daylight = ifelse(gear == "efish_boat", "Night", NA))%>%  
  select(UID, gear, goal, target, reach_length_m, avg_reach_width_m, efish_duration_s, efish_runs, daylight) %>% 
  unique()

ri_method$efish_duration_s <- gsub("~", "", ri_method$efish_duration_s) #remove the ~approx symbol from the lengths, becuase exact measurement doesnt matter for us
ri_method$efish_duration_s[is.na(as.numeric(ri_method$efish_duration_s))] <- NA #turn the non numerical entires into NA 
ri_method$efish_duration_s <- as.numeric(ri_method$efish_duration_s)




####################################

#save dataframe
save(ri_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ri_fish_method.RData")
save(ri_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ri_fish_event.RData")
save(ri_fish, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ri_fish_fish.RData")

