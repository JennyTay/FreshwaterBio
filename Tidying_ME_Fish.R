

#Tidying Me mussel data


library(tidyverse)
library(lubridate)
library(RODBC) 
library(measurements)
library(sf)

#read in data

#connect to the database
db <- "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/ME DIFW Fish Data/Rogers2022_MSFDBcopy.accdb"
con2 <- odbcConnectAccess2007(db)

#get a list of the table names
sqlTables(con2, tableType = "TABLE")$TABLE_NAME

#bring in 2 of the tables into R as dataframes that we want all of the field
counts <- sqlFetch(con2, "Groupfishdata")
backpack <- sqlFetch(con2, "backpack runs")

#bring in remainder of tables as qrys so that we can reduce the size of the table

#location data 
sqlColumns(con2, "LocationID")$COLUMN_NAME #get column names
qry1 <- "SELECT TopUTMX, TopUTMY, LOCATIONID FROM LocationID"
location_data <- sqlQuery(con2, qry1)

#fish_data 
sqlColumns(con2, "newIndividualfish")$COLUMN_NAME #get column names
qry2 <- "SELECT SampleID, `Gear Type`, RunNum, SPP, `Life Stage`, Units, Length, Weight, Origin, Age FROM newIndividualfish"
lengths_data <- sqlQuery(con2, qry2)

#species data 
sqlColumns(con2, "SPP")$COLUMN_NAME #get column names
qry3 <- "SELECT SPECIES, SPP, SCIENTIF FROM SPP"
species_data <- sqlQuery(con2, qry3)

#event data
sqlColumns(con2, "tblSamples")$COLUMN_NAME
qry4 <- "SELECT SampleID, LocationID, Surveyors, BegDate, LengthSection, AveWidth  FROM tblSamples"
event_data <- sqlQuery(con2, qry4)





# event data
me_event <- event_data %>% 
  select("SampleID", "BegDate", "LocationID") %>% 
  mutate(source = "MEDIFW - MerryGallagher",
         UID = paste("ME", SampleID, sep = "_"),
         project = "ME_DIFW",
         state = "ME",
         waterbody = "lotic") %>% #Merry said all are flowing water surveys
  rename(date = BegDate) %>% 
  select(UID, state, date, waterbody, project, source, LocationID) %>% 
  unique()

#need to get lat and long from the location data df
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")

shp <- st_as_sf(x = location_data[!is.na(location_data$TopUTMX),],                         
                coords = c("TopUTMX", "TopUTMY"),
                crs = "+proj=utm +zone=19 +datum=NAD83  +units=m")

shp <- st_transform(shp, st_crs(huc8))

shp <- shp %>% 
  mutate(longitude = unlist(map(geometry, 1)),
         latitude = unlist(map(geometry, 2))) %>% 
  filter(latitude > 43, #remove 2 rows with erroneous latitude
         longitude > -72) #remove 1 row with erroneous longitude

me_event <- left_join(me_event, shp, by  = c("LocationID" = "LOCATIONID")) %>% 
  data.frame() %>% 
  select(UID, state, latitude, longitude, date, waterbody, project, source) %>% 
  unique()


#### Make the count/occurrence file separate from the length file, that way we dont need to duplicate rows

dat <- counts %>% 
  select(SampleID, SPP, "Life Stage", Abundance, QualAbun, Origin) %>% 
  mutate(UID = paste("ME", SampleID, sep = "_"),
         occurrence = 1) %>% #even for ones where Abundance = 0, Merry said the fish was there, the 0 indicates no count was made 
  filter(Origin != "Hatchery" | is.na(Origin)) %>% 
  rename(count = Abundance)

dat <- left_join(dat, species_data, by = "SPP") %>%  
  mutate(common_name = tolower(SPECIES),
         scientific_name = tolower(SCIENTIF)) %>% 
  select(UID, common_name, occurrence, count, QualAbun)

dat$count[dat$count == 0] <- NA

#some of the common_names are not for fish (frog, salamander, pollywog, mudpuppy)
me_count <- dat %>% 
  filter(! common_name %in% c("frog", "salamander", "mudpuppy", "pollywog", "crayfish"))
  


dat_length <- lengths_data %>% 
  mutate(UID = paste("ME", SampleID, sep = "_")) %>% 
  rename(run_num = RunNum,
         length_mm = Length,
         weight_g = Weight) %>% 
  filter(Origin != "Hatchery" | is.na(Origin)) %>% 
  select(UID, SPP, length_mm, weight_g)

me_length <- left_join(dat_length, species_data, by = "SPP") %>%  
  mutate(common_name = tolower(SPECIES),
         scientific_name = tolower(SCIENTIF)) %>% 
  select(UID, common_name, length_mm, weight_g) %>% 
  filter(!is.na(length_mm)) %>% 
  filter(length_mm != 0)
me_length$weight_g[me_length$weight_g == -99] <- NA
me_length$weight_g[me_length$weight_g == 0] <- NA






me_method <- event_data %>% 
  select(SampleID, LengthSection, AveWidth) %>% 
  rename(reach_length_m = LengthSection, avg_reach_width_m = AveWidth) %>% 
  mutate(UID = paste("ME", SampleID, sep = "_"),
         goal = "Total Pick-up") %>%  #merry said all post 2000s (even most in the 90s were total pick up, not the older surveys)
  select(UID, goal, reach_length_m, avg_reach_width_m) %>% 
  unique()

#the runs and the duration come from other tables

runs1 <- backpack %>%
  mutate(UID = paste("ME", SampleID, sep = "_")) %>% 
  rename(efish_duration_s = `Wand Seconds`) %>% 
  group_by(UID) %>% 
  summarise(efish_duration_s = sum(efish_duration_s)) %>% 
  select(UID, efish_duration_s) %>%
  unique()
  

runs2 <- counts %>% 
  mutate(UID = paste("ME", SampleID, sep = "_")) %>% 
  rename(gear = `Gear Type`) %>% 
  filter(!is.na(gear)) %>% 
  select(UID, gear) %>% 
  unique() %>% 
  filter(gear != "b",
         gear != "Baited Trap",
         gear != "Experimental Angling")

runs2$gear[runs2$gear == "Backpack Electrofishingb"] <- "Backpack Electrofishing"


runs3 <- counts %>% 
  mutate(UID = paste("ME", SampleID, sep = "_")) %>% 
  rename(efish_runs = RunNum) %>% 
  filter(!is.na(efish_runs)) %>% 
  select(UID, efish_runs) %>% 
  group_by(UID) %>% 
  summarise(efish_runs = max(efish_runs)) %>% 
  unique()



runs <- left_join(runs2, runs1, by = "UID") %>% 
  select(UID, gear, efish_duration_s) %>% #the efish_runs do not align perfectly between tables, but merry said to use the one from the group fish data
  unique()

runs <- left_join(runs, runs3, by = "UID") %>% 
  unique()

me_method <- left_join(me_method, runs, by = "UID") %>% 
  unique()

#any method attribute that is 0 shoudl be NA - Merry said they record 0 if its not quanitified
me_method$reach_length_m[me_method$reach_length_m == 0] <- NA
me_method$avg_reach_width_m[me_method$avg_reach_width_m == 0] <- NA
me_method$efish_duration_s[me_method$efish_duration_s == 0] <- NA
me_method$efish_runs[me_method$efish_runs == 0] <- NA

####################################
#save dataframe
save(me_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/me_method_method.RData")
save(me_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/me_event_event.RData")
save(me_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/me_count_fish.RData")
save(me_length, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/me_length_fish.RData")

