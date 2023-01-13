library(RODBC)    
library(tidyverse)
library(lubridate)
library(sf)
library(readxl)


#####################################################

################### Massachusetts ###################

#####################################################


#connect to the database
db <- "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA DFW Fish Data/Fisheries Survey and Inventory Database.accdb"
con2 <- odbcConnectAccess2007(db)

#get a list of the table names
sqlTables(con2, tableType = "TABLE")$TABLE_NAME

#bring in 3 of the tables into R as dataframes
gillnet_data <- sqlFetch(con2, "gillnet_data")
seine_data <- sqlFetch(con2, "seine_data")
water_chem <- sqlFetch(con2, "water_chemistry")

#bring in remainder of tables as qrys so that we can reduce the size of the table

#make a query for the sample data df so that when we bind it with the others it's not such a large file
qry1 <- "SELECT sample_id, saris_palis, sample_date, sample_type, method, agency, latitude, longitude FROM sample_data"
sample_data <- sqlQuery(con2, qry1)

#make a query for the fish_data to reduce object size
qry2 <- "SELECT run_num, sample_id, fish_code, length, weight FROM fish_data"
fish_data <- sqlQuery(con2, qry2)

#make a query for the species data to reduce object size
qry3 <- "SELECT fish_code, common_name, family, scientific_name, origin, pt, temp, function, River_size, listed_status FROM species_data"
species_data <- sqlQuery(con2, qry3)

#make a query for the waterbody data
qry4 <- "SELECT saris_palis, watershed, waterbody, town, FROM waterbody_data"
waterbody_data <- sqlQuery(con2, qry4)

#make a query for the backpack_data to reduce size
qry5 <- "SELECT sample_id, number_of_passes, seconds_pass_1, seconds_pass_2, seconds_pass_3,
total_seconds, reach_length, reach_avg_width, reach_avg_depth, reach_max_depth, percent_reach_sampled, number_backpacks
FROM backpack_data"
backpack_data <- sqlQuery(con2, qry5)

#make a query for the boat_barge_data to reduce size
qry6 <- "SELECT sample_id, seconds, reach_length, reach_avg_width, reach_avg_depth, 
reach_max_depth,  percent_reach_sampled, sample_period, number_of_runs FROM boat_barge_data"
boat_data <- sqlQuery(con2, qry6)




#prepare tables that will be joined with tables in other states


#prepare sampling event df
names(sample_data)

ma_event <- sample_data %>% 
  select(sample_id, saris_palis, sample_date, latitude, longitude, agency) %>% 
  mutate(state = "MA",
         source = "MassWildlife - JasonStolarski",
         UID = paste("MA", sample_id, sep = "_"),
         waterbody = ifelse(saris_palis > 99999, "lotic", "lentic")) %>% 
  rename(project = agency,
         date = sample_date) %>% 
  select(UID, state, date, waterbody, latitude, longitude, project, source, -sample_id)

#prepare fish df. I am goin to use scientific name for each state, and then I'll go back and create an ID because the fish codes differ by state.
tmp <- left_join(fish_data, species_data, by = "fish_code")
head(tmp)

ma_fish <- tmp %>% 
  select(sample_id, scientific_name, run_num, length, weight) %>% 
  mutate(UID = paste("MA", sample_id, sep = "_")) %>% 
  select(-sample_id) %>%  
  rename(length_mm = length, weight_g = weight) %>% 
  select(UID, scientific_name, length_mm, weight_g, run_num)
rm(tmp)

#then create a count field that is the sum of the spp observed per site and per run.
tmp <- ma_fish %>% 
  group_by(UID, scientific_name, run_num) %>% 
  summarise(count = n()) %>% 
  ungroup() 

ma_fish <- left_join(ma_fish, tmp, by = c("UID", "scientific_name", "run_num"))

#identify stocked fish to exclude. this is a combination of there being less than 5 trout in a sample and the trout being >200mm
# stocked <- ma_fish %>% 
#   filter(grepl("Oncorhynchus mykiss|Salvelinus fontinalis|Salmo trutta", scientific_name)) %>% 
#   filter(count < 2)

#prepare species df
ma_species <- species_data %>% 
  select(common_name, scientific_name, origin, temp, pt, "function", River_size) %>% 
  rename(temperature_preference = temp, 
         tolerance = pt,
         eco_function = "function",
         stream_preference = River_size)
names(ma_species)
head(ma_species)



#prepare method df


#there are many of the same columns in the boat_data and backpack_data so we will need to combine these

tmp <- left_join(sample_data, backpack_data, by = "sample_id")
tmp <- left_join(tmp, boat_data, by = "sample_id")
tmp <- left_join(tmp, seine_data, by = "sample_id")
tmp <- left_join(tmp, gillnet_data, by = "sample_id")

tmp$reach_length <- ifelse(is.na(tmp$reach_length.x), tmp$reach_length.y, tmp$reach_length.x)
tmp$reach_avg_depth <- ifelse(is.na(tmp$reach_avg_depth.x), tmp$reach_avg_depth.x, tmp$reach_avg_depth.x)
tmp$reach_avg_width <- ifelse(is.na(tmp$reach_avg_width.x), tmp$reach_avg_width.y, tmp$reach_avg_width.x)
tmp$reach_max_depth <- ifelse(is.na(tmp$reach_max_depth.x), tmp$reach_max_depth.y, tmp$reach_max_depth.x)
tmp$percent_reach_sampled <- ifelse(is.na(tmp$percent_reach_sampled.x), tmp$percent_reach_sampled.y, tmp$percent_reach_sampled.x)
tmp$sample_period <- ifelse(is.na(tmp$sample_period.x), tmp$sample_period.y, tmp$sample_period.x)
tmp$total_seconds <- ifelse(is.na(tmp$total_seconds), tmp$seconds, tmp$total_seconds)
tmp$number_of_passes <- ifelse(is.na(tmp$number_of_passes), tmp$number_of_runs, tmp$number_of_passes)

ma_method <- tmp %>% 
   rename(reach_length_m = reach_length, avg_reach_width_m = reach_avg_width,
         goal = sample_type, gear = method, efish_runs = number_of_passes,
         efish_duration_s = total_seconds, daylight = sample_period) %>% 
  mutate(UID = paste("MA", sample_id, sep = "_")) %>% 
  select(UID, gear, goal, reach_length_m, avg_reach_width_m, efish_runs, efish_duration_s, daylight)


rm(tmp)


####################################
#save dataframe
save(ma_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ma_fish_method.RData")
save(ma_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ma_fish_event.RData")
save(ma_fish, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ma_fish_fish.RData")
save(ma_species, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ma_fish_species.RData")


#added in stocking information Dec 23,2022
#atlantic salmon are all stocked and only naturally reproduce in tribs of the quabbin and wachusetts reservoirs
#for brown trout and brook trout, mark any larger than 200mm as stocked
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/ma_fish_fish.RData")
ma_fish$stock <- ifelse(ma_fish$scientific_name %in% c("Salmo trutta", "Salvelinus fontinalis", "Oncorhynchus mykiss") & 
                          ma_fish$length_mm >200, "stock", "natural")

