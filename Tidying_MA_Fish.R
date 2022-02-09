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

#bring in 4 of the tables into R as dataframes: sample_data, fish_data, species_data, waterbody_data
sample_data <- sqlFetch(con2, "sample_data")
fish_data <- sqlFetch(con2, "fish_data")
species_data <- sqlFetch(con2, "species_data")
backpack_data <- sqlFetch(con2, "backpack_data")
gillnet_data <- sqlFetch(con2, "gillnet_data")
seine_data <- sqlFetch(con2, "seine_data")
water_chem <- sqlFetch(con2, "water_chemistry")
waterbody_data <- sqlFetch(con2, "waterbody_data")
boat_data <- sqlFetch(con2, "boat_barge_data")

head(sample_data)
str(sample_data)
names(sample_data)

mthod <- sample_data %>% 
  filter(!is.na(method)) %>% 
  filter(method != "Water Quality")
ggplot(data = mthod)+
  stat_count(mapping = aes(x = method))+
  theme(axis.text.x = element_text(angle = 90))
  


#make a query for the sample data df so that when we bind it with the others it's not such a large file
qry1 <- "SELECT sample_id, saris_palis, sample_date, sample_type, method, agency, snapped_latitude, snapped_longitude FROM sample_data"
sample_data <- sqlQuery(con2, qry1)

#make a query for the fish_data to reduce object size
names(fish_data)
qry2 <- "SELECT sample_id, fish_code, length, weight FROM fish_data"
fish_data <- sqlQuery(con2, qry2)

#make a query for the species data to reduce object size
names(species_data)
qry3 <- "SELECT fish_code, common_name, family, scientific_name, origin, pt, temp, function, River_size, listed_status FROM species_data"
species_data <- sqlQuery(con2, qry3)

#make a query for the waterbody data
names(waterbody_data)
qry4 <- "SELECT saris_palis, watershed, waterbody, town, FROM waterbody_data"
waterbody_data <- sqlQuery(con2, qry4)

#make a query for the backpack_data to reduce size
names(backpack_data)
qry5 <- "SELECT sample_id, number_of_passes, seconds_pass_1, seconds_pass_2, seconds_pass_3,
total_seconds, reach_length, reach_avg_width, reach_avg_depth, reach_max_depth, percent_reach_sampled, amps, volts, pf, pw, number_backpacks
FROM backpack_data"
backpack_data <- sqlQuery(con2, qry5)

#make a query for the boat_barge_data to reduce size
names(boat_data)
qry6 <- "SELECT sample_id, hull_type, gpp_size, seconds, volts, amps, range, percent_of_range, pps_and_mode, reach_length, reach_avg_width, reach_avg_depth, 
reach_max_depth,  percent_reach_sampled, sample_period, number_of_runs FROM boat_barge_data"
boat_data <- sqlQuery(con2, qry6)


#join data tables

dat <- left_join(fish_data, sample_data, by = "sample_id")
dat <- left_join(dat, species_data, by = "fish_code")
dat <- left_join(dat, waterbody_data, by = "saris_palis")

head(dat)
names(dat)
str(dat)

fish <- dat %>% 
  select(sample_id, sample_date, saris_palis, watershed, agency, fish_code, common_name, 
         scientific_name, length, family, method, run_num, origin, 
         pt, temp, listed_status, waterbody, town, 
         snapped_latitude, snapped_longitude) %>% 
  mutate(year = year(sample_date),
         month = month(sample_date)) %>% 
  filter(!is.na(snapped_longitude)) %>% 
  group_by(sample_id, common_name) %>% 
  mutate(count = n(),
         common_name = ifelse(common_name == "Hybrid Redfin/Chain Pickerel", "Hybrid Redfin_Chain Pickerel",
                              ifelse(common_name == "Green Sunfish X Red Breasted Sunfish Hybrid", "Hybrid Green Sunfish_Red Breasted Sunfish",
                                     ifelse(common_name == "Pumpkinseed X Green Sunfish", "Hybrid Pumpkinseed_Green Sunfish",
                                            ifelse(common_name == "Hybrid Bluegill/Pumpkinseed", "Hybrid Bluegill_Pumpkinseed",
                                                   ifelse(common_name == "Pumpkinseed X Red Breasted Sunfish Hybrid", "Hybrid Pumpkinseed_Red Breasted Sunfish",
                                                          ifelse(common_name == "Tiger Trout (Hybrid Brook Trout/Brown Trout)", "Hybrid Brook Trout_Brown Trout)",
                                                                 common_name)))))))

#explore data
table(fish$common_name)
length(unique(fish$common_name))
table(fish$watershed)
head(fish)


#plot fish distriubtion on a map to view annually
#first need to make it a spatial file and then load in watershed data, and stream data, and state data

#load NHD data
MAflowline <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/Application materials/UMassPostDoc/NDH/Shape/NHDFlowline.shp")
MAflowline <- MAflowline %>% 
  st_zm()

MAflowline2 <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/Application materials/UMassPostDoc/NDH/Shape/NHDFlowline2.shp")
MAflowline2 <- MAflowline2 %>% 
  st_zm()

MAflowline_combined <- rbind(MAflowline, MAflowline2)
rm(MAflowline, MAflowline2)

MAhuc8 <- st_read("C:/Users/Jenny/Documents/JennySCCWRP/Application materials/UMassPostDoc/NDH/Shape/WBDHU8.shp")


#make the fish dataframe an sf object so it can be plotted spatially
test <- st_as_sf(x = fish,                         
                 coords = c("snapped_longitude", "snapped_latitude"),
                 crs = st_crs(MAflowline_combined))

table(test$common_name)

for( i in 1:length(unique(test$common_name))){
  
  ggplot()+
    # geom_sf(data = MAflowline_combined)+
    geom_sf(data = MAhuc8, color = "green", fill = NA)+
    geom_sf(data = test[test$common_name == unique(test$common_name)[i], ], color = "red")+
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    ggtitle(label = paste(unique(test$common_name)[i], (unique(test$scientific_name)[i]), sep = ", "))
  
  ggsave(
    filename = paste("MA fish plots/", unique(test$common_name)[i], ".png", sep = ""),
    plot = last_plot(),
    width = 9,
    height = 7,
    units = "cm",
    dpi = 300
  )
  
  print(i)
  
}

#prepare sampling event df
event <- sample_data %>% 
  select(sample_id, sample_date, snapped_latitude, snapped_longitude, agency) %>% 
  mutate(state = "MA",
         source = "MassWildlife - JasonStolarski",
         UID = paste("MA", sample_id, sep = "_")) %>% 
  rename(project = agency,
         latitude = snapped_latitude,
         longitude = snapped_longitude,
         date = sample_date) %>% 
  select(UID, state, date, latitude, longitude, project, source, -sample_id)

#prepare fish df. I am goin to use common name for each state, and then I'll go back and create an ID because the fish codes differ by state.
tmp <- left_join(fish_data, species_data, by = "fish_code")

fish <- tmp %>% 
  select(sample_id, common_name, length, weight) %>% 
  mutate(UID = paste("MA", sample_id, sep = "_")) %>% 
  select(-sample_id) %>% 
  rename(fish = common_name)
rm(tmp)

#prepare species df
species <- species_data %>% 
  select(common_name, scientific_name, origin, temp, pt, "function", River_size) %>% 
  rename(tolerance = pt,
         eco_function = "function",
         stream_preference = River_size)

#prepare method df
tmp <- left_join(sample_data, backpack_data, by = "sample_id")
tmp <- left_join(tmp, boat_data, by = "sample_id")
tmp <- left_join(tmp, seine_data, by = "sample_id")
tmp <- left_join(tmp, gillnet_data, by = "sample_id")


