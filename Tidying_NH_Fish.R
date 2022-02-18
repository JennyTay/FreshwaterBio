library(RODBC)    
library(tidyverse)
library(lubridate)
library(sf)
library(readxl)

#########################################################

################### New Hampshire DES ###################

#########################################################


des <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DES Fish Data/20220119_NHDES_Fish Data_yrs 2000-2021.csv",
                skip = 1)
des <- des %>% 
  filter(!is.na(Lat_Dec)) %>% 
  mutate(Long_Dec = ifelse(Long_Dec>0, -Long_Dec, Long_Dec),
         CollMeth = tolower(CollMeth))
des$CollMeth[des$CollMeth == "singlepass"] <- "backpack"

# these four are duplicated because there were multiple pass: "F03P-02", "F97C-155", "F97C-157", "F97M-158"
#add a column for pass number and make them all one except for the 4 that Andy identified as being a second pass.
#FO3P-02 second pass actually occurred the following day, however, i labeled it pass 2 and left the date the same so that the activity ID would align with the original data set Andy gave me
des$run_num <- ifelse(des$ActivityID == "F03P-02" & des$Duration..sec. == 983, 2,
                      ifelse(des$ActivityID == "F97C-155" & des$Duration..sec. == 1388, 2,
                             ifelse(des$ActivityID == "F97C-157" & des$Duration..sec. == 1052, 2,
                                    ifelse(des$ActivityID == "F97M-158" & des$Duration..sec. == 1166, 2,
                                           1))))



#make shapefile the same crs as NHD
#load NHD data
MAflowline <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NDH/Shape/NHDFlowline.shp")


#make the fish dataframe an sf object so it can be plotted spatially
shp <- st_as_sf(x = des,                         
                coords = c("Long_Dec", "Lat_Dec"),
                crs = st_crs(MAflowline))
st_write(shp, dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DES Fish Data/20220119_NHDES_Fish Data_yrs 2000-2021.shp")



# tidy data
des_event <- des %>% 
  select(CollDate, Lat_Dec, Long_Dec, ActivityID) %>% 
  mutate(source = "NHDES - AndyChapman",
         UID = paste("NH", ActivityID, sep = "_"),
         project = "des",
         state = "NH") %>% 
  rename(latitude = Lat_Dec, longitude = Long_Dec, date = CollDate) %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  unique()

des_fish <- des %>% 
  select(ActivityID, Common.Name, Individuals, run_num) %>% 
  mutate(UID = paste("NH", ActivityID, sep = "_")) %>% 
  rename(common_name = Common.Name, count = Individuals) %>% 
  select(UID, common_name, count, run_num)

des_methods <- des %>% 
  select(ActivityID, CollMeth, Duration..sec., StLength) %>% 
  rename(gear = CollMeth, efish_duration_s = Duration..sec., reach_length_m = StLength) %>% 
  mutate(UID = paste("NH", ActivityID, sep = "_"),
         goal = "Total Pick-up") %>% 
  select(UID, gear, goal, reach_length_m, efish_duration_s) %>% 
  unique() %>% 
  group_by(UID, gear, goal, reach_length_m) %>% 
  summarise(efish_duration_s = sum(efish_duration_s)) %>% 
  mutate(target = NA,
         efish_run_num = ifelse(UID %in% c("NH_F03P-02", "NH_F97C-155", "NH_F97C-157", "NH_F97M-158"), 2,
                                ifelse(gear %in% c("seine", "gillnet"), NA, 1)))

des_species <- des %>% 
  select(Common.Name, FinalID) %>% 
  rename(common_name = Common.Name, scientific_name = FinalID) %>% 
  unique()

#########################################################

################### New Hampshire DFW ###################

#########################################################

#read in sample data
dat <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Data 1983-2020_20210519- DONT ALTER.xlsx",
                  col_names = TRUE, sheet = "Activity data", col_types = "text")

#read in metadata that has the project goal (total pick up or selective pick up that i characterized based on the metadata)
goal <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/project_goal.xlsx",
                   col_names = TRUE, col_types = "text")
dat <- left_join(dat, goal, by = "Project") %>% 
  filter(Project != "NHDES") %>% 
  separate(EFISH_time_total, into = c("tmp", "unit"), sep = " ") %>%  #clean total duration column
  mutate(tmp = as.numeric(tmp)) %>% 
  mutate(EFISH_time_total = ifelse(!is.na(unit), tmp*3600, tmp)) %>% 
  select(-tmp, -unit)


#read in fish data
fish <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Data 1983-2020_20210519- DONT ALTER.xlsx",
                  col_names = TRUE, sheet = "Fish data",
                  col_types = "text", range = cell_cols("A:W"))
fish <- fish %>% 
  filter(Project != "NHDES") %>% 
  mutate(Species = toupper(Species))
#there are 5 rows where weight was rated '>1' and these rows were removed when reading in the data because it is not numeric. I would have removed them anyways, so I left it this way.


#read in the fish spp code look up table
spp <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Species Codes.xls",
                   skip = 2, col_names = TRUE)
names(spp)[3] <- "Species"

#need to figure out what EBT_H, BT_H and RT_H should be, because there is no look up for them in the look up table
fish <- left_join(fish, spp, by = "Species")




# tidy data
fg_event <- dat %>% 
  select(ACT_ID, Lat_Start, Long_Start, Project, Date) %>% 
  mutate(source = "NHFG - MattCarpenter",
         UID = paste("NH", ACT_ID, sep = "_"),
         state = "NH") %>% 
  rename(latitude = Lat_Start, longitude = Long_Start) %>% 
  select(UID, state, Date, latitude, longitude, Project, source)
names(fg_event)[2:7] <- tolower(names(fg_event)[2:7])

fg_fish <- fish %>% 
  select("ACT_ID", "Common Name", "Length mm", "Weight g", "Total_Num", "Run_Num") %>% 
  mutate(UID = paste("fg", ACT_ID, sep = "_"),
         Total_Num = as.numeric(ifelse(Total_Num == "NE", NA, Total_Num))) %>% 
  rename(common_name = "Common Name", count = Total_Num, length_mm = "Length mm", weight_g = "Weight g") %>% 
  select(UID, common_name, count, length_mm, weight_g, Run_Num) %>% 
  mutate(length_mm = as.numeric(length_mm),
         weight_g = as.numeric(weight_g))
names(fg_fish)[2:6] <- tolower(names(fg_fish)[2:6])

fg_methods <- dat %>% 
  select(ACT_ID, Gear, goal, target, N_Runs, 
         EFISH_time_total, EFISH_length,
         EFISH_Avg_Width, EFISH_width_estimated, 
         Comments, Data_Comments)%>% 
  mutate(UID = paste("fg", ACT_ID, sep = "_"),
         avg_reach_width_m = ifelse(EFISH_Avg_Width == 0, EFISH_width_estimated, EFISH_Avg_Width)) %>% 
  rename(gear = Gear, efish_run_num = N_Runs, efish_duration_s = EFISH_time_total, 
         reach_length_m = EFISH_length) %>% 
  select(-EFISH_Avg_Width, -EFISH_width_estimated, -ACT_ID) %>% 
  mutate(efish_run_num = as.numeric(ifelse(efish_run_num == 0, NA, efish_run_num)),
         efish_duration_s = as.numeric(ifelse(efish_duration_s == 0, NA, efish_duration_s)),
         reach_length_m = as.numeric(ifelse(reach_length_m == 0, NA, reach_length_m)),
         avg_reach_width_m = as.numeric(ifelse(avg_reach_width_m == 0, NA, avg_reach_width_m)),
         gear = ifelse(gear == 0, NA, gear)) %>% 
  select(UID, gear, goal, reach_length_m, efish_duration_s, efish_run_num, target, avg_reach_width_m, Comments, Data_Comments)

  

#make shapefile the same crs as NHD
#load NHD data
MAflowline <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NDH/Shape/NHDFlowline.shp")


#make the fish dataframe an sf object so it can be plotted spatially
dat <- data.frame(dat)
shp <- st_as_sf(x = dat,                         
                coords = c("Long_Start", "Lat_Start"),
                crs = st_crs(MAflowline))
st_write(shp, dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Data 1983-2020_20210519- DONT ALTER.shp")



#### combine the NH datasets
names(des_event)
names(fg_event)

names(fg_fish)
names(des_fish)

names(des_methods)
names(fg_methods)
