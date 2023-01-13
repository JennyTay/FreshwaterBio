
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

str(des)
des$CollDate <- mdy(des$CollDate)

# these four are duplicated because there were multiple pass: "F03P-02", "F97C-155", "F97C-157", "F97M-158"
#add a column for pass number and make them all one except for the 4 that Andy identified as being a second pass.
#FO3P-02 second pass actually occurred the following day, however, i labeled it pass 2 and left the date the same so that the activity ID would align with the original data set Andy gave me
des$run_num <- ifelse(des$ActivityID == "F03P-02" & des$Duration..sec. == 983, 2,
                      ifelse(des$ActivityID == "F97C-155" & des$Duration..sec. == 1388, 2,
                             ifelse(des$ActivityID == "F97C-157" & des$Duration..sec. == 1052, 2,
                                    ifelse(des$ActivityID == "F97M-158" & des$Duration..sec. == 1166, 2,
                                           1))))




# tidy data
des_event <- des %>% 
  select(CollDate, Lat_Dec, Long_Dec, ActivityID, WaterbodyName) %>% 
  mutate(source = "NHDES - AndyChapman",
         UID = paste("des", ActivityID, sep = "_"),
         project = "NHDES",
         state = "NH",
         waterbody = ifelse(grepl("Pond$", WaterbodyName), "lentic",      #identify lotic or lentic by searching for lakes or ponds at the end of the locatioName
                            ifelse(grepl("Lake$", WaterbodyName), "lentic", "lotic"))) %>%  
  rename(latitude = Lat_Dec, longitude = Long_Dec, date = CollDate) %>% 
  select(UID, state, date, waterbody, latitude, longitude, project, source) %>% 
  unique()

#the three waterbodies that were identified at lentic are actually lotic so I will change these 
#"Trib to Sip Pond", "Snow Brook, DS of Conway Lake", "Snow Brook, US of Conway Lake"
des_event$waterbody[des_event$waterbody == "lentic"] <- "lotic"

#want to make a column for total count, which equals individals + DEADnum - NumStocked
#added in stocking information Dec 23,2022. They mark the count of stocked fish, so we annotated those.

des$NumStocked[is.na(des$NumStocked)] <- 0

des_fish <- des %>% 
  select(ActivityID, FinalID , Individuals, run_num, NumStocked) %>% 
  mutate(UID = paste("des", ActivityID, sep = "_"),
         totalcount = Individuals - NumStocked,
         stock = "natural") %>% #new count column that subtracts the stocked number. The DEADnum is already included in the individuals column and we want to include these, because they died fromthe eshock
  rename(scientific_name = FinalID , count = totalcount) %>% 
  select(UID, scientific_name, count, run_num, stock)

#to make comprable to other datasets, repeat rows with lengths the number of times based on the count value. 

n <-  des_fish$count
des_fish <- des_fish[rep(seq_len(nrow(des_fish)), n),]


des_fish_stock <- des %>% 
  select(ActivityID, FinalID , Individuals, run_num, NumStocked) %>% 
  mutate(UID = paste("des", ActivityID, sep = "_"),
         totalcount = NumStocked,
         stock = "stock") %>% #new count column that equals the count of stocked fish
  rename(scientific_name = FinalID , count = totalcount) %>% 
  select(UID, scientific_name, count, run_num, stock) %>% 
  filter(count>0)

#to make comprable to other datasets, repeat rows with lengths the number of times based on the count value. 

n <-  des_fish_stock$count
des_fish_stock <- des_fish_stock[rep(seq_len(nrow(des_fish_stock)), n),]


des_fish <- rbind(des_fish, des_fish_stock)



des_methods <- des %>% 
  select(ActivityID, CollMeth, Duration..sec., StLength) %>% 
  rename(gear = CollMeth, efish_duration_s = Duration..sec., reach_length_m = StLength) %>% 
  mutate(UID = paste("des", ActivityID, sep = "_"),
         goal = "Total Pick-up") %>% 
  select(UID, gear, goal, reach_length_m, efish_duration_s) %>% 
  unique() %>% 
  group_by(UID, gear, goal, reach_length_m) %>% 
  summarise(efish_duration_s = sum(efish_duration_s)) %>% 
  mutate(target = NA,
         efish_runs = ifelse(UID %in% c("NH_F03P-02", "NH_F97C-155", "NH_F97C-157", "NH_F97M-158"), 2,
                                ifelse(gear %in% c("seine", "gillnet"), NA, 1)))

des_species <- des %>% 
  select(Common.Name, FinalID) %>%
  separate(Common.Name, into = c("common_name", "delete"), sep = "[(]" ) %>% 
  rename(scientific_name = FinalID) %>% 
  select(-delete) %>% 
  unique()

#########################################################

################### New Hampshire DFW ###################

#########################################################

#read in sample data
dat <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Data 1983-2021-1-27-22.xlsx",
                  col_names = TRUE, sheet = "Activity data", col_types = "text")


#read in metadata that has the project goal (total pick up or selective pick up that i characterized based on the metadata)
goal <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/project_goal.xlsx",
                   col_names = TRUE, col_types = "text")
dat <- left_join(dat, goal, by = "Project") %>% 
  filter(Project != "NHDES") %>%  #remove DES data because we got that separately from Andy
  separate(EFISH_time_total, into = c("tmp", "unit"), sep = " ") %>%  #clean total duration column
  mutate(tmp = as.numeric(tmp)) %>% 
  mutate(EFISH_time_total = ifelse(!is.na(unit), tmp*3600, tmp)) %>% #most were in seconds, but a few were in hours and these had a unit 
  select(-tmp, -unit) %>% 
  filter(!Date == "0") %>% 
  mutate(Date = ymd(Date),
         Lat_Start = as.numeric(Lat_Start),
         Long_Start = as.numeric(Long_Start),
         EFISH_length = as.numeric(EFISH_length),
         EFISH_Avg_Width = as.numeric(EFISH_Avg_Width),
         EFISH_width_estimated = as.numeric(EFISH_width_estimated))


#read in fish data

fish <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Data 1983-2021-1-27-22.xlsx",
                   col_names = TRUE, sheet = "Fish data",
                   col_types = "text", range = cell_cols("A:W"))


fish <- fish %>% 
  filter(Project != "NHDES") %>% 
  mutate(Species = toupper(Species)) %>% 
  filter(!Date == "0")
fish$Species[fish$Species == "CFS"] <- "CSF" #typo confirmed by Matt Carpenter
#there are 5 rows where weight was rated '>1' and these rows were removed when reading in the data because it is not numeric. I would have removed them anyways, so I left it this way.


#read in the fish spp code look up table
spp <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Species Codes.xlsx",
                   col_names = TRUE)
names(spp)[1] <- "Species"

fish <- left_join(fish, spp, by = "Species")
#confirm that all spp codes align with a name
unique(fish$Species[is.na(fish$`Common Name`)])
 

# tidy data
fg_event <- dat %>% 
  select(ACT_ID, Lat_Start, Long_Start, Project, Date, Water_Body) %>% 
  mutate(source = "NHFG - MattCarpenter",
         UID = paste("fg", ACT_ID, sep = "_"),
         state = "NH",
         waterbody = ifelse(grepl("Pond$", Water_Body), "lentic",      #identify lotic or lentic by lake, pond, reservoir at the end of the Water_Body
                            ifelse(grepl("Lake$", Water_Body), "lentic", 
                                   ifelse(grepl("Reservoir$", Water_Body), "lentic","lotic")))) %>% 
  rename(latitude = Lat_Start, longitude = Long_Start) %>% 
  select(UID, state, Date, waterbody, latitude, longitude, Project, source)
names(fg_event)[2:7] <- tolower(names(fg_event)[2:7])



fg_fish <- fish %>% 
  select("ACT_ID", "Scientific Name", "Length mm", "Weight g", "Total_Num", "Run_Num") %>% 
  mutate(UID = paste("fg", ACT_ID, sep = "_"),
         Total_Num = as.numeric(ifelse(Total_Num == "NE", NA, Total_Num))) %>% 
  rename(scientific_name = "Scientific Name", count = Total_Num, length_mm = "Length mm", weight_g = "Weight g") %>% 
  select(UID, scientific_name, count, length_mm, weight_g, Run_Num) %>% 
  mutate(length_mm = as.numeric(length_mm),
         weight_g = as.numeric(weight_g),
         Run_Num =as.numeric(Run_Num),
         stock = ifelse(scientific_name %in% c("Salvelinus fontinalis_hatchery", "Salmo trutta_hatchery", "Oncorhynchus mykiss_hatchery"),
                        "stock", "natural")) 
names(fg_fish)[2:6] <- tolower(names(fg_fish)[2:6])

#if brown, rainbow, or brook trout are >200 mark it as stocked
fg_fish$scientific_name[fg_fish$scientific_name == "Salvelinus fontinalis_hatchery"] <- "Salvelinus fontinalis"
fg_fish$scientific_name[fg_fish$scientific_name == "Salmo trutta_hatchery"] <- "Salmo trutta"
fg_fish$scientific_name[fg_fish$scientific_name == "Oncorhynchus mykiss_hatchery"] <- "Oncorhynchus mykiss"
fg_fish$stock <- ifelse(fg_fish$scientific_name %in% c("Salvelinus fontinalis", "Salmo trutta", "Oncorhynchus mykiss") & fg_fish$length_mm >200,
                        "stock", fg_fish$stock)




#to make comprable to other datasets, repeat rows with lengths the number of times based on the count value. 
tmp <- fg_fish %>% filter(count>1) #filter for the rows where count >1 - these are the rows we want to replicate based on count
tmp2 <- fg_fish %>% filter(count == 1) #filter for counts of 1 - these we dont do anything to
tmp3 <- fg_fish %>% filter(is.na(count)) #filter for NA counts - these spp were measured or counted, they are present - we dont do anything to these observations
n <-  tmp$count
tmp <- tmp[rep(seq_len(nrow(tmp)), n),]

tmp4 <- rbind(tmp, tmp2)# dont want to add in the NA counts yet because these do not get a count

#now that there is one row per fish observation, we remove the count column and group by UID, name, and run number and sum the total counts to get
#total counts for the fish that were measured and the fish that were counted
tmp5 <- tmp4 %>% 
  select(-count) %>% 
  group_by(UID, scientific_name, run_num, stock) %>% 
  summarise(count = n())

#join to the tmp4 df, which is the list of all observations.
fg_fish <- tmp4 %>% 
  select(-count) %>% 
  left_join(tmp5, by = c("UID", "scientific_name", "run_num", "stock")) %>% 
  select(UID, scientific_name, count, length_mm, weight_g, run_num, stock)

fg_fish <- rbind(fg_fish, tmp3) #add in fish with NA counts





fg_methods <- dat %>% 
  select(ACT_ID, 
         Gear, 
         goal, 
         target, 
         N_Runs, 
         EFISH_time_total, 
         EFISH_length,
         EFISH_Avg_Width, 
         EFISH_width_estimated, 
         Comments, 
         Data_Comments)%>% 
  mutate(UID = paste("fg", ACT_ID, sep = "_"),
         avg_reach_width_m = ifelse(EFISH_Avg_Width == 0, 
                                    EFISH_width_estimated, 
                                    EFISH_Avg_Width)) %>% 
  rename(gear = Gear, 
         efish_runs = N_Runs, 
         efish_duration_s = EFISH_time_total, 
         reach_length_m = EFISH_length) %>% 
  select(-EFISH_Avg_Width, 
         -EFISH_width_estimated, 
         -ACT_ID) %>% 
  mutate(efish_runs = as.numeric(ifelse(efish_runs == 0, NA, efish_runs)),
         efish_duration_s = as.numeric(ifelse(efish_duration_s == 0, NA, efish_duration_s)),
         reach_length_m = as.numeric(ifelse(reach_length_m == 0, NA, reach_length_m)),
         avg_reach_width_m = as.numeric(ifelse(avg_reach_width_m == 0, NA, avg_reach_width_m)),
         gear = ifelse(gear == 0, NA, gear)) %>% 
  select(UID, gear, goal, reach_length_m, efish_duration_s, efish_runs, target, avg_reach_width_m, Comments, Data_Comments) %>% 
  unique()

  


#### combine the NH datasets
names(des_event)
names(fg_event)

names(fg_fish)
names(des_fish)

names(des_methods)
names(fg_methods)
 
nh_event <- bind_rows(des_event, fg_event)
nh_fish <- bind_rows(des_fish, fg_fish)
nh_method <- bind_rows(des_methods, fg_methods)
nh_species <- des_species


####################################
#save dataframe
save(nh_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/nh_fish_method.RData")
save(nh_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/nh_fish_event.RData")
save(nh_fish, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/nh_fish_fish.RData")
save(nh_species, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/nh_fish_species.RData")

