#load libraries

library(tidyverse)
library(lubridate)
library(sf)
library(readxl)



#########################################################

################### Vermont DEC ########################

#########################################################


vt <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT DEC Fish Data/VT DEC Fish Data 01-28-2022 (with TE).xlsx",
                 col_names = TRUE, sheet = "All VDEC Fish Data w TE Species")

fish <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT DEC Fish Data/VT DEC Fish Data 01-28-2022 (with TE).xlsx",
                 col_names = TRUE, sheet = "Fish Library", range = cell_cols("A:I"))
fish <- fish %>% 
  select(FishID, Species)

vt <- left_join(vt, fish, by = "FishID")

range(vt$Date)
str(vt)



# tidy data
dec_event <- vt %>% 
  select(Date, "Latitude (DD)", "Longitude (DD)", EventID, Location) %>% 
  mutate(source = "VTDEC - JimDeshler",
         UID = paste("VT", EventID, sep = "_"),
         project = "VTDEC",
         state = "VT",
         waterbody = ifelse(grepl("Pond$", Location), "lentic",      #identify lotic or lentic by lake, pond, reservoir at the end of the Water_Body
                   ifelse(grepl("Lake$", Location), "lentic", 
                          ifelse(grepl("Reservoir$", Location), "lentic","lotic")))) %>% 
  rename(latitude = "Latitude (DD)", longitude = "Longitude (DD)", date = Date) %>% 
  select(UID, state, date, waterbody, latitude, longitude, project, source) %>% 
  unique()

dec_fish <- vt %>% 
  select(EventID, Species, Run1, Run2, Run3) %>% 
  mutate(UID = paste("VT", EventID, sep = "_")) %>% 
  rename(common_name = Species) %>% 
  pivot_longer(cols = 3:5, names_to = "run_num", values_to = "count" ) %>% 
  select(UID, common_name, count, run_num) 
dec_fish$run_num <- str_replace_all(dec_fish$run_num, "Run", "")
dec_fish$run_num <- as.numeric(dec_fish$run_num)

keep <- complete.cases(dec_fish) #remove rows that did not do a 2nd or 3rd pass 
dec_fish$keep <- keep
dec_fish <- dec_fish %>% 
  filter(keep == TRUE) %>% 
  select(-keep)


#to make comprable to other datasets, repeat rows with lengths the number of times based on the count value. 

n <-  dec_fish$count
dec_fish <- dec_fish[rep(seq_len(nrow(dec_fish)), n),]




#note that the count field is the number of spp per survey and per run - I did not calculate this, its what they record (they do not do the individual measuresments)

dec_method <- vt %>% 
  select(EventID, GearID, SectionWidth, SectionLength, Run1, Run2, Run3) %>% 
  rename(gear = GearID, reach_length_m = SectionLength, avg_reach_width_m = SectionWidth) %>% 
  mutate(UID = paste("VT", EventID, sep = "_"),
         goal = "Total Pick-up",
         efish_runs = ifelse(!is.na(Run3), 3,
                                ifelse(!is.na(Run2) & is.na(Run3), 2, 
                                       ifelse(is.na(Run1), NA, 1)))) %>% 
  select(UID, gear, goal, reach_length_m, avg_reach_width_m, efish_runs) %>% 
  unique()

dec_method$gear[dec_method$gear == "ES"] <- "electroshock"
dec_method$gear[dec_method$gear == "SN"] <- "seine"


fish <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT DEC Fish Data/VT DEC Fish Data 01-28-2022 (with TE).xlsx",
                   col_names = TRUE, sheet = "Fish Library", range = cell_cols("A:I"))
dec_species <- fish %>% 
  filter(Species != "NO FISH!") %>% 
  select(Species, NonnativeToState, WaterTypeID, Tolerance, FishFunctionLookupID) %>% 
  rename(common_name = Species, temperature_preference = WaterTypeID, tolerance = Tolerance, eco_function = FishFunctionLookupID) %>% 
  unique() %>% 
  mutate(orgin = ifelse(NonnativeToState == "Y", "nonnative", "native")) %>% 
  select(-NonnativeToState)
dec_species$orgin[is.na(dec_species$orgin)] <- "native"



####################################
#save dataframe
save(dec_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdec_fish_method.RData")
save(dec_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdec_fish_event.RData")
save(dec_fish, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdec_fish_fish.RData")
save(dec_species, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdec_fish_species.RData")













#########################################################

################### Vermont DFW ########################

#########################################################





dfw <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT DFG Fish Data/Vermont electrofishing data 1954-2020.xlsx",
                 col_names = TRUE)


names(dfw) <- tolower(names(dfw))

names(dfw)





# tidy data
dfw_event <- dfw %>% 
  select(date, latitude, longitude, streamname) %>% 
  mutate(source = "VTDFW - CourtneyBuckley",
         UID = paste("VT", streamname, latitude, longitude, date, sep = "_"),
         project = "VTDFG",
         state = "VT",
         waterbody = "lotic") %>% #courtney confirmed all are stream sites 
  select(UID, state, date, waterbody, latitude, longitude, project, source) %>% 
  unique()

# I left off here, need to edit this text for dfg, right now its just copied from above vtdec
dfw_fish <- dfw %>% 
  select(EventID, Species, Run1, Run2, Run3) %>% 
  mutate(UID = paste("VT", EventID, sep = "_")) %>% 
  rename(common_name = Species) %>% 
  pivot_longer(cols = 3:5, names_to = "run_num", values_to = "count" ) %>% 
  select(UID, common_name, count, run_num) 
dfw_fish$run_num <- str_replace_all(dfw_fish$run_num, "Run", "")
dfw_fish$run_num <- as.numeric(dfw_fish$run_num)



#to make comprable to other datasets, repeat rows with lengths the number of times based on the count value. 

n <-  dec_fish$count
dec_fish <- dec_fish[rep(seq_len(nrow(dec_fish)), n),]




#note that the count field is the number of spp per survey and per run - I did not calculate this, its what they record (they do not do the individual measuresments)

dec_method <- dfw %>% 
  select(EventID, GearID, SectionWidth, SectionLength, Run1, Run2, Run3) %>% 
  rename(gear = GearID, reach_length_m = SectionLength, avg_reach_width_m = SectionWidth) %>% 
  mutate(UID = paste("VT", EventID, sep = "_"),
         goal = "Total Pick-up",
         efish_runs = ifelse(!is.na(Run3), 3,
                             ifelse(!is.na(Run2) & is.na(Run3), 2, 
                                    ifelse(is.na(Run1), NA, 1)))) %>% 
  select(UID, gear, goal, reach_length_m, avg_reach_width_m, efish_runs) %>% 
  unique()

dec_method$gear[dec_method$gear == "ES"] <- "electroshock"
dec_method$gear[dec_method$gear == "SN"] <- "seine"




####################################
#save dataframe
save(vtdfw_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdfw_fish_method.RData")
save(vtdfw_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdfw_fish_event.RData")
save(vtdfw_fish, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdfw_fish_fish.RData")






