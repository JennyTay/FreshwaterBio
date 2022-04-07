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

#remove the rows with no common name and with no lat or long
dfw <- dfw %>% 
  filter(!is.na(`species common name`),
         !is.na(latitude))



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


dfw_fish <- dfw %>% 
  mutate(UID = paste("VT", streamname, latitude, longitude, date, sep = "_")) %>% 
  rename(common_name = "species common name",
         count = "# captured",
         weight_g = "meanweight (g)",
         size_class = "size bin (in)")  %>% 
  select(UID, common_name, count, weight_g, size)

dfw_fish$size <- tolower(dfw_fish$size)
dfw_fish$size[dfw_fish$size== "present"] <- NA




#to make comprable to other datasets, repeat rows with lengths the number of times based on the count value. 
#split data into two dataframe, one that has counts, and one that does not, then we will repeat the count rows before joing back with the presence data

tmp <- dfw_fish %>% #df of the observations with  no counts
  filter(is.na(count))

tmp1 <- dfw_fish %>%  #df of the observations with counts
  filter(!is.na(count))

n <-  tmp1$count
tmp1 <- tmp1[rep(seq_len(nrow(tmp1)), n),] #

dfw_fish <- rbind(tmp, tmp1)

#tidy the common names
dfw_fish$common_name[dfw_fish$common_name == "Sucker (Unidentified)"] <- "sucker family"
dfw_fish$common_name[dfw_fish$common_name == "Sunfish (Unidentified)"] <- "sunfish family"
dfw_fish$common_name[dfw_fish$common_name == "Cyprinid (Unidentified)"] <- "minnow family"
dfw_fish$common_name[dfw_fish$common_name == "Unidentified/Unknown"] <- "unknown"
dfw_fish$common_name <- tolower(dfw_fish$common_name)

#remove rows where the common name has an ? (there are not many)
dfw_fish <- dfw_fish %>% 
  filter(!grepl("\\?",common_name))

#need to ask courtney the following questions:
#can I assign random values 1-6 for <6, and so on, to the size class column
#what do all the size classes mean?
#what does zero counts mean?



dfw_method <- dfw %>% 
  mutate(UID = paste("VT", streamname, latitude, longitude, date, sep = "_"),
         reach_length_m = (`stream length of site (ft)`) * (0.3048), #convert to m
         avg_reach_width_m = (`mean bankfill width (ft)`) * (0.3048), #convert to m
         goal = "Total Pick-up", #Courtney confirmed that even though the purpose of the survey is trout, they record all fish they see. only trout get measured. other spp either get counted, or listed as present
         gear = "efish_backpack") %>%  #Courtney confirmed all surveys are backpack efish in wadeable streams. May be variation in equipment, multiple wands, canoes, etc but, but its all similar to backpack efish, no large barges or boats.
  select(UID, gear, goal, reach_length_m, avg_reach_width_m) %>% 
  unique()





####################################
#save dataframe
save(dfw_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdfw_fish_method.RData")
save(dfw_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdfw_fish_event.RData")
save(dfw_fish, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/vtdfw_fish_fish.RData")






