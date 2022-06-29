

#Tidying Me mussel data


library(tidyverse)
library(lubridate)
library(RODBC) 

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

#make a query for the location data df so that when we bind it with the others it's not such a large file
qry1 <- "SELECT LocID, TopUTMX, TopUTMY FROM LocationID"
location_data <- sqlQuery(con2, qry1)

#make a query for the fish_data to reduce object size
qry2 <- "SELECT Sample ID, Gear, RunNum, TaxonID, Stage, Units, Length, Weight, Origin, Age, FROM newIndividualfish"
lengths_data <- sqlQuery(con2, qry2)

#make a query for the species data to reduce object size
qry3 <- "SELECT SPECIES, SPP, SCIENTIF FROM SPP"
species_data <- sqlQuery(con2, qry3)

#make a query for the event data
qry4 <- "SELECT Sample ID, LocationID, Surveyors, BegDate, LengthSection, AveWidth,  FROM tblSamples"
waterbody_data <- sqlQuery(con2, qry4)



#### the rest is copied from the DEC_VT file to guide the code, but need to update for ME!!!


# event data
me_event <- vt %>% 
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

me_fish <- vt %>% 
  select(EventID, Species, Run1, Run2, Run3) %>% 
  mutate(UID = paste("VT", EventID, sep = "_")) %>% 
  rename(common_name = Species) %>% 
  pivot_longer(cols = 3:5, names_to = "run_num", values_to = "count" ) %>% 
  select(UID, common_name, count, run_num) 
me_fish$run_num <- str_replace_all(me_fish$run_num, "Run", "")
me_fish$run_num <- as.numeric(me_fish$run_num)

keep <- complete.cases(me_fish) #remove rows that did not do a 2nd or 3rd pass 
me_fish$keep <- keep
me_fish <- me_fish %>% 
  filter(keep == TRUE) %>% 
  select(-keep)


#to make comprable to other datasets, repeat rows with lengths the number of times based on the count value. 

n <-  me_fish$count
me_fish <- me_fish[rep(seq_len(nrow(me_fish)), n),]




#note that the count field is the number of spp per survey and per run - I did not calculate this, its what they record (they do not do the individual measuresments)

me_method <- vt %>% 
  select(EventID, GearID, SectionWidth, SectionLength, Run1, Run2, Run3) %>% 
  rename(gear = GearID, reach_length_m = SectionLength, avg_reach_width_m = SectionWidth) %>% 
  mutate(UID = paste("VT", EventID, sep = "_"),
         goal = "Total Pick-up",
         efish_runs = ifelse(!is.na(Run3), 3,
                             ifelse(!is.na(Run2) & is.na(Run3), 2, 
                                    ifelse(is.na(Run1), NA, 1)))) %>% 
  select(UID, gear, goal, reach_length_m, avg_reach_width_m, efish_runs) %>% 
  unique()

me_method$gear[me_method$gear == "ES"] <- "electroshock"
me_method$gear[me_method$gear == "SN"] <- "seine"






####################################
#save dataframe
save(me_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/me_fish_method.RData")
save(me_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/me_fish_event.RData")
save(me_fish, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/me_fish_fish.RData")


