#########################################################

################### New Hampshire DES ###################

#########################################################


des <- read.csv("C:/Users/Jenny/Documents/JennySCCWRP/Application materials/UMassPostDoc/spp_data/NH DES Fish Data/20220119_NHDES_Fish Data_yrs 2000-2021.csv",
                skip = 1)
des$CollDate <- mdy(des$CollDate)
des$year <- year(des$CollDate)
des$month <- month(des$CollDate)
table(des$year)
table(des$month)
length(unique(des$EMDStationID_Current))

names(des) <-  c("date", "waterbody", "town", "activityID", "EMDStationID_Current", 
                 "FishSamps_FishSampID", "method", "commonName", "Individuals", "scientificName",                       
                 "Comments", "Latitude", "Longitude", "Duration_sec", "Shock Set_V_duty_cycle/freq",
                 "StLength", "NumYOY", "DEADnum", "NumStocked", "FishValues_FishSampID",         
                 "year", "month")

table(des$FishSamps_FishSampID == des$FishValues_FishSampID)

#########################################################

################### New Hampshire DFW ###################

#########################################################

#read in sample data
dat <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Data 1983-2020_20210519- DONT ALTER.xlsx",
                  col_names = TRUE, sheet = "Activity data", col_types = "text")

#read in fish data
tmp <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH DFG Fish Data/Fish Data 1983-2020_20210519- DONT ALTER.xlsx",
                  col_names = TRUE, sheet = "Fish data",
                  col_types = "text", range = cell_cols("A:W"))
#there are 5 rows where weight was rated '>1' and these rows were removed when reading in the data because it is not numeric. I would have removed them anyways, so I left it this way.
#remove variables that are duplicated in dat
tmp <- tmp %>% 
  select(- c("Year", "Date", "Time", "Gear", "County"))

#read in the fish spp code look up table
tmp2 <- read_excel("C:/Users/Jenny/Documents/JennySCCWRP/Application materials/UMassPostDoc/spp_data/NH DFG Fish Data/Fish Species Codes.xls",
                   skip = 2, col_names = TRUE)
names(tmp2)[3] <- "Species"

test <- left_join(dat, tmp, by = "ACT_ID")
table(test$Site_Name.x == test$Site_Name.y)
f <- test %>% filter(Project.x != Project.y) %>% select(ACT_ID, Town.x, Town.y, Year.x, Year.y, Project.x, Project.y, County.y, County.x, Site_Name.x, Site_Name.y)

dat$year2 <- year(ymd(dat$Date))
table(dat$year2 == dat$Year)
f <- dat %>% filter(year2 != Year)