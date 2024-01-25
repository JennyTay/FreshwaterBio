#linking EO data (which is the data about the time of survey and what was found) to the source file, which 

library(sf)
library(tidyverse)
library(lubridate)
library(readxl)

#first load in an NHD layer, becuase we will use the CRS of the NHD layer to project all the mussel data collected in a different CRS
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")




###########################################################

##################   mussel2002  ##########################


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/mussel2002.mdb")
spp_visit <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data//mussel2002.mdb", layer = "SPECIES_VISIT")
spp_desc <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data//mussel2002.mdb", layer = "SPECIES_DESC")
site_visit <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data//mussel2002.mdb", layer = "SITE_VISIT")
spp_demogr <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data//mussel2002.mdb", layer = "SPECIES_DEMOGRAPHICS")

shp <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MusselDB_species.shp")
st_crs(shp)
st_crs(huc8)



#look up table to go from scientific name to common name
spp_desc <- spp_desc %>%
  data.frame() %>% 
  select(SNAME, SCOMNAME, TAX_GRP)
spp_desc$SCOMNAME[spp_desc$SNAME == "STROPHITUS UNDULATUS"] <- "CREEPER"


#table to get the characterics of the mussel
spp_demogr <- spp_demogr %>% 
  data.frame() %>% 
  rename(SNAME = SCIENTIFIC_NAME) %>% 
  select(2:10)

head(spp_visit)
mussel <- spp_visit %>% 
  select(3:15) 

#fix the method information in the mussel data
mussel$NUMBER_SEARCHERS[mussel$SCUBA_Divers == 2] <- 2
mussel$NUMBER_SEARCHERS[mussel$Snorkels == 1 & mussel$View_Buckets == 3] <- 4
names(mussel)[7:10] <- c("Snorkel", "SCUBA", "View_Bucket", "Shoreline_Walk")
mussel$survey_method_new <- ifelse(!is.na(mussel$Snorkel), "snorkel",
                                   ifelse(!is.na(mussel$Shoreline_Walk), "shoreline walk",
                                          ifelse(!is.na(mussel$SCUBA), "scuba",
                                                 ifelse(!is.na(mussel$View_Bucket), "view bucket", NA))))
mussel$survey_method_new <- ifelse(!is.na(mussel$Snorkel) & !is.na(mussel$View_Bucket), "snorkel and view bucket", mussel$survey_method_new)
mussel <- mussel %>% 
  select(SNAME, OBS_DATE, date1, SITENUMBER, survey_method_new, NUMBER_SEARCHERS, SEARCH_TIME, LIVE, SHELLS) %>% 
  rename(survey_method = survey_method_new)

#join mussel data to the spp descrition table (the table that gives common name and taxonomic group)
mussel <- left_join(mussel, spp_desc, by = "SNAME") %>% 
  filter(TAX_GRP == "Mussels" | TAX_GRP == "Non-Native Mollusks") %>% 
  data.frame() %>% 
  select(-geometry)

#join mussel data to the spp demographic table (the table that describes the mussels that were observed)
mussel <- left_join(mussel, spp_demogr, by = c("OBS_DATE", "SITENUMBER", "SNAME"))
names(mussel) <- tolower(names(mussel))


length(unique(shp$SITENUMBER))
length(unique(mussel$sitenumber))

#project the geographic information to the NHD projection
shp <- st_transform(shp, st_crs(huc8))


#filter the shapefile for only the mussel spp
shp <- shp %>% 
  filter(Species %in% mussel$sname) %>% 
  select(SITENUMBER) %>% 
  unique()
names(shp) <- tolower(names(shp))

#left join the mussel to thes hp to get geographic inforatmion - now the mussel data has geographic information
ma_mussel <- left_join(mussel, shp, by = "sitenumber")

str(ma_mussel)
colSums(is.na(ma_mussel))


ma_mussel <- ma_mussel %>% 
  mutate(longitude = unlist(map(ma_mussel$geometry, 1)),
         latitude = unlist(map(ma_mussel$geometry, 2))) %>% 
  select(latitude, longitude, obs_date, sname, scomname, sitenumber, 
         live, shells, tax_grp, length, height, condition, sex, gravid_brooding, 
         survey_method, number_searchers, search_time) %>% 
  mutate(live_occurrence = ifelse(live == "Present", 1,
                             ifelse(live == 0, 0, 1)),
         shell_occurrence = ifelse(shells == "present" | shells == "many", 1,
                              ifelse(shells == 0, 0, 1)),
         length_mm = ifelse(length <5, length *10, length)) %>% #confirmed with jason, if length is less than 5, likely cm, otherwise its mm.
  rename(live_count = live,
         shell_count = shells,
         scientific_name = sname,
         common_name = scomname,
         date = obs_date) %>% 
  mutate(source = "JasonCarmignani-MassWildlife",
         project= "mussel2002 database",
         state = "MA")

#tidy the 'live_count column
ma_mussel$live_count[ma_mussel$live_count == "Present"] <- NA #it is important to do this after the ifelse command above, because we want the occurence column to register these as a presnt. If we revalue it as 
#NA, then the ifelse statement above will give the occurrence value of NA for these observations.
ma_mussel$live_count[ma_mussel$live_count == "100s to 1000s"] <- 500 #take the mid-range
ma_mussel$live_count[ma_mussel$live_count == "10s to 100"] <- 50 #take the mid-range
ma_mussel$live_count <- as.numeric(ma_mussel$live_count)

#tidy the "shell_count" column
ma_mussel$shell_count[ma_mussel$shell_count == "present"] <- NA
ma_mussel$shell_count[ma_mussel$shell_count == "many"] <- NA
ma_mussel$shell_count <- as.numeric(ma_mussel$shell_count)

#survey_method column
ma_mussel$survey_method <- tolower(ma_mussel$survey_method)


#make names lowercase 
ma_mussel$scientific_name <- tolower(ma_mussel$scientific_name)
ma_mussel$common_name <- tolower(ma_mussel$common_name)
table(ma_mussel$scientific_name)


site_visit <- site_visit %>% 
  data.frame() %>% 
  select(c(2:3, 12)) 


str(site_visit)
unique(site_visit$DIST_INVENT)
site_visit$DIST_INVENT[is.na(as.numeric(site_visit$DIST_INVENT))]  #identify the non numerical entires into NA
site_visit$DIST_INVENT[site_visit$DIST_INVENT == "120 m"] <- 120
site_visit$DIST_INVENT[site_visit$DIST_INVENT == "NA"] <- NA

site_visit$DIST_INVENT <- as.numeric(site_visit$DIST_INVENT)

names(site_visit) <- c("sitenumber", "date", "reach_length_m") #need to confirm with Jason that the reach lenght is in m


ma_mussel <- left_join(ma_mussel, site_visit, by = c("sitenumber", "date"))

#remove an erroneaous entry
ma_mussel <- ma_mussel[-2325,] #this row was found to be erroneous because when I made the mussel occurrence df below and then grouped by UID and 
#common_name (test below), there were two rows - one that found the mussel to the present, and one that did not. The one that did not appeared erroneous because
#the other columns were not consistent with the values for the rest of the rows done on that date at that site.
ma_mussel <- ma_mussel[-819,] #same issue here - when we made the count df, there were two different counts for the brook floater (swollen wedgemussel), 8 and 22, so 
#we kept the one that was 8


ma_mussel_event <- ma_mussel %>% 
  mutate(state = "MA",
         UID = paste(state, sitenumber, date, sep = "_")) %>% 
  select(UID, state, latitude, longitude, date, project, source) %>% 
  unique()



ma_mussel_occurrence <- ma_mussel %>% 
  mutate(UID = paste("MA", sitenumber, date, sep = "_")) %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence) %>% 
  unique()

test <- ma_mussel_occurrence %>% 
  group_by(UID, common_name,) %>% 
  summarise(count = n())



ma_mussel_count <- ma_mussel %>% 
  mutate(UID = paste("MA", sitenumber, date, sep = "_")) %>%  
  select(UID, common_name, scientific_name, live_count, shell_count) %>% 
  unique()
test <- ma_mussel_count %>% 
  group_by(UID, common_name,) %>% 
  summarise(count = n())



ma_mussel_length <- ma_mussel %>% 
  mutate(UID = paste("MA", sitenumber, date, sep = "_")) %>% 
  select(UID, common_name, scientific_name, length_mm, height) %>% 
  rename(height_mm = height) %>% #confirm height is mm
  filter(!is.na(length_mm))

ma_mussel_method <- ma_mussel %>% 
  mutate(UID = paste("MA", sitenumber, date, sep = "_")) %>% 
  select(c(UID, 15:17, 24)) %>% 
  unique()


rm(mussel, shp, site_visit, spp_demogr, spp_desc, spp_visit, ma_mussel, test)


#make a common_name to scientific_name look up file

lookup <- ma_mussel_occurrence %>% 
  select(common_name, scientific_name) %>% 
  unique()

write.csv(lookup, file = "musselsppnames.csv")


#################################################################

############   MAFloaterDatabaseDATA2019_20210824 ###############






st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MAFloaterDatabaseDATA2019_20210824.accdb")
mussel_data <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MAFloaterDatabaseDATA2019_20210824.accdb", layer = "MusselData")
survey_data <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MAFloaterDatabaseDATA2019_20210824.accdb", layer = "SurveyData")

colSums(is.na(mussel_data)) #sex and width have almost no data so we will remove these fields


#fix the abundance data, so first remove the rows with no mussels
tmp1 <- mussel_data %>%
  data.frame() %>% 
  filter(!Species == "NO MUSSELS") %>% 
  mutate(shell_count = ifelse(Abundance.Category == "Shell Only", Count, NA),
         individual_count = ifelse(Abundance.Category == "See Count", Count, NA),
         live_occurrence = ifelse(Abundance.Category == "Shell Only", 0, 1),
         shell_occurrence = ifelse(shell_count >0, 1,NA)) %>% 
  select(-Width, -Abundance.Category, -Sex, -geometry, -TagNumber) %>% 
  rename("UID" = "Mussel_SiteName")

tmp1a <- tmp1 %>% 
  filter(!is.na(Count)) %>% 
  group_by(UID, Species) %>% 
  summarise(shellcountnew = sum(shell_count, na.rm = T),
            individual_countnew = sum(individual_count, na.rm = T))


tmp1 <- left_join(tmp1, tmp1a, by = c("UID", "Species"))



tmp2 <- mussel_data %>% 
  filter(Species == "NO MUSSELS")%>% 
  select(-Width, -Abundance.Category, -Count, -Sex, -geometry, -TagNumber) %>% 
  rename("UID" = "Mussel_SiteName") %>% 
  mutate(shellcountnew = 0,
         individual_countnew = 0,
         live_occurrence = 0,
         shell_occurrence = 0)

#add in names with zeros to the sites with no mussels observed
num <- length(unique(lookup$scientific_name))
tmp3 <- do.call("rbind", replicate(num, tmp2, simplify = F))

spp <- rep(unique(lookup$scientific_name), each = 89)

tmp3$Species <- spp


mussel_data <- bind_rows(tmp1, tmp3)

#add in survey data


mussel_data <- left_join(mussel_data, survey_data, by = c("UID" = "Survey_SiteName"))
mussel_data$UID[mussel_data$UID == "Konkapot_2"] <- "MA_Konkapot_2"
mussel_data$UID[mussel_data$UID == "Konkapot_1"] <- "MA_Konkapot_1"
mussel_data$scientific_name <- mussel_data$Species


names(mussel_data)[3:64] <- tolower(names(mussel_data)[3:64])
mussel_data$scientific_name <- tolower(mussel_data$scientific_name)

mussel_data <- left_join(mussel_data, lookup, by = "scientific_name")



bk_mussel_event <- mussel_data %>% 
  mutate(state = "MA",
         project = "brookfloater_study",
         source = "JasonCarmignani-brookfloater",
         waterbody = "lotic") %>% 
  select(UID, state, x, y, date, waterbody, project, source) %>% 
  rename(latitude = y,
         longitude = x) %>% 
  unique() %>% 
  filter(!is.na(latitude))




bk_mussel_occurrence <- mussel_data %>% 
  select(UID, scientific_name, common_name, live_occurrence, shell_occurrence) %>% 
  unique()




bk_mussel_count <- mussel_data %>% 
  select(UID, scientific_name, common_name, individual_countnew, shellcountnew) %>% 
  rename(live_count = individual_countnew,
         shell_count = shellcountnew) %>% 
  unique()


bk_mussel_length <- mussel_data %>% 
  select(UID, scientific_name, common_name, length, height) %>% 
  rename(length_mm = length, #confirm unit is mm
         height_mm = height) %>%  #confirm unit is mm
  filter(!is.na(length_mm),
         scientific_name != "no mussels")


bk_mussel_method <- mussel_data %>% 
  select(UID, noobservers, wetwidth1, wetwidth2, wetwidth3, measured.length.of.stream.survey, access.visibility..1m) %>% 
  filter(UID %in% bk_mussel_event$UID) %>% 
  rename(number_searchers = noobservers,
         reach_length_m = measured.length.of.stream.survey, #want to confirm unit is m
         access_visibility_1m = access.visibility..1m) %>% 
  mutate(wet_width_avg_m = mean(c(wetwidth1, wetwidth2, wetwidth3))) %>%  #want to confirm wetted width is m
  select(-c(3:5)) %>% 
  unique()


rm(mussel_data, survey_data, tmp1, tmp1a, tmp2, tmp3)
###

#bind together mussel database and the brook floater data


head(ma_mussel_event)
head(bk_mussel_event)

ma_mussel_event <- bind_rows(ma_mussel_event, bk_mussel_event)
ma_mussel_occurrence <- bind_rows(ma_mussel_occurrence, bk_mussel_occurrence)
ma_mussel_count <- bind_rows(ma_mussel_count, bk_mussel_count)
ma_mussel_length <- bind_rows(ma_mussel_length, bk_mussel_length)
ma_mussel_method <- bind_rows(ma_mussel_method, bk_mussel_method)

save(ma_mussel_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_event.RData")
save(ma_mussel_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_occurrence.RData")
save(ma_mussel_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_count.RData")
save(ma_mussel_length, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_length.RData")
save(ma_mussel_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_method.RData")




###################################################################

################# Maine Dept of Inland Waters  ####################



library(measurements)
library(stringi)

me_mussel <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/ME IFW Mussel Data/Maine Freshwater Mussel Survey Data_20220426.csv")

names(me_mussel)
me_mussel <- me_mussel %>% 
  filter(LAT != "")

#we will use the converstion tool to make the LAT and LONG from dms into dd. The decimal degree columns in this dataset are not as complete as the dms, that's why we won't
#just use the dd columns
tmp2 <- me_mussel %>% 
  select(LAT, LONG, DDLONG, DDLAT) %>% 
  
  mutate(latitude2 = stri_sub_replace(LAT, 3, 3, value = " "), #to use the conv_unit, the dms  needs to have a space in between
         latitude3 = stri_sub_replace(latitude2, 6, 6, value = " "),
         longitude2 = stri_sub_replace(LONG, 3, 3, value = " "),
         longitude3 = stri_sub_replace(longitude2, 6, 6, value = " "))


tmp2$latitudefinal <- conv_unit(tmp2$latitude3, from = "deg_min_sec", to= "dec_deg") #convert to decimal degrees
tmp2$longitudefinal <- conv_unit(tmp2$longitude3, from = "deg_min_sec", to= "dec_deg") #convert to decimal degrees

me_mussel$latitude <- tmp2$latitudefinal
me_mussel$longitude <- tmp2$longitudefinal

me_mussel <- me_mussel %>% 
  select(WTYPE, SITE, MONTH, DAY, YEAR, SVYTYPE, SRCTYPE, SOURCE, 17:30) %>% 
  rename(waterbody = WTYPE,
         survey_method = SVYTYPE,
         project = SOURCE) %>% 
  mutate(source = "Beth Swartz - ME Inland Fisheries and Wildlife",
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         longitude = ifelse(longitude > 0, -longitude, longitude))




head(me_mussel)

me_mussel$waterbody[me_mussel$waterbody == "L"] <- "lentic"
me_mussel$waterbody[me_mussel$waterbody == "W"] <- "lotic"
me_mussel$survey_method[me_mussel$survey_method == "S"] <- "survey"
me_mussel$survey_method[me_mussel$survey_method == "I"] <- "incidental submission"

names(me_mussel)[9:18] <- c("Margaritifera margaritifera", "Elliptio complanata",
                             "Alasmidonta undulata", "Alasmidonta varicosa",
                             "Pyganodon cataracta", "Anodonta implicata",
                             "Strophitus undulatus", "Leptodea ochracea",
                             "Lampsilis cariosa", "Lampsilis radiata")

me_mussel <- me_mussel %>% 
  pivot_longer(cols = 9:18, names_to = "scientific_name", values_to = "occurrence") %>% 
  mutate(live_occurrence = ifelse(occurrence == "X", 1, 0),
         shell_occurrence = ifelse(occurrence == "S", 1, 0),
         day = ifelse(is.na(DAY), 15, DAY), #assigned the 15th to the surveys without days
         month = ifelse(is.na(MONTH), 8, MONTH), #assigned august to surveys without months because this is the most common month
         date = dmy(paste(day, month, YEAR, by = "-")),
         UID = paste("ME", date, round(longitude, 4), round(latitude,4),  sep = "-"),
         scientific_name = tolower(scientific_name)) %>% 
  select(-DAY, -MONTH, -YEAR)

names(me_mussel)[1:17] <- tolower(names(me_mussel)[1:17])

name_conversion <- read.csv("musselsppnames.csv")

me_mussel <- left_join(me_mussel, name_conversion, by = "scientific_name")

me_mussel$project[me_mussel$project == 1] <- "MDIFW Surveys (Haskins & Siebenmann 1994)"
me_mussel$project[me_mussel$project == 2] <- "MDIFW Surveys (Haskins 1993, 1994)"
me_mussel$project[me_mussel$project == 3] <- "MDIFW Surveys (1993)"
me_mussel$project[me_mussel$project == 4] <- "MDIFW Surveys (1994)"
me_mussel$project[me_mussel$project == 5] <- "MDIFW Surveys (Haskins & Nedeau 1995)"
me_mussel$project[me_mussel$project == 6] <- "Miscellaneous Submissions"
me_mussel$project[me_mussel$project == 7] <- "MDIFW Surveys (Haskins & Hanlon 1996)"
me_mussel$project[me_mussel$project == 8] <- "MDIFW Surveys (Haskins & Welch 1997)"
me_mussel$project[me_mussel$project == 9] <- "MDIFW Surveys (deMaynadier & Haskins 1997)"
me_mussel$project[me_mussel$project == 10] <- "Edwards Dam Removal Surveys"
me_mussel$project[me_mussel$project == 11] <- "MDIFW Surveys (Nedeau & deMaynadier 1998)"
me_mussel$project[me_mussel$project == 12] <- "Maritimes & Northeast Pipeline Surveys"
me_mussel$project[me_mussel$project == 13] <- "Presumpscot River Hydro Project Survey 1997"



me_mussel_event <- me_mussel %>% 
  select(UID, date, latitude, longitude, waterbody, project, source) %>% 
  mutate(state = "ME") %>% 
  unique()
me_mussel_event$latitude[me_mussel_event$UID == "ME-1995-07-20--45.9667-70.1583"] <- 45.96667
me_mussel_event$longitude[me_mussel_event$UID == "ME-1995-07-20--45.9667-70.1583"] <- -70.15833

me_mussel_occurrence <- me_mussel %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence)
me_mussel_occurrence$common_name[me_mussel_occurrence$common_name == "brook floater (swollen wedgemussel)"] <- "brook floater"

me_mussel_method <- me_mussel %>% 
  select(UID, survey_method)

save(me_mussel_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/me_mussel_event.RData")
save(me_mussel_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/me_mussel_occurrence.RData")
save(me_mussel_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/me_mussel_method.RData")












###########################################################################

################# Rhode Island Dept of Env Management  ####################

###########################################################################



name_conversion <- read.csv("musselsppnames.csv")


#There are 5 files from Rhode island - comments show file description from Corey Pelletier

#Location data and species presence from Raithel and Hartenstine, 2006 (DIV=diversity, other abbreviations are for scientific names of species)
mus1 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/mussels.shp")

#Flat_R_160728, 160822, 160921- Shapefiles with targeted count data (Eastern Pearlshell) in the Flat River (Exeter, RI) for three different surveys days
mus2 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/FlatR_160921.shp")
mus2$date <- mdy(09212016)
mus3 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/FlatR_160822.shp")
mus3$date <- mdy(08222016)
mus4 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/Flat River_160728.shp")
mus4$date <- mdy(07282016)

#Freshwater Mussel Survey Database.xlsx- Data for mussel surveys conducted in 2020, total of 16 surveys
mus5spatial <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/2020 Mussel Survey Locations.shp")
mus5site <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/Freshwater Mussel Survey Database.xlsx",
                   sheet = 1)
mus5data <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/Freshwater Mussel Survey Database.xlsx",
                    sheet = 2)





#first clean the Raithel and Hartenstine, 2006 data

names(mus1)[14:23] <- c("alasmidonta undulata", "alasmidonta varicosa", "anodonta implicata",
                        "corbicula fluminea", "elliptio complanata", "lampsilis radiata", "ligumia nasuta",
                        "margaritifera margaritifera", "pyganodon cataracta", "strophitus undulatus") #confirm the COFL is corbicula fluminea

test <- mus1 %>% 
  mutate(UID = paste("RI", LOCATION, DATE1, sep = "_"),
         state = "RI",
         project = "Raithel and Hartenstine, 2006",
         source = "CoreyPelletier_RIDEM",
         longitude = unlist(map(geometry, 1)),
         latitude = unlist(map(geometry, 2)),
         goal = "total pick up", #based on paper - they inventoried all mussel spp, not just a particular spp
         waterbody = ifelse(grepl("pond$", HABTYPE), "lentic",      #identify lotic or lentic by searching for lakes or ponds at the end of the HABTYPE
                                                 ifelse(grepl("lake$", HABTYPE), "lentic", "lotic"))) %>% 
  pivot_longer(cols = 14:23, names_to = "scientific_name", values_to = "live_occurrence") %>% 
  rename("survey_method" = "SURVTYPE",
         date = DATE1) %>% 
  data.frame()

#need to fix the date to be 2006
test$date <- as.character(test$date)
test$date <- gsub("0001", "2006", test$date)
test$date <- ymd(test$date)

test <- left_join(test, name_conversion, by = "scientific_name")


#make the file sf object so it can be plotted spatially
#the lat long units are in the same coorinate system as the mus5spatial data so first assign that crs
#then project to the huc8 reference system so that it can be plotted along with the NHD data
test <- st_as_sf(x = test,                         
                coords = c("longitude", "latitude"),
                crs = st_crs(mus5spatial))

test <- st_transform(test, st_crs(huc8))

#extract the lat and long
test <- test %>% 
  mutate(longitude = unlist(map(geometry, 1)),
         latitude = unlist(map(geometry, 2)))

mus1_mussel_event <- test %>% 
  data.frame() %>% 
  select(UID, state, date, latitude, longitude, waterbody, project, source) %>% 
  unique()


mus1_mussel_occurrence <- test %>% 
  data.frame() %>%  
  select(UID, common_name, scientific_name, live_occurrence) %>% 
  unique() %>% 
  mutate(live_occurrence = as.numeric(live_occurrence))


mus1_mussel_method <- test %>% 
  data.frame() %>% 
  select(UID, survey_method, goal) %>% 
  unique()







#mus5site
names(mus5data)[8:22] <- as.vector(mus5data[1, 8:22])
names(mus5data)[23] <- "subsample"



test <- mus5data %>% 
  filter(!is.na(Year)) %>% 
  pivot_longer(cols = 8:22, names_to = "length_cm", values_to = "count2") %>% 
  rename(date = "Month/ Day", count = "Total #", "number_measure" = "Sub-Sample #", scientific_name = "Species") %>% 
  mutate(UID = paste("RI", Location, date, sep = "_"))

test$scientific_name[test$scientific_name == "Marg"] <- "margaritifera margaritifera"
test$scientific_name[test$scientific_name == "E Elliptio"] <- "elliptio complanata"
test$scientific_name[test$scientific_name == "Elliptio"] <- "elliptio complanata"

#make a presence absence file

#first make the file with all options at each site
UID <- unique(test$UID)
mussel <- unique(test$scientific_name)
absence <- data.frame('UID' = rep(UID, each = 3))
absence$scientific_name <- rep(mussel, times = 16)
absence <- absence %>% 
  filter(scientific_name != "N/A")
#then make a file with just presence
presence <- test %>% 
  select(UID, scientific_name) %>% 
  filter(scientific_name != "N/A") %>% 
  unique() %>% 
  mutate(live_occurrence = 1)

#antijoin to keep the rows that arent in the presence dat
keep <- anti_join(absence, presence, by = c("UID", "scientific_name"))
keep$live_occurrence <- 0

mus5_mussel_occurrence <- rbind(presence, keep)
mus5_mussel_occurrence <- left_join(mus5_mussel_occurrence, name_conversion, by = "scientific_name")
mus5_mussel_occurrence <- mus5_mussel_occurrence %>% 
  select(UID, common_name, scientific_name, live_occurrence) %>% 
  mutate(live_occurrence = as.numeric(live_occurrence))


#make count file
count <- test %>% 
  select(UID, scientific_name, count) %>% 
  unique() %>% 
  filter(scientific_name != "N/A")
names(keep)[3] <- "count"
count <- rbind(count, keep)#rbind the zeros
names(count)[3] <- "live_count"
mus5_mussel_count <- left_join(count, name_conversion, by = "scientific_name") %>% 
  select(UID, common_name, scientific_name, live_count)


#make a length file
length <- test %>% 
  filter(!is.na(count2)) %>% 
  select(UID, scientific_name, length_cm, count2)
n <-  length$count2
length <- length[rep(seq_len(nrow(length)), n),]
mus5_mussel_length <- left_join(length, name_conversion, by = "scientific_name") %>%
  mutate(length_mm = as.numeric(length_cm) * 10) %>% 
  select(UID, common_name, scientific_name, length_mm) 

#Event file

#join the site information
site <- mus5site %>% 
  mutate(UID = paste("RI", Location, Date, sep = "_")) %>% 
  filter(!is.na(`Start Latitude`)) %>% 
  select(Date, Location, Surveyors, `Survey Technique`, `Start Latitude`, `Start Longitude`, `Survey Length`, `Av. Wetted Width`, `Av. Depth`, UID, `Start Time`, `End Time`) %>% 
  rename(date = Date,
         survey_method = `Survey Technique`,
         latitude = `Start Latitude`,
         longitude = `Start Longitude`,
         reach_length_m = `Survey Length`,
         wet_width_avg_m = `Av. Wetted Width`,
         dept_avg_m = `Av. Depth`,
         start = `Start Time`,
         end = `End Time`) %>% 
  mutate(number_searchers = ifelse(Surveyors == "MP, BE", 2, 3),
         search_time_min = end - start,
         waterbody = "lotic",#based on spreadsheet - all are brooks or rivers
         goal = 'total pick up') #based on feeback from Corey

mus5_mussel_event <- left_join(site, test, by = c("UID", "date")) %>% 
  select(UID, date, latitude, longitude, waterbody) %>% 
  unique() %>% 
  mutate(state = "RI",
         project = "mussels2020",
         source = "CoreyPelletier_RIDEM")

#method file
mus5_mussel_method <- left_join(site, test, by = c("UID", "date")) %>% 
  data.frame() %>% 
  select(UID, survey_method, number_searchers, search_time_min, reach_length_m, wet_width_avg_m, dept_avg_m, goal) %>% 
  unique()





#mus2 mus3 mus4

flat <- rbind(mus2,mus3)
flat$Count <- NA
flat <- rbind(flat, mus4)

test <- st_transform(flat, st_crs(huc8))
test <- test %>% 
  mutate(longitude = unlist(map(geometry, 1)),
         latitude = unlist(map(geometry, 2)),
         UID = paste("RI", latitude, date),
         source = "CoreyPelletier_RIDEM",
         project = "flatriversurveys",
         common_name = "eastern pearlshell",
         scientific_name = "margaritifera margaritifera",
         state = "RI",
         live_occurrence = 1,
         live_count = Count,
         goal = "targeted",
         waterbody = "lotic") #all in the flat river 

mus2_mussel_event <- test %>% 
  data.frame() %>%  
  select(UID, state, date, latitude, longitude, waterbody, project, source) %>% 
  unique()

#dont make an absence file here becuase the surveys were targeted
mus2_mussel_occurrence <- test %>% 
  data.frame() %>% 
  select(UID, common_name, scientific_name, live_occurrence) %>% 
  unique()%>% 
  mutate(live_occurrence = as.numeric(live_occurrence))

mus2_mussel_count <- test %>% 
  data.frame() %>%  
  select(UID, common_name, scientific_name, live_count) %>% 
  filter(!is.na(live_count)) %>% 
  unique()

mus2_mussel_method <- test %>% 
  data.frame() %>% 
  select(UID, goal)





#join together the different RI data sources

#event
ri_mussel_event <- bind_rows(mus1_mussel_event, mus2_mussel_event, mus5_mussel_event)

#method
ri_mussel_method <- bind_rows(mus1_mussel_method, mus2_mussel_method, mus5_mussel_method)

#occurrence
ri_mussel_occurrence <- bind_rows(mus1_mussel_occurrence, mus2_mussel_occurrence, mus5_mussel_occurrence)

#count
ri_mussel_count <- bind_rows(mus2_mussel_count, mus5_mussel_count)

#length
ri_mussel_length <- mus5_mussel_length

save(ri_mussel_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ri_mussel_event.RData")
save(ri_mussel_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ri_mussel_occurrence.RData")
save(ri_mussel_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ri_mussel_method.RData")
save(ri_mussel_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ri_mussel_count.RData")
save(ri_mussel_length, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ri_mussel_length.RData")










######################################################################

####################### Vermont DEC mussel Data#######################

######################################################################


name_conversion <- read.csv("musselsppnames.csv")

#read in VT DEC data


vtdat <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/VTDEC-Mussel-Records-20220228.csv")
vtdat <- vtdat %>% 
  mutate(UID = paste("VT", Location, Latitude, Longitude, Date, sep = "_"),
         state = "VT",
         project = "VTDEC",
         source = "MichelleGraziosi-VTDEC",
         sp = ifelse(Species == "sp"  & Genus == "MARGARITIFERA", "margaritifera",
                     ifelse (Species == "sp" & Genus == "ELLIPTIO", "complanata", Species)),
         scientific_name = tolower(paste(Genus, sp, sep = " ")),
         live_occurrence = 1,
         date = mdy(Date),
         survey_method = "incidental observation") %>% 
  rename(latitude= Latitude, longitude = Longitude)


vtdat <- left_join(vtdat, name_conversion, by = "scientific_name")
vtdat$common_name[vtdat$scientific_name == "pyganodon grandis"] <- "giant floater"
vtdat$common_name[vtdat$scientific_name == "lasmigona compressa"] <- "creek heelsplitter"  
vtdat$common_name[vtdat$scientific_name == "lampsilis ovata"] <- "pocketbook"
vtdat$common_name[vtdat$scientific_name == "potamilus alatus"] <-  "pink heelsplitter" 
vtdat$common_name[vtdat$common_name == "squawfoot"] <-  "creeper"
vtdat$common_name[vtdat$common_name == "pocketbook mussel"] <-  "pocketbook"
vtdat$common_name[vtdat$common_name == "brook floater (swollen wedgemussel)"] <-  "brook floater"

vt_mussel_event <- vtdat %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  unique()

vt_mussel_occurrence <- vtdat %>% 
  select(UID, common_name, scientific_name, live_occurrence) %>% 
  unique() %>% 
  filter(!is.na(common_name))

vt_mussel_method <- vtdat %>% 
  select(UID, survey_method) %>% 
  unique()

#no method or count information here


#added in the recent 2022 data that Michelle sent
vtdat2022 <- read_xlsx("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/2022-Mussel-Survey-Records-VTDEC.xlsx")

vtdat2022 <- vtdat2022  %>% 
  rename(latitude= "Latitude (DD)", longitude = "Longitude (DD)") %>%  
  mutate(UID = paste("VT", Location, latitude,  longitude, Date, sep = "_"),
         state = "VT",
         project = "VTDEC",
         source = "MichelleGraziosi-VTDEC",
         scientific_name = tolower(paste(Genus, Species, sep = " ")),
         live_occurrence = 1,
         date = ymd(Date),
         survey_method = "incidental observation")


vtdat2022 <- left_join(vtdat2022, name_conversion, by = "scientific_name")
vtdat2022$common_name[vtdat2022$scientific_name == "lampsilis ovata"] <- "pocketbook"
vtdat2022$common_name[vtdat2022$scientific_name == "potamilus alatus"] <-  "pink heelsplitter" 
vtdat2022$common_name[vtdat2022$scientific_name == "lasmigona costata"] <-  "flutedshell" 

vt_mussel_event2022 <- vtdat2022 %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  unique()

vt_mussel_occurrence2022 <- vtdat2022 %>% 
  select(UID, common_name, scientific_name, live_occurrence) %>% 
  unique() 

vt_mussel_method2022 <- vtdat2022 %>% 
  select(UID, survey_method) %>% 
  unique()

#bind the two datasets together
vt_mussel_event <- rbind(vt_mussel_event, vt_mussel_event2022)
vt_mussel_occurrence <- rbind(vt_mussel_occurrence, vt_mussel_occurrence2022)
vt_mussel_method <- rbind(vt_mussel_method, vt_mussel_method2022)


save(vt_mussel_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_event.RData")
save(vt_mussel_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_occurrence.RData")
save(vt_mussel_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_method.RData")






#######################################################################

##################### Vermont PDF data ################################

#######################################################################


VTevent <- read_excel("C:/Users/jenrogers/OneDrive - University of Massachusetts/SpeciesDataExtraction/PDF Digitizing/VT/VT_PDF_Data_Extraction.xlsx",
                sheet = 1, col_types = c("text", "text", "numeric", "numeric", 
                                         "date", "text", "text", "text", "text", 
                                         "text", "numeric", "text", "text", "text", "text"))


VToccurr <- read_excel("C:/Users/jenrogers/OneDrive - University of Massachusetts/SpeciesDataExtraction/PDF Digitizing/VT/VT_PDF_Data_Extraction.xlsx",
                      sheet = 2, col_types = c("text", "text", "text", "numeric", "numeric", 
                                               "numeric", "numeric", "text"))

VTdem <- read_excel("C:/Users/jenrogers/OneDrive - University of Massachusetts/SpeciesDataExtraction/PDF Digitizing/VT/VT_PDF_Data_Extraction.xlsx",
                       sheet = 3, col_types = c("text", "text", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", "text"))


#tidy the event data
vt_mussel_pdf_event <- VTevent %>% 
  select(UID, State, date, latitude, longitude, waterbody) %>% 
  mutate(source = "MichelleGraziosi-VTDEC-PDFreports",
         project = "variable mussel reports")

names(vt_mussel_pdf_event)[2:8] <- tolower(names(vt_mussel_pdf_event)[2:8])



#tidy the occurrence data
vt_mussel_pdf_occurrence <- VToccurr
vt_mussel_pdf_occurrence$live_occurrence[103:620] <- ifelse(vt_mussel_pdf_occurrence$live_count[103:620] >0, 1, 0 )

vt_mussel_pdf_occurrence <- vt_mussel_pdf_occurrence %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence) %>% 
  mutate(common_name = tolower(common_name),
         scientific_name = tolower(scientific_name))
#correct the name spellings
vt_mussel_pdf_occurrence$common_name[vt_mussel_pdf_occurrence$common_name == "squawfoot"] <- "creeper"
vt_mussel_pdf_occurrence$common_name[vt_mussel_pdf_occurrence$common_name == "eastern elliptios"] <- "eastern elliptio"
vt_mussel_pdf_occurrence$common_name[vt_mussel_pdf_occurrence$common_name == "eastern lamp mussel"] <- "eastern lampmussel"
vt_mussel_pdf_occurrence$common_name[vt_mussel_pdf_occurrence$common_name == "alewife floaters"] <- "alewife floater"
vt_mussel_pdf_occurrence$common_name[vt_mussel_pdf_occurrence$common_name == "fluted-shell"] <- "flutedshell"
vt_mussel_pdf_occurrence$common_name[vt_mussel_pdf_occurrence$common_name == "fluted shell"] <- "flutedshell"
vt_mussel_pdf_occurrence$common_name[vt_mussel_pdf_occurrence$common_name == "pocketbook mussel"] <- "pocketbook"
vt_mussel_pdf_occurrence$common_name[vt_mussel_pdf_occurrence$common_name == "eastern pearl mussel"] <- "eastern pearlshell"
vt_mussel_pdf_occurrence$scientific_name[vt_mussel_pdf_occurrence$scientific_name == "strophitus undulates"] <- "strophitus undulatus"
vt_mussel_pdf_occurrence$scientific_name[vt_mussel_pdf_occurrence$scientific_name == "elliptic complanata"] <- "elliptio complanata"
vt_mussel_pdf_occurrence$scientific_name[vt_mussel_pdf_occurrence$scientific_name == "anodontoidesferussacianus"] <- "anodontoides ferussacianus"
vt_mussel_pdf_occurrence$scientific_name[vt_mussel_pdf_occurrence$scientific_name == "lasmigonacompressa"] <- "lasmigona compressa"


#add scientific  names to the spp that only have common names
temp <-vt_mussel_pdf_occurrence %>% 
  filter(is.na(scientific_name))
unique(temp$common_name)

temp$scientific_name[temp$common_name == "alewife floater"] <- "anodonta implicata"
temp$scientific_name[temp$common_name == "brook floater"] <- "alasmidonta varicosa"
temp$scientific_name[temp$common_name == "creek heelsplitter"] <- "lasmigona compressa"
temp$scientific_name[temp$common_name == "creeper"] <- "strophitus undulatus"
temp$scientific_name[temp$common_name == "cylindrical papershell"] <- "anodontoides ferussacianus"
temp$scientific_name[temp$common_name == "eastern elliptio"] <- "elliptio complanata"
temp$scientific_name[temp$common_name == "eastern floater"] <- "pyganodon cataracta"
temp$scientific_name[temp$common_name == "eastern lampmussel"] <- "lampsilis radiata"
temp$scientific_name[temp$common_name == "eastern pearlshell"] <- "margaritifera margaritifera"
temp$scientific_name[temp$common_name == "elktoe"] <- "alasmidonta marginata"
temp$scientific_name[temp$common_name == "flutedshell"] <- "lasmigona costata"
temp$scientific_name[temp$common_name == "fragile papershell"] <- "leptodea fragilis"
temp$scientific_name[temp$common_name == "giant floater"] <- "pyganodon grandis"
temp$scientific_name[temp$common_name == "pink heelsplitter"] <- "potamilus alatus"
temp$scientific_name[temp$common_name == "pocketbook"] <- "lampsilis ovata "
temp$scientific_name[temp$common_name == "triangle floater"] <- "alasmidonta undulata"

#add common names to the ones with scientific names
temp2 <-vt_mussel_pdf_occurrence %>% 
  filter(is.na(common_name))
sort(unique(temp2$scientific_name))

temp2$common_name[temp2$scientific_name == "alasmidonta undulata"] <- "triangle floater"
temp2$common_name[temp2$scientific_name == "alasmidonta varicosa"] <- "brook floater"
temp2$common_name[temp2$scientific_name == "elliptio complanata"] <- "eastern elliptio"
temp2$common_name[temp2$scientific_name == "lampsilis ovata"] <- "pocketbook"
temp2$common_name[temp2$scientific_name == "lampsilis radiata"] <- "eastern lampmussel"
temp2$common_name[temp2$scientific_name == "lasmigona costata"] <- "flutedshell"
temp2$common_name[temp2$scientific_name == "margaritifera margaritifera"] <- "eastern pearlshell"
temp2$common_name[temp2$scientific_name == "strophitus undulatus"] <- "creeper"

#make a df with the rows that originally had both scientific and common names
temp3 <- vt_mussel_pdf_occurrence %>% 
  filter(!is.na(scientific_name), !is.na(common_name))

#combine the three datasets back to one occurence dataset
vt_mussel_pdf_occurrence <- rbind(temp, temp2, temp3)

rm(temp, temp2, temp3)



#tidy the count data
vt_mussel_pdf_count <- VToccurr %>% 
  select(UID, common_name, scientific_name, live_count, shell_count)%>% 
  mutate(common_name = tolower(common_name),
         scientific_name = tolower(scientific_name))
 #fix the spelling
vt_mussel_pdf_count$common_name[vt_mussel_pdf_count$common_name == "squawfoot"] <- "creeper"
vt_mussel_pdf_count$common_name[vt_mussel_pdf_count$common_name == "eastern elliptios"] <- "eastern elliptio"
vt_mussel_pdf_count$common_name[vt_mussel_pdf_count$common_name == "eastern lamp mussel"] <- "eastern lampmussel"
vt_mussel_pdf_count$common_name[vt_mussel_pdf_count$common_name == "alewife floaters"] <- "alewife floater"
vt_mussel_pdf_count$common_name[vt_mussel_pdf_count$common_name == "fluted-shell"] <- "flutedshell"
vt_mussel_pdf_count$common_name[vt_mussel_pdf_count$common_name == "fluted shell"] <- "flutedshell"
vt_mussel_pdf_count$common_name[vt_mussel_pdf_count$common_name == "pocketbook mussel"] <- "pocketbook"
vt_mussel_pdf_count$common_name[vt_mussel_pdf_count$common_name == "eastern pearl mussel"] <- "eastern pearlshell"
vt_mussel_pdf_count$scientific_name[vt_mussel_pdf_count$scientific_name == "strophitus undulates"] <- "strophitus undulatus"
vt_mussel_pdf_count$scientific_name[vt_mussel_pdf_count$scientific_name == "elliptic complanata"] <- "elliptio complanata"
vt_mussel_pdf_count$scientific_name[vt_mussel_pdf_count$scientific_name == "anodontoidesferussacianus"] <- "anodontoides ferussacianus"
vt_mussel_pdf_count$scientific_name[vt_mussel_pdf_count$scientific_name == "lasmigonacompressa"] <- "lasmigona compressa"

#add scientific  names to the spp that only have common names
temp <-vt_mussel_pdf_count %>% 
  filter(is.na(scientific_name))
sort(unique(temp$common_name))

temp$scientific_name[temp$common_name == "alewife floater"] <- "anodonta implicata"
temp$scientific_name[temp$common_name == "brook floater"] <- "alasmidonta varicosa"
temp$scientific_name[temp$common_name == "creek heelsplitter"] <- "lasmigona compressa"
temp$scientific_name[temp$common_name == "creeper"] <- "strophitus undulatus"
temp$scientific_name[temp$common_name == "cylindrical papershell"] <- "anodontoides ferussacianus"
temp$scientific_name[temp$common_name == "eastern elliptio"] <- "elliptio complanata"
temp$scientific_name[temp$common_name == "eastern floater"] <- "pyganodon cataracta"
temp$scientific_name[temp$common_name == "eastern lampmussel"] <- "lampsilis radiata"
temp$scientific_name[temp$common_name == "eastern pearlshell"] <- "margaritifera margaritifera"
temp$scientific_name[temp$common_name == "elktoe"] <- "alasmidonta marginata"
temp$scientific_name[temp$common_name == "flutedshell"] <- "lasmigona costata"
temp$scientific_name[temp$common_name == "fragile papershell"] <- "leptodea fragilis"
temp$scientific_name[temp$common_name == "giant floater"] <- "pyganodon grandis"
temp$scientific_name[temp$common_name == "pink heelsplitter"] <- "potamilus alatus"
temp$scientific_name[temp$common_name == "pocketbook"] <- "lampsilis ovata "
temp$scientific_name[temp$common_name == "triangle floater"] <- "alasmidonta undulata"

#add common  names to the spp that only have scientific names
temp2 <-vt_mussel_pdf_count %>% 
  filter(is.na(common_name))
sort(unique(temp2$scientific_name))

temp2$common_name[temp2$scientific_name == "alasmidonta undulata"] <- "triangle floater"
temp2$common_name[temp2$scientific_name == "alasmidonta varicosa"] <- "brook floater"
temp2$common_name[temp2$scientific_name == "elliptio complanata"] <- "eastern elliptio"
temp2$common_name[temp2$scientific_name == "lampsilis ovata"] <- "pocketbook"
temp2$common_name[temp2$scientific_name == "lampsilis radiata"] <- "eastern lampmussel"
temp2$common_name[temp2$scientific_name == "lasmigona costata"] <- "flutedshell"
temp2$common_name[temp2$scientific_name == "margaritifera margaritifera"] <- "eastern pearlshell"
temp2$common_name[temp2$scientific_name == "strophitus undulatus"] <- "creeper"

#make a df with the rows that originally had both scientific and common names
temp3 <- vt_mussel_pdf_count %>% 
  filter(!is.na(scientific_name), !is.na(common_name))

#combine the three datasets back to one count dataset
vt_mussel_pdf_count <- rbind(temp, temp2, temp3)


rm(temp, temp2, temp3)




#tidy the mussel length data
vt_mussel_pdf_length <- VTdem %>% 
  select(-QC)

vt_mussel_pdf_length <- vt_mussel_pdf_length %>% 
  mutate(scientific_name = ifelse(common_name == "brook floater", 
                                  "alasmidonta varicosa",  
                                  "margaritifera margaritifera")) %>% 
  select(UID, common_name, scientific_name, length_mm, mean_length_mm, min_length_mm, max_length_mm, sd_mm)



#tidy the method data - the search time and average wetted width are not terribily reliable... probabilty should just remove this data
vt_mussel_pdf_method <- VTevent %>% 
  select(UID, survey_method, number_searchers, survey_duration_hr, reach_length_surveyed_m, wet_width_avg_m, survey_goal) %>% 
  rename(reach_length_m = reach_length_surveyed_m,
         goal = survey_goal,
         "search time" = survey_duration_hr) #these times and units are not reliatble, will likely want to exclude them
vt_mussel_pdf_method$reach_length_m <- gsub("m", "", vt_mussel_pdf_method$reach_length_m)
#two reach lengths were recorded in ft, change to meters
vt_mussel_pdf_method$reach_length_m[vt_mussel_pdf_method$reach_length_m == "250ft"] <- 76.2
vt_mussel_pdf_method$reach_length_m[vt_mussel_pdf_method$reach_length_m == "250 ft"] <- 76.2
vt_mussel_pdf_method$reach_length_m <- as.numeric(vt_mussel_pdf_method$reach_length_m)

#the wetted widths are all in m so just remove the m and make it numeric
vt_mussel_pdf_method$wet_width_avg_m <- gsub("m", "", vt_mussel_pdf_method$wet_width_avg_m)
vt_mussel_pdf_method$wet_width_avg_m <- as.numeric(vt_mussel_pdf_method$wet_width_avg_m)




save(vt_mussel_pdf_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_pdf_event.RData")
save(vt_mussel_pdf_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_pdf_occurrence.RData")
save(vt_mussel_pdf_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_pdf_count.RData")
save(vt_mussel_pdf_length, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_pdf_length.RData")
save(vt_mussel_pdf_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_pdf_method.RData")




###########################################################

################  NH natural heritage #####################

###########################################################


#read in the source feature excel, the long fields EO data excel,  the source feature shape file, and the EO shape file

# src1 <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH Mussel Data/Source-Features-Visits-table.xlsx")
# src2 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH Mussel Data/MusselSourceFeatures_NHB_June2022.shp")
# eo1 <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH Mussel Data/LongFields_nhb_2022.xlsx")
# eo2 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH Mussel Data/musselEOs_nhb_Feb2022.shp")
# 
# src1 <- src1 %>% 
#   select(1:3)
# 
# src2keep <- src2 %>% 
#   data.frame() %>% 
#   select(EO_ID, SOURCE_FEA, SOURCE_F_1)
# 
# src <- left_join(src1, src2keep, by = c("EO_ID", "SOURCE_FEA")) 
# 
# 
# eo1 <- eo1 %>% 
#   select(1:3)
# 
# eo2keep <- eo2 %>% 
#   data.frame() %>% 
#   select(EO_ID, SNAME, SCOMNAME) 
# 
# eo <- left_join(eo1, eo2keep, by  = "EO_ID")
# 
# nh <- left_join(src, eo, by = "EO_ID") %>% 
#   select(EO_ID, SOURCE_FEA, VISIT_NOTES, SOURCE_F_1, SNAME, SCOMNAME, EO_DATA, GEN_DESC)
# 
# write_csv(nh, "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH Mussel Data/nh_heritage_data.csv")
# #this is the file that we will edit




#this file was edited in my ondrive folder




nhmussel <- read_xlsx("C:/Users/jenrogers/OneDrive - University of Massachusetts/SpeciesDataExtraction/Heritage Spreadsheet/nh_heritage_data_extraction_JR.xlsx")
nhmussel <- nhmussel %>% 
  select(EO_ID, SOURCE_FEA, SOURCE_F_1, SNAME, SCOMNAME, date, live_count, shell_count, 
         live_occurrence, shell_occurrence, live_length_mm, reach_length_m, reach_width, 
         search_time)


#left join the edited data to the source shape file by the EO_ID, SOURCE_FEA
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp") #get the polygon to crop with
src2 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/NH Mussel Data/MusselSourceFeatures_NHB_June2022.shp")

shp <- st_transform(src2, st_crs(huc8)) %>% 
  select(EO_ID, SOURCE_FEA, SOURCE_F_1) %>% 
  st_centroid() %>% 
  mutate(longitude = unlist(map(geometry, 1)),
         latitude = unlist(map(geometry, 2))) 

dat <- left_join(nhmussel, shp, by = c("EO_ID", "SOURCE_FEA"))

#need to add in the common names or the scientific names
dat$SNAME <- tolower(dat$SNAME)
dat$SCOMNAME <- tolower(dat$SCOMNAME)


dat$SNAME[dat$SCOMNAME == "triangle floater"] <- "alasmidonta undulata"
dat$SNAME[dat$SCOMNAME == "eastern elliptio"] <- "elliptio complanata"
dat$SNAME[dat$SCOMNAME == "asian clam"] <- "corbicula fluminea"
dat$SNAME[dat$SCOMNAME == "eastern floater"] <- "pyganodon cataracta"
dat$SNAME[dat$SNAME == "a. undulata"] <- "alasmidonta undulata"
dat$SNAME[dat$SNAME == "strophitis undulatus"] <- "strophitus undulatus"
dat$SCOMNAME[dat$SCOMNAME == "dwarf wedge mussel"] <- "dwarf wedgemussel"
dat$SCOMNAME[dat$SCOMNAME == "eastern pond mussel"] <- "eastern pondmussel"

dat$SCOMNAME[dat$SNAME == "alasmidonta undulata"] <- "triangle floater"
dat$SCOMNAME[dat$SNAME == "elliptio complanata"] <- "eastern elliptio"
dat$SCOMNAME[dat$SNAME == "lampsilis radiata"] <- "eastern lampmussel"
dat$SCOMNAME[dat$SNAME == "pyganodon cataracta"] <- "eastern floater"
dat$SCOMNAME[dat$SNAME == "strophitus undulatus"] <- "creeper"

#tidy method columns

dat <- dat %>% 
  rename(search_time_hr = search_time,
         scientific_name = SNAME,
         common_name = SCOMNAME,
         length_mm = live_length_mm,
         wet_width_avg_m = reach_width)

dat$length_mm <- gsub("mm", "", dat$length_mm)

dat$reach_length_m <- gsub("m", "", dat$reach_length_m)
dat$reach_length_m <- gsub(" ", "", dat$reach_length_m)
dat$wet_width_avg_m <- gsub("m", "", dat$wet_width_avg_m)

dat$search_time_hr <- gsub("hr", "", dat$search_time_hr)
dat$search_time_hr <- gsub("", "", dat$search_time_hr)
dat$search_time_hr <- gsub(" s", "", dat$search_time_hr)

dat <- dat %>% 
  mutate(UID = paste("NH", dat$EO_ID, dat$SOURCE_FEA, dat$date, sep = "_"),
         state = "NH",
         project = "nh_NatureServeDatabase",
         source = "AmyLamb",
         date2 = paste("6/1/", date, sep = ""),
         date3 = mdy(date2),
         search_time_min = as.numeric(search_time_hr)*60,
         reach_length_m = as.numeric(reach_length_m),
         length_mm = as.numeric(length_mm),
         wet_width_avg_m = as.numeric(wet_width_avg_m)) %>% 
  select(-date, -date2) %>% 
  filter(!is.na(date3)) %>% 
  rename(date = date3)



nh_mussel_event <- dat %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  filter(!is.na(latitude))

nh_mussel_occurrence <- dat %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence) %>% 
  filter(!is.na(live_occurrence) | !is.na(shell_occurrence))

nh_mussel_count <- dat %>% 
  select(UID, common_name, scientific_name, live_count, shell_count) %>% 
  filter(!is.na(live_count) | !is.na(shell_count))


nh_mussel_length <- dat %>% 
  select(UID, common_name, scientific_name, length_mm)%>% 
  filter(!is.na(length_mm))

nh_mussel_method <- dat %>% 
  select(UID, reach_length_m, wet_width_avg_m, search_time_min)%>% 
  filter(!is.na(reach_length_m) | !is.na(wet_width_avg_m) | !is.na(search_time_min))


save(nh_mussel_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/nh_mussel_event.RData")
save(nh_mussel_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/nh_mussel_occurrence.RData")
save(nh_mussel_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/nh_mussel_count.RData")
save(nh_mussel_length, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/nh_mussel_length.RData")
save(nh_mussel_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/nh_mussel_method.RData")






#######################################################################

##################### Connecticut PDF data ############################

#######################################################################





CTevent <- read_excel("C:/Users/jenrogers/OneDrive - University of Massachusetts/SpeciesDataExtraction/PDF Digitizing/CT/CT_PDF_Data_Extraction_v2.xlsx",
                      sheet = 5, 
                      range = cell_cols("A:AB"),
                      col_types = c("text", "text", "text", "text", "text", 
                                               "numeric", "numeric", "text",
                                               "date", "date", "date", "date", "date", "date", 
                                    "text", "text", "text", "text", 
                                               "numeric", "numeric", "text", "numeric","numeric", 
                                               "text", "text", "text", "text", "text"))


CToccurr <- read_excel("C:/Users/jenrogers/OneDrive - University of Massachusetts/SpeciesDataExtraction/PDF Digitizing/CT/CT_PDF_Data_Extraction_v2.xlsx",
                       sheet = 6, 
                       range = cell_cols("A:I"),
                       col_types = c("text", "text", "text", "numeric", "numeric", 
                                                "numeric", "text", "numeric","text"))

CTdem <- read_excel("C:/Users/jenrogers/OneDrive - University of Massachusetts/SpeciesDataExtraction/PDF Digitizing/CT/CT_PDF_Data_Extraction_v2.xlsx",
                    sheet = 7, 
                    range = cell_cols("A:G"),
                    col_types = c("text", "text", "text", "numeric", 
                                  "text", "text", "text"))






#event
ct_mussel_event <- CTevent %>% 
  select(UID, STATE, LATITUDE, LONGITUDE, DATE, WATERBODY, TITLE) %>% 
  mutate(source = "LauraSaucierCTDEEP_PDFreports") %>% 
  rename(project = TITLE) %>% 
  filter(!is.na(LATITUDE),
         !is.na(UID))

  
names(ct_mussel_event)[2:8] <- tolower(names(ct_mussel_event)[2:8])
colSums(is.na(ct_mussel_event))


#tidy occurrence and count columns
names(CToccurr)[2:9] <- tolower(names(CToccurr)[2:9])
CToccurr$common_name <- tolower(CToccurr$common_name)
CToccurr$scientific_name <- tolower(CToccurr$scientific_name)

#add zeros to the count columns if the occurrence column was 0
CToccurr$live_count[CToccurr$live_occurrence == 0] <- 0
CToccurr$shell_count[CToccurr$shell_occurrence == 0] <- 0

#fix the common names
CToccurr$common_name[CToccurr$common_name == "easatern elliptio"] <- "eastern elliptio"
CToccurr$common_name[CToccurr$common_name == "eastern lampussel"] <- "eastern lampmussel"
CToccurr$common_name[CToccurr$common_name == "eastern pearshell"] <- "eastern pearlshell"
CToccurr$common_name[CToccurr$common_name == "tidewater muckets"] <- "tidewater mucket"
CToccurr$common_name[CToccurr$common_name == "traingle floater"] <- "triangle floater"

#tidy the scientific names
CToccurr$scientific_name[CToccurr$scientific_name == "alamidonta heterodon"] <- "alasmidonta heterodon"
CToccurr$scientific_name[CToccurr$scientific_name == "alasmidont varicosa"] <- "alasmidonta varicosa"
CToccurr$scientific_name[CToccurr$scientific_name == "elliptio complanta"] <- "elliptio complanata"
CToccurr$scientific_name[CToccurr$scientific_name == "ellipto complanata"] <- "elliptio complanata"
CToccurr$scientific_name[CToccurr$scientific_name == "lampsilis radiata radiata"] <- "lampsilis radiata"
CToccurr$scientific_name[CToccurr$scientific_name == "lampsilis radiatea radiata"] <- "lampsilis radiata"
CToccurr$scientific_name[CToccurr$scientific_name == "lamsilis radiata"] <- "lampsilis radiata"
CToccurr$scientific_name[CToccurr$scientific_name == "leptodea orchracea"] <- "leptodea ochracea"
CToccurr$scientific_name[CToccurr$scientific_name == "ligunia nasuta"] <- "ligumia nasuta"
CToccurr$scientific_name[CToccurr$scientific_name == "margatifera margatifera"] <- "margaritifera margaritifera"
CToccurr$scientific_name[CToccurr$scientific_name == "strophhitus undulatus"] <- "strophitus undulatus"
CToccurr$scientific_name[CToccurr$scientific_name == "strophilus undulatus"] <- "strophitus undulatus"

#occurrence
ct_mussel_occurrence <- CToccurr %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence)

#in the rows where no mussels were found, make them a 0 occurrence for eliptio. Eventually we will add in zeros for all the spp, but for now we'll just do eliptio as a placeholder
ct_mussel_occurrence$scientific_name[ct_mussel_occurrence$common_name == "no mussels"] <- "elliptio complanata"
ct_mussel_occurrence$common_name[ct_mussel_occurrence$common_name == "no mussels"] <- "eastern elliptio"


#count
ct_mussel_count <- CToccurr %>% 
  select(UID, common_name, scientific_name, live_count, shell_count) %>% 
  filter(!is.na(live_count) | !is.na(shell_count))
#add in eastern elliptio in the rows where 0 live and shell were found as a placeholder. In the final version well add in zeros for all spp where they werent recorded
ct_mussel_count$scientific_name[ct_mussel_count$common_name == "no mussels"] <- "elliptio complanata"
ct_mussel_count$common_name[ct_mussel_count$common_name == "no mussels"] <- "eastern elliptio"



#fix the counts that aren't numberic
ct_mussel_count$live_count[ct_mussel_count$live_count == "100+"] <- 100
ct_mussel_count$live_count[ct_mussel_count$live_count == "500+"] <- 500
ct_mussel_count$live_count[ct_mussel_count$live_count == "several hundred"] <- 200
ct_mussel_count$live_count[ct_mussel_count$live_count == "thousands"] <- 1000

ct_mussel_count$live_count <- as.numeric(ct_mussel_count$live_count)


#mussel lengths
ct_mussel_length <- CTdem %>% 
  filter(LIVE_MUSSEL == TRUE) %>% 
  select(UID, COMMON_NAME, SCIENTIFIC_NAME, LENGTH_mm)

names(ct_mussel_length)[2:4] <- tolower(names(ct_mussel_length)[2:4])
ct_mussel_length$common_name <- tolower(ct_mussel_length$common_name)
ct_mussel_length$scientific_name <- tolower(ct_mussel_length$scientific_name)

ct_mussel_length$scientific_name[ct_mussel_length$scientific_name == "margatifera margatifera"] <- "margaritifera margaritifera"
ct_mussel_length$scientific_name[ct_mussel_length$scientific_name == "alamidonta heterodon"] <- "alasmidonta heterodon"

#add in data on average length
temp <- CToccurr %>% 
  select(UID, common_name, scientific_name, avg_length) %>% 
  filter(!is.na(avg_length)) %>% 
  rename(mean_length_mm = avg_length)
temp$common_name <- tolower(temp$common_name)

#in some cases the average length will be repeated if individal measurements were also taken
ct_mussel_length <- full_join(ct_mussel_length, temp, by = c("UID", "scientific_name", "common_name"))
rm(temp)


#method
ct_mussel_method <- CTevent %>% 
  select(UID, SURVEY_METHOD, NUMBER_SEARCHERS, SURVEY_DURATION, REACH_LENGTH, WET_WIDTH, SURVEY_GOAL) %>% 
  filter(!is.na(UID))

ct_mussel_method$SURVEY_METHOD <- tolower(ct_mussel_method$SURVEY_METHOD)

ct_mussel_method$SURVEY_METHOD[ct_mussel_method$SURVEY_METHOD == "bucket & snorkel"] <- "snorkel & bucket"
ct_mussel_method$SURVEY_METHOD[ct_mussel_method$SURVEY_METHOD == "scuba & snorkel"] <- "snorkel & scuba"
ct_mussel_method$SURVEY_METHOD[ct_mussel_method$SURVEY_METHOD == "quadrat by scuba"] <- "scuba"
ct_mussel_method$SURVEY_METHOD[ct_mussel_method$SURVEY_METHOD == "visual of banks"] <- "bank survey"



ct_mussel_method <- ct_mussel_method %>% 
  rename(wet_width_avg_m = WET_WIDTH,
         reach_length_m = REACH_LENGTH) %>% 
  mutate(goal = ifelse(SURVEY_GOAL %in% c("all species", "mussels", "macros", "mussels & macros" ,
                                          "macrso", "mussles & macros", "all mussel species"), "total pick-up", 
                       ifelse(is.na(SURVEY_GOAL), NA, "targeted"))) %>% 
  rename(target = SURVEY_GOAL) 

ct_mussel_method$target[ct_mussel_method$target == "all species"] <- "all mussel species"
ct_mussel_method$target[ct_mussel_method$target == "mussels"] <- "all mussel species"
ct_mussel_method$target[ct_mussel_method$target == "macros"] <- "macroinvertebrates"
ct_mussel_method$target[ct_mussel_method$target == "macrso"] <- "macroinvertebrates"
ct_mussel_method$target[ct_mussel_method$target == "mussles & macros"] <- "macroinvertebrates"
ct_mussel_method$target[ct_mussel_method$target == "mussels & macros"] <- "macroinvertebrates"

ct_mussel_method$target <- tolower(ct_mussel_method$target)
  
ct_mussel_method$target[ct_mussel_method$target == "state-listed species"] <- "listed species"
ct_mussel_method$target[ct_mussel_method$target == "state & federally listed species"] <- "listed species"
ct_mussel_method$target[ct_mussel_method$target == "endangered, threatened, or special concern species"] <- "listed species"
ct_mussel_method$target[ct_mussel_method$target == "rare mussels"] <- "listed species"
ct_mussel_method$target[ct_mussel_method$target == "state-listed mussels"] <- "listed species"
ct_mussel_method$target[ct_mussel_method$target == "translocation follow-up"] <- "relocation"
ct_mussel_method$target[ct_mussel_method$target == "mussel relocation"] <- "relocation"

names(ct_mussel_method)[2:8] <- tolower(names(ct_mussel_method)[2:8])


save(ct_mussel_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ct_mussel_event.RData")
save(ct_mussel_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ct_mussel_occurrence.RData")
save(ct_mussel_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ct_mussel_count.RData")
save(ct_mussel_length, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ct_mussel_length.RData")
save(ct_mussel_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ct_mussel_method.RData")





#################################################################

#################  Vermont natural heritage  ###################

###############################################################



st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb")

rare <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb", layer = "VT_RTE_Animals")
uncom <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb", layer = "VT_Uncommon_animals")


#select just the freshwater mussels
mus1 <- rare %>% 
  filter(INFORMAL_TAXONOMY == 'Freshwater Mussels') %>% 
  select(EO_ID, S_NAME, ENGLISH,SURVEYDATE, EO_DATA, GEN_DESC ) %>% 
  mutate(EO_ID = as.character(EO_ID))

mus2 <- uncom %>% 
  filter(INFORMAL_TAXONOMY == 'Freshwater Mussels') %>% 
  mutate(EO_ID = paste(ENGLISH, Shape_Length, sep = "_")) %>% 
  select(EO_ID, S_NAME, ENGLISH, SURVEYDATE, EO_DATA, GEN_DESC, VISITS)


mus3 <- bind_rows(mus1, mus2) %>% 
  data.frame() %>% 
  select(-Shape)

write.csv(mus3, "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/heritage_data.csv")


#### VT nature serve data was editted in excel in my onedrive, and then saved to the spp_data folder, and then loaded back in below
mus4 <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/VT_heritage_data_extraction_JR.csv") %>% 
  select(EO_ID, S_NAME, ENGLISH, date, live_count,  shell_count, live_occurrence, shell_occurrence, live_length_mm, 
         survey_method, reach_length_m, reach_width, goal, search_time, number_of_searchers, sex) 


mus5 <- mus1 %>% 
  left_join(mus4, by = c("EO_ID", "S_NAME", "ENGLISH")) %>% 
  select(EO_ID, S_NAME, ENGLISH, date, live_count,  shell_count, live_occurrence, shell_occurrence, live_length_mm, 
         survey_method, reach_length_m, reach_width, goal, search_time, number_of_searchers, sex)

mus6 <- mus2 %>% 
  left_join(mus4, by = c("EO_ID", "S_NAME", "ENGLISH")) %>% 
  select(EO_ID, S_NAME, ENGLISH, date, live_count,  shell_count, live_occurrence, shell_occurrence, live_length_mm, 
         survey_method, reach_length_m, reach_width, goal, search_time, number_of_searchers, sex)

names(mus5) == names(mus6)

mus7 <- rbind(mus5, mus6)
mus7 <- st_transform(mus7, st_crs(huc8))

mus8 <- mus7 %>% 
  mutate(state = "VT",
         project = "VT_NatureServeDatabase",
         source = "Everett Marshall",
         date = mdy_hm(date),
         live_count = as.numeric(live_count),
         longitude = map_dbl(Shape, ~st_point_on_surface(.x)[[1]]),
         latitude = map_dbl(Shape, ~st_point_on_surface(.x)[[2]]),
         scientific_name = tolower(S_NAME),
         common_name = tolower(ENGLISH),
         UID = paste("VT", EO_ID, latitude, longitude, scientific_name, date, sep = "_")) %>% 
  unique()


vt_mussel_event_NatureServe <- mus8 %>%
  data.frame() %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  filter(!is.na(latitude))

vt_mussel_occurrence_NatureServe <- mus8 %>% 
  data.frame() %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence) %>% 
  filter(!is.na(live_occurrence) | !is.na(shell_occurrence))


vt_mussel_count_NatureServe <- mus8 %>% 
  data.frame() %>% 
  select(UID, common_name, scientific_name, live_count, shell_count) %>% 
  filter(!is.na(live_count) | !is.na(shell_count))



save(vt_mussel_event_NatureServe, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_NatureServe_event.RData")
save(vt_mussel_occurrence_NatureServe, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_NatureServe_occurrence.RData")
save(vt_mussel_count_NatureServe, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/vt_mussel_NatureServe_count.RData")







##################################################################

################ join all the datasets together ##################

##################################################################




#read in data

#load in datasets
path <-  "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel"
files <- list.files(path = path)

for (i in 1:length(files)){
  
  load(paste(path, files[i], sep = "/"))  #load files
  
}


#bind event data
me_mussel_event$project <- as.character(me_mussel_event$project) #shoudl go back to the maine tidying and add in the text for each project integer, for now, just do this

all_mussel_event <- bind_rows(ma_mussel_event, me_mussel_event, ri_mussel_event, vt_mussel_event, 
                              nh_mussel_event, vt_mussel_pdf_event, ct_mussel_event, vt_mussel_event_NatureServe,
                              ma_mussel_natureserve_poly_event, ma_mussel_natureserve_line_event, ma_mussel_natureserve_point_event)

#bind occurrence data

all_mussel_occurrence <- bind_rows(ma_mussel_occurrence, me_mussel_occurrence, ri_mussel_occurrence, 
                                   vt_mussel_occurrence, nh_mussel_occurrence, vt_mussel_pdf_occurrence,
                                   ct_mussel_occurrence, vt_mussel_occurrence_NatureServe,
                                   ma_mussel_natureserve_poly_occurrence, ma_mussel_natureserve_line_occurrence,
                                   ma_mussel_natureserve_point_occurrence) #there are 20 repeated values here.. need to fix
all_mussel_occurrence$scientific_name <- str_trim(all_mussel_occurrence$scientific_name)
all_mussel_occurrence$common_name[all_mussel_occurrence$common_name == "brook floater (swollen wedgemussel)"] <- "brook floater"
all_mussel_occurrence$scientific_name[all_mussel_occurrence$common_name == "eastern floater" &
                                        all_mussel_occurrence$scientific_name == "alasmidonta undulata"] <- "pyganodon cataracta"
all_mussel_occurrence$scientific_name[all_mussel_occurrence$common_name == "alewife floater"] <- "anodonta implicata"
all_mussel_occurrence$scientific_name[all_mussel_occurrence$common_name == "eastern pondmussel"] <- "ligumia nasuta"




#there is a mismatch between common and sci name alasmidonta undulata is called eastern floater once in CT and MA

#bind count data

all_mussel_count <- bind_rows(ma_mussel_count, ri_mussel_count, nh_mussel_count, vt_mussel_pdf_count, 
                              ct_mussel_count, vt_mussel_count_NatureServe, ma_mussel_natureserve_poly_count,
                              ma_mussel_natureserve_line_count, ma_mussel_natureserve_point_count)
all_mussel_count$scientific_name <- str_trim(all_mussel_count$scientific_name)
all_mussel_count$common_name[all_mussel_count$common_name == "brook floater (swollen wedgemussel)"] <- "brook floater"
#there is a mismatch between common and sci name alasmidonta undulata is called eastern floater once in CT and MA
all_mussel_count$scientific_name[all_mussel_count$common_name == "eastern floater" &
                                   all_mussel_count$scientific_name == "alasmidonta undulata"] <- "pyganodon cataracta"
all_mussel_count$scientific_name[all_mussel_count$common_name == "alewife floater"] <- "anodonta implicata"
all_mussel_count$scientific_name[all_mussel_count$common_name == "eastern pondmussel"] <- "ligumia nasuta"




#bind length data
all_mussel_length <- bind_rows(ma_mussel_length, ri_mussel_length, nh_mussel_length, vt_mussel_pdf_length, ct_mussel_length)

#bind method data

all_mussel_method <- bind_rows(ma_mussel_method, me_mussel_method, ri_mussel_method, 
                               nh_mussel_method, vt_mussel_pdf_method, ct_mussel_method) #this needs more cleaning



save(all_mussel_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/all_mussel_event.RData")
save(all_mussel_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/all_mussel_occurrence.RData")
save(all_mussel_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/all_mussel_count.RData")
save(all_mussel_length, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/all_mussel_length.RData")
save(all_mussel_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/all_mussel_method.RData")


#make a shape file for the occurrence data
shp <- left_join(all_mussel_occurrence, all_mussel_event, by = "UID")

shp <- shp %>% 
  filter(!is.na(latitude))

shp <- st_as_sf(x = shp,                         
                 coords = c("longitude", "latitude"),
                 crs = st_crs(huc8))

st_write(shp, "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/sppdata/all_mussel_occurrence.shp")




#make a shapefile for the event data
shp <- all_mussel_event %>% 
  filter(!is.na(latitude))

shp <- st_as_sf(x = shp,                         
                coords = c("longitude", "latitude"),
                crs = st_crs(huc8))

st_write(shp, "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/sppdata/all_mussel_event.shp")











################################################################################################
  
#this code prepares the NatureServe source files so that they have the EO_Data information so we can edit

#################################################################################################




################ Massachusetts #################################


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_EOReps.gdb")
ma_eo <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_EOReps.gdb", layer = "query_result")


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcLine.gdb")
ma_srcline <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcLine.gdb", layer = "query_result")


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPolys.gdb")
ma_srcpoly <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPolys.gdb", layer = "query_result")


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPts.gdb")
ma_srcpoint <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPts.gdb", layer = "query_result")


#select just the freshwater mussels
unique(ma_eo$Sname)
ma_eo <- ma_eo %>% 
  filter(Sname %in% c("Ligumia nasuta", "Alasmidonta undulata", "Lampsilis cariosa", "Strophitus undulatus", 
                      "Leptodea ochracea", "Alasmidonta heterodon", "Alasmidonta varicosa"))

ma_srcline <- ma_srcline %>% 
  filter(Sname %in% c("Ligumia nasuta", "Alasmidonta undulata", "Lampsilis cariosa", "Strophitus undulatus", 
                      "Leptodea ochracea", "Alasmidonta heterodon", "Alasmidonta varicosa"))

ma_srcpoly <- ma_srcpoly %>% 
  filter(Sname %in% c("Ligumia nasuta", "Alasmidonta undulata", "Lampsilis cariosa", "Strophitus undulatus", 
                      "Leptodea ochracea", "Alasmidonta heterodon", "Alasmidonta varicosa"))

ma_srcpoint <- ma_srcpoint %>% 
  filter(Sname %in% c("Ligumia nasuta", "Alasmidonta undulata", "Lampsilis cariosa", "Strophitus undulatus", 
                      "Leptodea ochracea", "Alasmidonta heterodon", "Alasmidonta varicosa"))

# remove spatial information from MA shapefiles
ma_eo <- data.frame(ma_eo) %>% 
  select(Sname, SComName, EO_ID, EO_DATA)

ma_srcline <- data.frame(ma_srcline) %>% 
  select(Sname, EO_ID, src_descr, src_lastobs)

ma_srcpoly <- data.frame(ma_srcpoly) %>% 
  select(Sname, EO_ID, src_descr, src_lastobs)

ma_srcpoint <- data.frame(ma_srcpoint) %>% 
  select(Sname, EO_ID, src_descr, src_lastobs)

#join the ma_eo df to the three src files so the src files can have the EO_DATA field
ma_srcline <- left_join(ma_srcline, ma_eo, by = c("EO_ID", "Sname")) %>% 
  arrange(EO_ID)
ma_srcpoint <- left_join(ma_srcpoint, ma_eo, by = c("EO_ID", "Sname")) %>% 
  arrange(EO_ID)
ma_srcpoly <- left_join(ma_srcpoly, ma_eo, by = c("EO_ID", "Sname")) %>% 
  arrange(EO_ID)

write.csv(ma_srcline, "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/srcline_eo_join.csv")
write.csv(ma_srcpoint, "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/srcpoint_eo_join.csv")
write.csv(ma_srcpoly, "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/srcpoly_eo_join.csv")

          

#### MA nature serve data was edited in excel in my onedrive, and then saved to the spp_data folder, and then loaded back in below


#load original polygon file so we get the spatial information


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPolys.gdb")
ma_srcpoly <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPolys.gdb", layer = "query_result")

ma_srcpoly <- st_transform(ma_srcpoly, st_crs(huc8))
ma_srcpoly <- ma_srcpoly %>% 
  mutate(longitude = map_dbl(SHAPE, ~st_point_on_surface(.x)[[1]]),
         latitude = map_dbl(SHAPE, ~st_point_on_surface(.x)[[2]]))

#remove 0s that start the src_desc
ma_srcpoly$src_descr <- sub("^0", "", ma_srcpoly$src_descr)
ma_srcpoly$src_descr <- str_replace(ma_srcpoly$src_descr, "/0", "/")
ma_srcpoly$src_descr <- str_replace(ma_srcpoly$src_descr, " 0", " ")
  

#load the editted polygon file
poly <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MA_srcpoly_eo_join_Tidy.csv")
poly$src_descr <- str_replace(poly$src_descr, "/0", "/")
poly$src_descr <- str_replace(poly$src_descr, " 0", " ")
poly$src_descr <- str_replace(poly$src_descr, "^0", "")

polyy <- poly %>% 
  filter(grepl("/", src_lastobs)) %>% 
  mutate(src_lastobs = mdy(src_lastobs),
         src_lastobs = as.character(src_lastobs))

poly <- poly %>% 
  filter(!grepl("/", src_lastobs))

poly <- rbind(poly, polyy)

#need to break this into three parts, with the src_lastobs and the src loctr to do the join in three parts
poly1 <- poly %>% 
  filter(!src_loctr == "")

poly2 <- poly %>% 
  filter(src_loctr == "")


poly3 <- poly1 %>% 
  left_join(ma_srcpoly, by = c("Sname", "EO_ID", "src_descr", "src_lastobs")) %>% 
  mutate(scientific_name = Sname,
         common_name = SComName,
         live_occurrence = individual.present.absent,
         live_count = as.numeric(individual.count),
         shell_count = shell.count,
         shell_occurrence = shell.present.absence,
         UID = paste("MA", as.character(EO_ID),src_descr, src_lastobs, date, sep= "_"),
         state = "MA",
         project = "MA_NatureServeDatabase_Polygon",
         source = "Sarah Maier",
         date = mdy(date))

poly4 <- poly2 %>% 
  left_join(ma_srcpoly, by = c("Sname", "EO_ID", "src_descr", "src_lastobs")) %>% 
  mutate(scientific_name = Sname,
         common_name = SComName,
         live_occurrence = individual.present.absent,
         live_count = as.numeric(individual.count),
         shell_count = shell.count,
         shell_occurrence = shell.present.absence,
         UID = paste("MA", as.character(EO_ID),src_descr, src_lastobs, date, sep= "_"),
         state = "MA",
         project = "MA_NatureServeDatabase_Polygon",
         source = "Sarah Maier",
         date = mdy(date))

poly5 <- rbind(poly3, poly4)



ma_mussel_natureserve_poly_event <- poly5 %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  filter(!is.na(latitude)) %>% 
  unique()

ma_mussel_natureserve_poly_occurrence <- poly5 %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence) %>% 
  filter(!is.na(live_occurrence) | !is.na(shell_occurrence)) %>% 
  mutate(common_name = tolower(common_name),
         scientific_name = tolower(scientific_name))%>% 
  unique()

ma_mussel_natureserve_poly_count <- poly5 %>% 
  select(UID, common_name, scientific_name, live_count, shell_count) %>% 
  filter(!is.na(live_count) | !is.na(shell_count)) %>% 
  mutate(common_name = tolower(common_name),
         scientific_name = tolower(scientific_name))%>% 
  unique()

save(ma_mussel_natureserve_poly_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_poly_event.RData")
save(ma_mussel_natureserve_poly_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_poly_occurrence.RData")
save(ma_mussel_natureserve_poly_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_poly_count.RData")




#load original line file so we get the spatial information


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcLine.gdb")
ma_srcline <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcLine.gdb", layer = "query_result")

ma_srcline <- st_transform(ma_srcline, st_crs(huc8))
ma_srcline <- ma_srcline %>% 
  mutate(longitude = map_dbl(SHAPE, ~st_point_on_surface(.x)[[1]]),
         latitude = map_dbl(SHAPE, ~st_point_on_surface(.x)[[2]]))

#remove 0s that start the src_desc
ma_srcline$src_descr <- sub("^0", "", ma_srcline$src_descr)
ma_srcline$src_descr <- str_replace(ma_srcline$src_descr, "/0", "/")
ma_srcline$src_descr <- str_replace(ma_srcline$src_descr, " 0", " ")
ma_srcline$src_lastobs[ma_srcline$src_lastobs == "2018-10-02"] <- "10/2/2018"
ma_srcline$src_lastobs[ma_srcline$src_lastobs == "2016-06-09"] <- "6/9/2016"
ma_srcline$src_lastobs[ma_srcline$src_lastobs == "2016-06-16"] <- "6/16/2016"
ma_srcline$src_lastobs[ma_srcline$src_lastobs == "2016-05-31"] <- "5/31/2016"
ma_srcline$src_descr[ma_srcline$src_descr == "2005-09-24"] <- "9/24/2005"


#load the editted line file
poly <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MA_srcline_eo_join_Tidy.csv") 

poly$src_descr <- str_replace(poly$src_descr, "/0", "/")
poly$src_descr <- str_replace(poly$src_descr, " 0", " ")
poly$src_descr <- str_replace(poly$src_descr, "^0", "")

#need to break this into three parts, with the src_lastobs and the src loctr to do the join in three parts


poly1 <- poly %>% 
  left_join(ma_srcline, by = c("Sname", "EO_ID", "src_descr", "src_lastobs")) %>% 
  mutate(scientific_name = Sname,
         common_name = SComName,
         live_occurrence = individual.present.absent,
         live_count = as.numeric(individual.count..not.including.shells.),
         shell_count = shell.count,
         shell_occurrence = shell.present.absence,
         UID = paste("MA", as.character(EO_ID),src_descr, src_lastobs, date, "line", sep= "_"),
         state = "MA",
         project = "MA_NatureServeDatabase_Line",
         source = "Sarah Maier",
         date = mdy(date))


ma_mussel_natureserve_line_event <- poly1 %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  filter(!is.na(latitude)) %>% 
  unique()

ma_mussel_natureserve_line_occurrence <- poly1 %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence) %>% 
  filter(!is.na(live_occurrence) | !is.na(shell_occurrence)) %>% 
  mutate(common_name = tolower(common_name),
         scientific_name = tolower(scientific_name))%>% 
  unique()

ma_mussel_natureserve_line_count <- poly1 %>% 
  select(UID, common_name, scientific_name, live_count, shell_count) %>% 
  filter(!is.na(live_count) | !is.na(shell_count)) %>% 
  mutate(common_name = tolower(common_name),
         scientific_name = tolower(scientific_name))%>% 
  unique()

save(ma_mussel_natureserve_line_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_line_event.RData")
save(ma_mussel_natureserve_line_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_line_occurrence.RData")
save(ma_mussel_natureserve_line_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_line_count.RData")








#MA Point file - load original line file so we get the spatial information


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPts.gdb")
ma_srcpoint <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPts.gdb", layer = "query_result")

ma_srcpoint <- st_transform(ma_srcpoint, st_crs(huc8))
ma_srcpoint <- ma_srcpoint %>% 
  mutate(longitude = map_dbl(SHAPE, ~st_point_on_surface(.x)[[1]]),
         latitude = map_dbl(SHAPE, ~st_point_on_surface(.x)[[2]])) %>% 
  unique()

#remove 0s that start the src_desc
ma_srcpoint$src_descr <- sub("^0", "", ma_srcpoint$src_descr)
ma_srcpoint$src_descr <- str_replace(ma_srcpoint$src_descr, "/0", "/")
ma_srcpoint$src_descr <- str_replace(ma_srcpoint$src_descr, " 0", " ")



#load the editted line file
poly <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MA_srcpoint_eo_join_Tidy.csv") %>% 
  unique()

poly$src_descr <- str_replace(poly$src_descr, "/0", "/")
poly$src_descr <- str_replace(poly$src_descr, " 0", " ")
poly$src_descr <- str_replace(poly$src_descr, "^0", "")
poly$Sname[poly$Sname == "Ligumia suta"] <- "Ligumia nasuta"
poly$src_descr[poly$src_descr == ""] <- NA
poly$Sname[poly$Sname == "3"] <- "leptodea ochracea"

#need to break this into three parts, with the src_lastobs and the src loctr to do the join in three parts


poly1 <- poly %>% 
  left_join(ma_srcpoint, by = c("Sname", "EO_ID", "src_descr", "src_lastobs")) %>% 
  mutate(scientific_name = Sname,
         common_name = SComme,
         live_occurrence = individual.present.absent,
         live_count = as.numeric(individual.count..not.including.shells.),
         shell_count = shell.count,
         shell_occurrence = shell.present.absence,
         UID = paste("MA", as.character(EO_ID),src_descr, src_lastobs, date, "point", sep= "_"),
         state = "MA",
         project = "MA_NatureServeDatabase_Point",
         source = "Sarah Maier",
         date = mdy(date))


ma_mussel_natureserve_point_event <- poly1 %>% 
  select(UID, state, date, latitude, longitude, project, source) %>% 
  filter(!is.na(latitude)) %>% 
  unique()

ma_mussel_natureserve_point_occurrence <- poly1 %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence) %>% 
  filter(!is.na(live_occurrence) | !is.na(shell_occurrence)) %>% 
  mutate(common_name = tolower(common_name),
         scientific_name = tolower(scientific_name))%>% 
  unique()

ma_mussel_natureserve_point_count <- poly1 %>% 
  select(UID, common_name, scientific_name, live_count, shell_count) %>% 
  filter(!is.na(live_count) | !is.na(shell_count)) %>% 
  mutate(common_name = tolower(common_name),
         scientific_name = tolower(scientific_name))%>% 
  unique()

save(ma_mussel_natureserve_point_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_point_event.RData")
save(ma_mussel_natureserve_point_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_point_occurrence.RData")
save(ma_mussel_natureserve_point_count, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ma_mussel_natureserve_point_count.RData")

