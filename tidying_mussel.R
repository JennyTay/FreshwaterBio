#linking EO data (which is the data about the time of survey and what was found) to the source file, which 

library(sf)
library(tidyverse)
library(lubridate)
library(readxl)

#first load in an NHD layer, becuase we will use the CRS of the NHD layer to project all the mussel data collected in a different CRS
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")




#################  Vermont natural heritage  ###################




st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb")

rare <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb", layer = "VT_RTE_Animals")
uncom <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb", layer = "VT_Uncommon_animals")


#select just the freshwater mussels
rare <- rare %>% 
  filter(INFORMAL_TAXONOMY == 'Freshwater Mussels')

uncom <- uncom %>% 
  filter(INFORMAL_TAXONOMY == 'Freshwater Mussels')








################  MA natural heritage #####################




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


#table to get the characterics of the mussel
spp_demogr <- spp_demogr %>% 
  data.frame() %>% 
  rename(SNAME = SCIENTIFIC_NAME) %>% 
  select(2:10)

head(spp_visit)
mussel <- spp_visit %>% 
  select(3:15) 

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
  select(latitude, longitude, obs_date, sname, scomname, sitenumber, live, shells, tax_grp, length, height, condition, sex, gravid_brooding, survey_method, number_searchers,
         snorkels, scuba_divers, view_buckets, shoreline_walkers, search_time) %>% 
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
         project= "mussel2002 database")

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
ma_mussel$survey_method[ma_mussel$survey_method == "timed search: uncosntrained"] <- "timed search: unconstrained"
ma_mussel$survey_method[ma_mussel$survey_method == "timed search: unconstrained."] <- "timed search: unconstrained"

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

names(site_visit) <- c("sitenumber", "date", "reach_length_m")


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
  select(UID, latitude, longitude, date, project, source) %>% 
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
  select(c(UID, 15:21, 26))


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

mussel_data <- bind_rows(tmp1, tmp2)

#add in survey data


mussel_data <- left_join(mussel_data, survey_data, by = c("UID" = "Survey_SiteName"))
mussel_data$UID[mussel_data$UID == "Konkapot_2"] <- "MA_Konkapot_2"
mussel_data$UID[mussel_data$UID == "Konkapot_1"] <- "MA_Konkapot_1"
mussel_data$scientific_name <- mussel_data$Species


names(mussel_data)[3:64] <- tolower(names(mussel_data)[3:64])
mussel_data$scientific_name <- tolower(mussel_data$scientific_name)

mussel_data <- left_join(mussel_data, lookup, by = "scientific_name")
mussel_data$common_name <- ifelse(mussel_data$scientific_name == "no mussels", "no mussels", mussel_data$common_name)


bk_mussel_event <- mussel_data %>% 
  mutate(state = "MA",
         project = "brookfloater_study",
         source = "JasonCarmignani-brookfloater") %>% 
  select(UID, x, y, date, project, source) %>% 
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
  select(-c(3:5))


rm(mussel_data, survey_data, tmp1, tmp1a, tmp2)
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

me_mussel_event <- me_mussel %>% 
  select(UID, date, latitude, longitude, project, source) %>% 
  mutate(state = "ME") %>% 
  unique()

me_mussel_occurrence <- me_mussel %>% 
  select(UID, common_name, scientific_name, live_occurrence, shell_occurrence)

me_mussel_method <- me_mussel %>% 
  select(UID, survey_method)

save(me_mussel_event, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/me_mussel_event.RData")
save(me_mussel_occurrence, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/me_mussel_occurrence.RData")
save(me_mussel_method, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/me_mussel_method.RData")










###################################################################




###################################################################

################# Rhode Island Dept of Env Management  ####################




#There are 5 files from Rhode island - comments show file description from Corey Pelletier

#Location data and species presence from Raithel and Hartenstine, 2006 (DIV=diversity, other abbreviations are for scientific names of species)
mus1 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/mussels.shp")

#Flat_R_160728, 160822, 160921- Shapefiles with targeted count data (Eastern Pearlshell) in the Flat River (Exeter, RI) for three different surveys days
mus2 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/FlatR_160921.shp")
mus3 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/FlatR_160822.shp")
mus4 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/Flat River_160728.shp")

#Freshwater Mussel Survey Database.xlsx- Data for mussel surveys conducted in 2020, total of 16 surveys
mus5spatial <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/RI Freshwater Mussel Data/2020 Mussel Survey Locations.shp")
mus5site <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/Freshwater Mussel Survey Database.xlsx",
                   sheet = 1)
mus5data <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/RI DEM Mussel Data/Freshwater Mussel Survey Database.xlsx",
                    sheet = 2)





names(mus1)[14:23] <- c("Alasmidonta undulata", "Alasmidonta varicosa", "Anodonta implicata",
                        "Corbicula fluminea", "Elliptio complanata", "Lampsilis radiata", "Ligumia nasuta",
                        "Margaritifera margaritifera", "Pyganodon cataracta", "Strophitus undulatus") #confirm the COFL is corbicula fluminea

######################################################################




#this code prepares the MA source files so that they have the EO_Data information so we can edit.
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

          







