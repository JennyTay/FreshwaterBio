#linking EO data (which is the data about the time of survey and what was found) to the source file, which 

library(sf)
library(tidyverse)
library(lubridate)


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
                              ifelse(shells == 0, 0, 1))) %>% 
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
  select(UID, common_name, scientific_name, length, height) %>% 
  filter(!is.na(length))

ma_mussel_method <- ma_mussel %>% 
  mutate(UID = paste("MA", sitenumber, date, sep = "_")) %>% 
  select(c(UID, 15:21, 26))


rm(mussel, shp, site_visit, spp_demogr, spp_desc, spp_visit, ma_mussel, test)







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
  rename("UID" = "Mussel_SiteName")

mussel_data <- bind_rows(tmp1, tmp2)

#add in survey data


mussel_data <- left_join(mussel_data, survey_data, by = c("UID" = "Survey_SiteName"))
mussel_data$UID[mussel_data$UID == "Konkapot_2"] <- "MA_Konkapot_2"
mussel_data$UID[mussel_data$UID == "Konkapot_1"] <- "MA_Konkapot_1"


bk_mussel_event <- mussel_data %>% 
  mutate(state = "MA",
         project = "brookfloater_study",
         source = "JasonCarmignani-brookfloater") %>% 
  select(UID, X, Y, Date, project, source) %>% 
  rename(latitude = Y,
         longitude = X,
         date = Date) %>% 
  unique() %>% 
  filter(!is.na(latitude))




bk_mussel_occurrence <- mussel_data %>% 
  select(UID, Species, live_occurrence, shell_occurrence) %>% 
  unique()




bk_mussel_count <- mussel_data %>% 
  select(UID, Species, individual_countnew, shellcountnew) %>% 
  rename(live_count = individual_countnew,
         shell_count = shellcountnew) %>% 
  unique()


bk_mussel_length <- mussel_data %>% 
  select(UID, Species, Length, Height) %>% 
  filter(!is.na(Length))


bk_mussel_method <- mussel_data %>% 
  select(UID, NoObservers, WetWidth1, WetWidth2, WetWidth3, Measured.Length.of.Stream.Survey, Access.Visibility..1m) %>% 
  filter(UID %in% bk_mussel_event$UID) %>% 
  rename(number_searchers = NoObservers,
         reach_length_m = Measured.Length.of.Stream.Survey,
         access_visibility_1m = Access.Visibility..1m) %>% 
  mutate( wet_width_avg_m = mean(c(WetWidth1, WetWidth2, WetWidth3))) %>% 
  select(-c(3:5))


rm(mussel_data, survey_data, tmp1, tmp1a, tmp2)
###

#bind together mussel database and the brook floater data

head(ma_mussel_event)
head(bk_mussel_event)

ma_mussel_event <- bind_rows(ma_mussel_event, bk_mussel_event)

###################################################################

################# Maine Dept of Inland Waters  ####################


me_mussel <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/ME IFW Mussel Data/Maine Freshwater Mussel Survey Data_20220426.csv")

names(me_mussel)
df$DDLONG <- ifelse(df$DDLONG > 0, -df$DDLONG, df$DDLONG)


me_mussel <- me_mussel %>% 
  select(WTYPE, SITE, MONTH, DAY, YEAR, SVYTYPE, SRCTYPE, SOURCE, 17:28) %>% 
  rename(longitude = DDLONG,
         latitude = DDLAT,
         waterbody = WTYPE,
         method = SVYTYPE,
         project = SOURCE) %>% 
  mutate(source = "Beth Swartz - ME Inland Fisheries and Wildlife")


head(me_mussel)

me_mussel$waterbody[me_mussel$waterbody == "L"] <- "lentic"
me_mussel$waterbody[me_mussel$waterbody == "W"] <- "lotic"
me_mussel$method[me_mussel$method == "S"] <- "survey"
me_mussel$method[me_mussel$method == "I"] <- "incidental submission"

names(me_mussel)[9:18] <- c("Margaritifera margaritifera", "Elliptio complanata",
                             "Alasmidonta undulata", "Alasmidonta varicosa",
                             "Pyganodon cataracta", "Anodonta implicata",
                             "Strophitus undulatus", "Leptodea ochracea",
                             "Lampsilis cariosa", "Lampsilis radiata")

test <- me_mussel %>% 
  pivot_longer(cols = 9:18, names_to = "scientific_name", values_to = "occurrence") %>% 
  mutate(live_occurrence = ifelse(occurrence == "X", 1, 0),
         shell_occurrence = ifelse(occurrence == "S", 1, 0),
         date = dmy(paste(DAY, MONTH, YEAR, by = "-")),
         UID = paste())

names(test) <- tolower(names(test))



write.csv(df, "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata_mussel/ME_mussel.csv")










###################################################################





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

          







