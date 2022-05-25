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
  select(c(2:3, 12, 13, 21, 22, 29))
#tidy site_visit table to extract the stream length surveyed and the stream width

# left off here!!!!!!!!!!!!!!!!!!!!!!!!!! and with the broat floater data below

names(site_visit)
colSums(is.na(site_visit))








#################################################################

############   MAFloaterDatabaseDATA2019_20210824 ###############






st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MAFloaterDatabaseDATA2019_20210824.accdb")
mussel_data <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MAFloaterDatabaseDATA2019_20210824.accdb", layer = "MusselData")
survey_data <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/MAFloaterDatabaseDATA2019_20210824.accdb", layer = "SurveyData")

colSums(is.na(mussel_data)) #sex and width have almost no data so we will remove these fields


tmp1 <- mussel_data %>%
  filter(!Species == "NO MUSSELS") %>% 
  mutate(shell_count = ifelse(Abundance.Category == "Shell Only", Count, NA),
         individual_count = ifelse(Abundance.Category == "See Count", Count, NA),
         individual_presence = ifelse(Abundance.Category == "Shell Only", 0, 1))
  

#make a dataset with negative data, so add a row for each spp and a 0
tmp2 <- mussel_data %>% 
  filter(Species == "NO MUSSELS")





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

          







