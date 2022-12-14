#tidying the dam layer
#want to combine the state dams into a single dataframe and then calcuate dam density at the HUC8, 10, 12.
library(sf)
library(tidyverse)

ri <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StateDams/RI_dams.csv")
vt <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StateDams/VT_dams.csv")
nh <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StateDams/NH_dams.csv")
ct <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StateDams/CT_dams.csv")

ma <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StateDams/MA/DAMS_PT.shp")
me <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/StateDams/Viewer_Dams_Export_120922.gdb")

ri <- ri %>%
  dplyr::select(X, Y, NAME) %>% 
  rename(latitude = Y, 
         longitude = X, 
         name = NAME) %>% 
  mutate(source = "URI Environmental Data Center and RIGIS")

ma <- ma %>% 
  dplyr::select(DAMLAT, DAMLONG, DAMNAME, HAZCODE) %>% 
  rename(latitude = DAMLAT, 
         longitude = DAMLONG, 
         name = DAMNAME,
         hazard = HAZCODE) %>% 
  data.frame() %>% 
  dplyr::select(-geometry) %>% 
  mutate(source = "MassGIS Data: Dams, MA Office of Dam Safety")

ct <- ct %>% 
  dplyr::select(X, Y, DAM_NAME, DAM_STATUS, DAM_HAZ) %>% 
  rename(latitude = Y, 
         longitude = X, 
         name = DAM_NAME,
         status = DAM_STATUS,
         hazard = DAM_HAZ) %>% 
  mutate(source = "CT DEEP GIS Open Data Website")

vt <- vt %>% 
  dplyr::select(LatNAD83, LongNAD83, DamName, DamStatus, Purposes) %>% 
  rename(latitude = LatNAD83, 
         longitude = LongNAD83, 
         name = DamName,
         status = DamStatus,
         use = Purposes) %>% 
  mutate(source = "VT Dept of Environmental Conservation")

nh <- nh %>% 
  dplyr::select(LATITUDE,LONGITUDE, DAM, HAZCL, HAZCL, USE) %>% 
  rename(latitude = LATITUDE, 
         longitude = LONGITUDE, 
         name = DAM,
         status = HAZCL,
         use = USE,
         hazard = HAZCL) %>% 
  mutate(source = "NH Dam Inventory; Jim Weber, NH Dept of Env Services") %>% 
  filter(latitude>30)

me <- me %>% 
  data.frame() %>% 
  select(Latitude, Longitude, Dam_Name) %>% 
  rename(latitude = Latitude,
         longitude = Longitude,
         name = Dam_Name) %>% 
  mutate(souce = "Alex Abbott")

#bind the datasets into one

dams <- bind_rows(ri, ct, ma, nh, vt, me)

#assign a datum
#read in the huc8 for our datum
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")
huc10 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU10/WBDHU10_NE.shp")
huc12 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU12/WBDHU12_NE.shp")

dams <- st_as_sf(x = dams,
                 coords = c("longitude", "latitude"),
                 crs = st_crs(huc8))

plot(dams['geometry'])

#add the dam density - number of dams acre in each HUC12, 10, 8 


#annote each dam with a huc name
# first need to prepare the huc files - select the attributes we want and remove the duplicated rows
#21 survey points are lost from falling outside the watershed boundaries that were clipped to the state outlines.
#also, reclatuate the areas of each hucc bc there appear to be some errors

sf_use_s2(FALSE)


huc8 <- huc8 %>% 
  dplyr::select(tnmid, areasqkm, name) %>% 
  rename(huc8_areasqkm = areasqkm, huc8_name= name, huc8_tnmid = tnmid)
huc8 <- huc8[!duplicated(huc8$huc8_tnmid),]#Remove the duplicated TNMID fields
huc8$huc8_area_new <- st_area(huc8)


huc10 <- huc10 %>% 
  dplyr::select(TNMID, AreaSqKm , Name)%>% 
  rename(huc10_areasqkm = AreaSqKm, huc10_name= Name, huc10_tnmid = TNMID)
huc10 <- huc10[!duplicated(huc10$huc10_tnmid),]#Remove the duplicated TNMID fields
huc10$huc10_area_new <- st_area(huc10)


huc12 <- huc12 %>% 
  dplyr::select(tnmid, areasqkm , name) %>% 
  rename(huc12_areasqkm = areasqkm, huc12_name= name, huc12_tnmid = tnmid)
huc12 <- huc12[!duplicated(huc12$huc12_tnmid),]#Remove the duplicated TNMID fields
#this df needs aditional editting
#Outlet Missisquoi River - HUc12_name this is the same shape, but it actually has two separate tnmid
#Outlet Sutton River and Riviere Sutton are the same shape, but have different HUC 12 names and have different tnmid
#Ruiss Coslett-Riviere Aux Brochets vs Groat Creek are the same shape with different names and tmnid

huc12 <- huc12 %>% 
  filter(huc12_tnmid != "{7964F492-75EA-4AA7-8E77-A96D99348771}",
         huc12_name != "Riviere Sutton",
         huc12_name != "Ruiss Coslett-Riviere Aux Brochets")

huc12$huc12_area_new <- st_area(huc12)
#join the huc files to the dam file using a spatial join
#each dam record will be associated with a huc8, huc10, and huc12 TNMID

sf_use_s2(FALSE)
dam_huc_join <- st_join(dams, huc8, left = FALSE)
dam_huc_join <- st_join(dam_huc_join, huc10, left = FALSE)
dam_huc_join <- st_join(dam_huc_join, huc12, left = FALSE)




#caluate dams per acre for each HUC

huc12dams <- dam_huc_join %>% 
  data.frame() %>% 
  dplyr::select(-geometry) %>% 
  group_by(huc12_tnmid) %>% 
  mutate(huc12_damcount = n(),
         damdensitysqm = as.numeric(huc12_damcount /huc12_area_new)) %>% 
  ungroup() %>% 
  dplyr::select(huc12_tnmid, huc12_name, huc12_area_new, huc12_damcount, damdensitysqm) %>% 
  unique()%>% 
  mutate(huc12_damden_sqkm = damdensitysqm/.000001) %>% 
  dplyr::select(-damdensitysqm)

huc10dams <- dam_huc_join %>% 
  data.frame() %>% 
  dplyr::select(-geometry) %>% 
  group_by(huc10_tnmid) %>% 
  mutate(huc10_damcount = n(),
         damdensitysqm = as.numeric(huc10_damcount /huc10_area_new)) %>% 
  ungroup() %>% 
  dplyr::select(huc10_tnmid, huc10_name, huc10_area_new, huc10_damcount, damdensitysqm) %>% 
  unique()%>% 
  mutate(huc10_damden_sqkm = damdensitysqm/.000001) %>% 
  dplyr::select(-damdensitysqm)

huc8dams <- dam_huc_join %>% 
  data.frame() %>% 
  dplyr::select(-geometry) %>% 
  group_by(huc8_tnmid) %>% 
  mutate(huc8_damcount = n(),
         damdensitysqm = as.numeric(huc8_damcount /huc8_area_new)) %>% 
  ungroup() %>% 
  dplyr::select(huc8_tnmid, huc8_name, huc8_area_new, huc8_damcount, damdensitysqm) %>% 
  unique() %>% 
  mutate(huc8_damden_sqkm = damdensitysqm/.000001) %>% 
  dplyr::select(-damdensitysqm)


#bind HUC 8, 10, 12 dataframe to get the area of all the remaining hucs and we can assign those toe zero dams. 
#Will need to come back to ME.
tmp1 <- huc8 %>% 
  filter(!huc8_tnmid%in% huc8dams$huc8_tnmid ) %>% 
  dplyr::select(huc8_tnmid, huc8_area_new, huc8_name) %>% 
  mutate(huc8_damcount = 0,
         huc8_damden_sqkm = 0) %>% 
  data.frame() %>% 
  dplyr::select(-geometry)

tmp2 <- huc10 %>% 
  filter(!huc10_tnmid%in% huc10dams$huc10_tnmid ) %>% 
  dplyr::select(huc10_tnmid, huc10_area_new, huc10_name) %>% 
  mutate(huc10_damcount = 0,
         huc10_damden_sqkm = 0)%>% 
  data.frame() %>% 
  dplyr::select(-geometry)


tmp3 <- huc12 %>% 
  filter(!huc12_tnmid%in% huc12dams$huc12_tnmid ) %>% 
  dplyr::select(huc12_tnmid, huc12_area_new, huc12_name) %>% 
  mutate(huc12_damcount = 0,
         huc12_damden_sqkm = 0)%>% 
  data.frame() %>% 
  dplyr::select(-geometry)

#rbind the zero data with the dam data
huc8dams <- rbind(huc8dams, tmp1)
huc10dams <- rbind(huc10dams, tmp2)
huc12dams <- rbind(huc12dams, tmp3)


save(huc12dams, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc12dams.RData")
save(huc10dams, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc10dams.RData")
save(huc8dams, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc8dams.RData")
