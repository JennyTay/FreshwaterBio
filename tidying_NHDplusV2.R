#combine the NHDplusV2 regions 1 and 2, and then clip by the NE region


library(sf)
library(sp)
library(maptools)
library(rgeos)

ne <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDPlusNE/NHDPlus01/NHDSnapshot/Hydrography/NHDFlowline.shp")
ma <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDPlusV21_MA_02_NHDSnapshot_04/NHDPlusMA/NHDPlus02/NHDSnapshot/Hydrography/NHDFlowline.shp")

head(ne)
head(ma)


NE <- rbind(ne, ma)


huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp") #get the polygon to crop with

st_crs(huc8) == st_crs(NE) #check to see if they are the same projection - they are not

states <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/newenglandshape/NEWENGLAND_POLY.shp")
states <- st_transform(states, st_crs(huc8)) #transform the states to match the nhd data files
NE_crop <- NE[states, ] # crop by state
NE_crop <- NE_crop %>% 
  st_zm()

rm(ne, ma)

st_write(NE_crop, dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")






##################################








# assigns every COMID a covariate in the model and a HUC8,10,12 name 
#this is the file we will use to predict the fish and mussels current and future biodiversity



#SHEDs temp data
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc12.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_annaul.RData")

#USFS flow metrics
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_historical_crop.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_endofcen_crop.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_midcen_crop.RData")


#StreamCat
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_census.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_landcov.RData") 
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_imp.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcat_byCOMID.RData")

#watersheds
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")
huc10 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU10/WBDHU10_NE.shp")
huc12 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU12/WBDHU12_NE.shp")
NHDplusV2_NewEngCrop <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")


#prepare the huc files - select the attributes we want and remove the duplicated rows
#21 survey points are lost from falling outside the watershed boundaries that were clipped to the state outlines.


huc8 <- huc8 %>% 
  select(tnmid, areasqkm, name) %>% 
  rename(huc8_areasqkm = areasqkm, huc8_name= name, huc8_tnmid = tnmid)
huc8 <- huc8[!duplicated(huc8$huc8_tnmid),]#Remove the duplicated TNMID fields



huc10 <- huc10 %>% 
  select(TNMID, AreaSqKm , Name)%>% 
  rename(huc10_areasqkm = AreaSqKm, huc10_name= Name, huc10_tnmid = TNMID)
huc10 <- huc10[!duplicated(huc10$huc10_tnmid),]#Remove the duplicated TNMID fields



huc12 <- huc12 %>% 
  select(tnmid, areasqkm , name) %>% 
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







#convert the NHD to points so we can join to the HUC
#convert the lines to midpoints
NHDplusV2_NewEngCrop <- NHDplusV2_NewEngCrop %>% st_zm() %>% 
  filter(FTYPE  == 'StreamRiver') %>% #remove artificial path, connector, canalditch, pipelines, and coastline
  select(COMID)
#read in this dataset because its a projection that works with the spatiallinesmidpoints function
lines04 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds/spatial_04/truncatedFlowlines04.shp")

NHDplusV2_NewEngCrop <- st_transform(NHDplusV2_NewEngCrop, st_crs(lines04))
rm(lines04)

lines_spatial <- as(NHDplusV2_NewEngCrop, "Spatial") #make a spatial data file
lines_midpoints <- SpatialLinesMidPoints(lines_spatial) #get the midpoints
lines_midpoints <- as(lines_midpoints, "sf") #convert back to an sf object

st_crs(huc8) == st_crs(lines_midpoints) #check to see if they are the same projection - they are not
lines_midpoints <- st_transform(lines_midpoints, st_crs(huc8)) #transform the lines to match the nhd data files
st_crs(huc8) == st_crs(lines_midpoints) #confirm they are correctly projected.. they now are
names(lines_midpoints)[1] <- "COMID"

lines_midpoints <- lines_midpoints%>% 
  mutate(long = unlist(map(lines_midpoints$geometry,1)),
         lat = unlist(map(lines_midpoints$geometry,2)))

#get the lat and long from the geography column

sf_use_s2(FALSE)
NHDv2_huc_join <- st_join(lines_midpoints, huc8, left = FALSE)
NHDv2_huc_join <- st_join(NHDv2_huc_join, huc10, left = FALSE)
NHDv2_huc_join <- st_join(NHDv2_huc_join, huc12, left = FALSE)





#join to the covariates by COMID

NHDv2_huc_join <- left_join(NHDv2_huc_join, strmcat_byCOMID, by = "COMID")
NHDv2_huc_join <- left_join(NHDv2_huc_join, strmcatByyr_census, by = "COMID")
NHDv2_huc_join <- left_join(NHDv2_huc_join, strmcatByyr_imp, by = "COMID")
NHDv2_huc_join <- left_join(NHDv2_huc_join, strmcatByyr_landcov, by = "COMID")


flowmet_historical_crop <- as.data.frame(flowmet_historical_crop)
flowmet_historical_crop$COMID <- as.integer(flowmet_historical_crop$COMID)
NHDv2_huc_join <- left_join(NHDv2_huc_join, flowmet_historical_crop, by = "COMID")

#join to the sheds data by HUC12 name.  the model is built using the modeled temperature at the year of the survey, but in the prediction data
#set, we will jsut use the average baseline temperature

metrics12 <- metrics12 %>% 
  group_by(huc12_name) %>% 
  summarise(max_temp = mean(max_temp),
            mean_jul_temp = mean(mean_jul_temp),
            mean_summer_temp = mean(mean_summer_temp),
            mean_max_temp_30d = mean(mean_max_temp_30d),
            mean_n_day_gt_22 = mean(mean_n_day_gt_22))

NHDv2_huc_join <- left_join(NHDv2_huc_join, metrics12, by = "huc12_name")





#edit the variables that we editted in the covariate dataset
NHDv2_huc_join <- NHDv2_huc_join %>% 
  mutate(pctforest_Ws = PctDecid_Ws + PctConif_Ws + PctMxFst_Ws, 
         logWsAreaSqKm = log(WsAreaSqKm),
         logMJJA_HIST = log(MJJA_HIST)) %>% 
  select(-WsAreaSqKm, -MJJA_HIST, -PctDecid_Ws, -PctConif_Ws, -PctMxFst_Ws ) %>% 
  rename(annual_mean_max_temp_30d = mean_max_temp_30d) #put annual in front so that it matches the covariates


#select just the variables we keep in the covariate dataset
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")


NHDv2_huc_join <- NHDv2_huc_join %>% 
  select(COMID, names(dat3)[2:3], names(dat3)[14:34]) %>% 
  data.frame() %>% 
  select(-geometry)



#join to the line file because its currently in the mid point form
NHDplusV2_NewEngCrop <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")
NHDplusV2_NewEngCrop <- NHDplusV2_NewEngCrop %>% st_zm() %>% 
  filter(FTYPE  == 'StreamRiver') %>% #remove artificial path, connector, canalditch, pipelines, and coastline
  select(COMID) 

NHDplusV2_NewEngCrop <- left_join(NHDplusV2_NewEngCrop, NHDv2_huc_join, by = "COMID")



save(NHDplusV2_NewEngCrop, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_covariates.RData")





load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_covariates.RData")

ggplot(data = NHDplusV2_NewEngCrop, mapping = aes(color = BFI_HIST))+
  geom_sf()


clu <- NHDplusV2_NewEngCrop %>%
  data.frame() %>% 
  select("annual_mean_max_temp_30d", "BFI_HIST", "CFM_HIST", "W95_HIST", 
         "BFIWs", "ElevWs", "PctImp_WsRp100", "pctforest_Ws")
clu <- clu[complete.cases(clu),]


#clustering isnt working because the dataframe is too big. will need to cut the data frame down randonly, maybe assign random number and drop 20,000
clusters <- hclust(dist(clu))
