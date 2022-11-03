#combine the NHDplusV2 regions 1 and 2, and then clip by the NE region


library(sf)
library(sp)

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



#USGS gauge data
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/flowmetrics/survey_huc8flowmetrics.RData")

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
NE_crop <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")


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
NE_crop <- NE_crop %>% st_zm() %>% 
  filter(FTYPE  == 'StreamRiver') %>% #remove artificial path, connector, canalditch, pipelines, and coastline
  select(COMID, GNIS_ID, GNIS_NAME, LENGTHKM, REACHCODE)

lines_spatial <- as(NE_crop, "Spatial") #make a spatial data file
lines_midpoints <- SpatialLinesMidPoints(lines_spatial) #get the midpoints

