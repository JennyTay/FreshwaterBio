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
states <- st_transform(states, st_crs(huc8))  #transform the states to match the nhd data files
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
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc12_pls4.RData")

#USFS flow metrics
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_historical_crop.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_endofcen_crop.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_midcen_crop.RData")


#StreamCat
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_census.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_landcov.RData") 
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_imp.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcat_byCOMID.RData")

#dams
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc12dams.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc10dams.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc8dams.RData")


#watersheds
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")
huc10 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU10/WBDHU10_NE.shp")
huc12 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU12/WBDHU12_NE.shp")
NHDplusV2_NewEngCrop <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")

#Riparian NLDC - this is the file Jason prepared - 100m buffer around flowlines and extracts the pixel count for each landcover class
#I removed X0 and X11 from the total (denominator) becuase open water is mostly just the stream itself
NLCD <- read.csv("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NLCD/landuse_100m_buff_NE.csv") %>% 
  rename(COMID = commid) %>% 
  mutate(pctforest_ripbuf100 = ((X41 + X42 + X43) / 
                                  (X0 + X12 + X21 + X22 + X23 + X24 + X31 + X41 + X42 + X43 + X51 + 
                                     X52 + X71 + X72 + X73 + X74 + X81 + X82 + X90 + X95))*100,
         pctNatural_ripbuf100 = ((X41 + X42 + X43 + X51 + X52 + X71 + X72 + X73 + X74) / 
                                   (X0 + X12 + X21 + X22 + X23 + X24 + X31 + X41 + X42 + 
                                      X43  + X81 + X82 + X90 + X95 + X51 + X52 + X71 + X72 + X73 + X74))*100,
         pctUrban_ripbuf100 = ((X21 + X22 + X23 + X24) / 
                                 (X0 + X12 + X31 + X41 + X42 + X43  + X81 + X82 + X90 + X21 + X22 + X23 + X24 +
                                    X95 + X51 + X52 + X71 + X72 + X73 + X74))*100
  ) %>% 
  select(COMID, pctforest_ripbuf100, pctNatural_ripbuf100, pctUrban_ripbuf100)




#prepare the huc files - select the attributes we want and remove the duplicated rows
#21 survey points are lost from falling outside the watershed boundaries that were clipped to the state outlines.

sf_use_s2(FALSE)

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

#remove year and join by COMID
strmcatByyr_census <- strmcatByyr_census %>% 
  select(-year)
NHDv2_huc_join <- left_join(NHDv2_huc_join, strmcatByyr_census, by = "COMID")

#because this is for the baseline prediction, we will use the year 2019 for both impervious surfaces and all the landcover. 
strmcatByyr_imp <- strmcatByyr_imp %>% 
  filter(year == 2019) %>% 
  select(-year)

NHDv2_huc_join <- left_join(NHDv2_huc_join, strmcatByyr_imp, by = "COMID")

strmcatByyr_landcov <- strmcatByyr_landcov %>% 
  filter(year == 2019) %>% 
  select(-year)
NHDv2_huc_join <- left_join(NHDv2_huc_join, strmcatByyr_landcov, by = "COMID")

NHDv2_huc_join <- left_join(NHDv2_huc_join, NLCD, by = "COMID")



#usfs flow metrics - historical values for the baseline predictions
flowmet_historical_crop <- as.data.frame(flowmet_historical_crop)
flowmet_historical_crop$COMID <- as.integer(flowmet_historical_crop$COMID)
NHDv2_huc_join <- left_join(NHDv2_huc_join, flowmet_historical_crop, by = "COMID")

#usfs flow metrics - midcen values for the future predictions
flowmet_midcen_crop <- as.data.frame(flowmet_midcen_crop)
flowmet_midcen_crop$COMID <- as.integer(flowmet_midcen_crop$COMID)
NHDv2_huc_join <- left_join(NHDv2_huc_join, flowmet_midcen_crop, by = "COMID")

#usfs flow metrics - endofcen values for the future predictions
flowmet_endofcen_crop <- as.data.frame(flowmet_endofcen_crop)
flowmet_endofcen_crop$COMID <- as.integer(flowmet_endofcen_crop$COMID)
NHDv2_huc_join <- left_join(NHDv2_huc_join, flowmet_endofcen_crop, by = "COMID")

#join to the sheds data by HUC12 name.  the model is built using the modeled temperature at the year of the survey, but in the prediction data
#set, we will jsut use the average baseline temperature from the 'post' time period (2000+)
#Probably want to go back to the sheds data and make the join by TNMID because some of the huc12 watersheds have the same name!

metrics12 <- metrics12 %>% 
  filter(timeperiod == "post") %>% 
  select(-timeperiod)

NHDv2_huc_join <- left_join(NHDv2_huc_join, metrics12, by = "huc12_name")


#join to the sheds future +4 data by HUC12 name.  
#Probably want to go back to the sheds data and make the join by TNMID because some of the huc12 watersheds have the same name!
names(metrics12_pls4)[2:6] <- paste(names(metrics12_pls4[2:6]), "pls_4", sep = "_")

NHDv2_huc_join <- left_join(NHDv2_huc_join, metrics12_pls4, by = "huc12_name")



#join to the dam data used the tnmid
NHDv2_huc_join <- left_join(NHDv2_huc_join, huc8dams, by = c("huc8_tnmid", "huc8_name"))
NHDv2_huc_join <- left_join(NHDv2_huc_join, huc12dams, by = c("huc12_tnmid", "huc12_name"))



#combine forest, urban, ag, and wetland

NHDv2_huc_join <- NHDv2_huc_join %>% 
  mutate(pctForest_ws = PctDecid_Ws + PctConif_Ws + PctMxFst_Ws,
         pctForest_Cat = PctDecid_Cat + PctConif_Cat + PctMxFst_Cat,
         pctUrban_ws = PctUrbOp_Ws + PctUrbLo_Ws + PctUrbMd_Ws + PctUrbHi_Ws,
         pctUrban_Cat = PctUrbOp_Cat + PctUrbLo_Cat + PctUrbMd_Cat + PctUrbHi_Cat,
         pctAg_Ws = PctHay_Ws + PctCrop_Ws,
         pctAg_Cat = PctHay_Cat + PctCrop_Cat,
         pctWetland_Ws = PctWdWet_Ws + PctHbWet_Ws,
         pctWetland_Cat = PctWdWet_Cat + PctHbWet_Cat) %>% 
  select(-c(PctDecid_Ws, PctConif_Ws, PctMxFst_Ws,
                   PctDecid_Cat, PctConif_Cat, PctMxFst_Cat,
                   PctUrbOp_Ws, PctUrbLo_Ws, PctUrbMd_Ws, PctUrbHi_Ws,
                   PctUrbOp_Cat, PctUrbLo_Cat, PctUrbMd_Cat, PctUrbHi_Cat,
                   PctHay_Ws, PctCrop_Ws,
                   PctHay_Cat, PctCrop_Cat,
                   PctWdWet_Ws, PctHbWet_Ws,
                   PctWdWet_Cat, PctHbWet_Cat))


NHDv2_huc_join <- NHDv2_huc_join %>% 
  mutate(logWsAreaSqKm = log(WsAreaSqKm+1),
         logMJJA_HIST = log(MJJA_HIST+1),
         logMJJA_2040 = log(MJJA_2040+1),
         logMJJA_2080 = log(MJJA_2080+1),
         logRdCrsCat = log(RdCrsCat+1),
         logPctOw_Cat = log(PctOw_Cat+1)) %>% 
  select(-WsAreaSqKm, -MJJA_HIST, -RdCrsCat, -PctOw_Cat ) %>% 
  rename(annual_mean_summer_temp = mean_summer_temp,   #put annual in front so that it matches the covariates
         annual_mean_summer_temp_pls_4 = mean_summer_temp_pls_4) 


#select just the variables we keep in the covariate dataset for the baseline predictions


load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")

baselinecov <- NHDv2_huc_join

baselinecov <- baselinecov %>% 
  select(COMID, huc8_tnmid, huc10_name, huc10_tnmid, huc12_name, huc12_tnmid, 
         names(fishcovariates)[2:3], names(fishcovariates)[8:41]) %>% 
  data.frame() %>% 
  select(-geometry)



#join to the line file because its currently in the mid point form
NHDplusV2_NewEngCrop <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")
NHDplusV2_NewEngCrop <- NHDplusV2_NewEngCrop %>% st_zm() %>% 
  filter(FTYPE  == 'StreamRiver') %>% #remove artificial path, connector, canalditch, pipelines, and coastline
  select(COMID) 

NHDplusV2_NewEngCrop <- left_join(NHDplusV2_NewEngCrop, baselinecov, by = "COMID")



save(NHDplusV2_NewEngCrop, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_covariates.RData")





#select just the variables we keep in the covariate dataset for the future predictions (lets do 2080)

futcov <- NHDv2_huc_join

#select the covariates for the future, some are the same from the baseline that we are going to manipulate for the mgmt scenarios,
# some of the projections of streamflow and stream temperature
futcov <- futcov %>% 
  select(COMID, huc8_tnmid, huc10_name, huc10_tnmid, huc12_name, huc12_tnmid, 
         names(fishcovariates)[2:3], names(fishcovariates)[8], names(fishcovariates)[14:38],
         names(fishcovariates)[40:41], 
         annual_mean_summer_temp_pls_4, 
         BFI_2080, 
         LO7Q1DT_2080, 
         CFM_2080, 
         W95_2080, 
         logMJJA_2080) %>% 
  data.frame() %>% 
  select(-geometry)



#join to the line file because its currently in the mid point form
NHDplusV2_NewEngCrop_2080 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")
NHDplusV2_NewEngCrop_2080 <- NHDplusV2_NewEngCrop_2080 %>% st_zm() %>% 
  filter(FTYPE  == 'StreamRiver') %>% #remove artificial path, connector, canalditch, pipelines, and coastline
  select(COMID) 

NHDplusV2_NewEngCrop_2080 <- left_join(NHDplusV2_NewEngCrop_2080, futcov, by = "COMID")



save(NHDplusV2_NewEngCrop_2080, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_covariates_2080.RData")





load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_covariates.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_covariates_2080.RData")

ggplot(data = NHDplusV2_NewEngCrop, mapping = aes(color = annual_mean_summer_temp))+
  geom_sf() +
  scale_color_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 17)+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(title = "huc8 dam count",
       color = "m")













#make a datafile that joins all the COMIDS, with the HUCs, and the states, so that we can discuss our results both by HUC and by state.

library(sf)
library(sp)
library(maptools)
library(rgeos)



# assigns every COMID a covariate in the model and a HUC8,10,12 name 
#this is the file we will use to predict the fish and mussels current and future biodiversity


#watersheds and states
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")
huc10 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU10/WBDHU10_NE.shp")
huc12 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU12/WBDHU12_NE.shp")
NHDplusV2_NewEngCrop <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplusV2_EPA/NHDplusV2_NewEngCrop.shp")
states <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/newenglandshape/NEWENGLAND_POLY.shp")


#prepare the huc files - select the attributes we want and remove the duplicated rows
#21 survey points are lost from falling outside the watershed boundaries that were clipped to the state outlines.

sf_use_s2(FALSE)

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

states <- states %>% 
  select(-FIPS, -SHAPE_LEN) %>% 
  rename(state = NAME, state_acres = ACRES, state_area = SHAPE_AREA,)
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

states <- st_transform(states, st_crs(huc8)) #transform the states to match the nhd data files

lines_midpoints <- lines_midpoints%>% 
  mutate(long = unlist(map(lines_midpoints$geometry,1)),
         lat = unlist(map(lines_midpoints$geometry,2)))

#get the lat and long from the geography column

sf_use_s2(FALSE)
NHDv2_huc_state_join <- st_join(lines_midpoints, huc8, left = FALSE)
NHDv2_huc_state_join <- st_join(NHDv2_huc_state_join, huc10, left = FALSE)
NHDv2_huc_state_join <- st_join(NHDv2_huc_state_join, huc12, left = FALSE)
NHDv2_huc_state_join <- st_join(NHDv2_huc_state_join, states, left = FALSE)




save(NHDv2_huc_state_join, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDv2_huc_state_join.RData")



