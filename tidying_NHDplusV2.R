#combine the NHDplusV2 regions 1 and 2, and then clip by the NE region


library(sf)

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



