library(sf)


#this identifier for the stream flow m etrics is the COMID but we have the NHDplusID, so we will need to cross walk


#historical data
st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/S_USA.Hydro_FlowMet_1990s.gdb")
flowmet_historical <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/S_USA.Hydro_FlowMet_1990s.gdb", layer = "Hydro_FlowMet_1990s")


huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp") #get the polygon to crop with

st_crs(huc8) == st_crs(flowmet_historical) #check to see if they are the same projection - they are not

states <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/newenglandshape/NEWENGLAND_POLY.shp")
states <- st_transform(states, st_crs(huc8)) #transform the states to match the nhd data files
flowmet_historical_crop <- flowmet_historical[states, ] # crop by state

plot(flowmet_historical_crop['SHAPE'])

rm(flowmet_historical)



#end of century data
st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/S_USA.Hydro_FlowMet_2080s.gdb")
flowmet_endofcen <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/S_USA.Hydro_FlowMet_2080s.gdb", layer = "Hydro_FlowMet_2080s")
flowmet_endofcen_crop <- flowmet_endofcen[states, ] #crop by state
rm(flowmet_endofcen)


#mid century data
st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/S_USA.Hydro_FlowMet_2040s.gdb")
flowmet_midcen <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/S_USA.Hydro_FlowMet_2040s.gdb", layer = "Hydro_FlowMet_2040s")
flowmet_midcen_crop <- flowmet_midcen[states, ] # crop by state
rm(flowmet_midcen)


#save cropped data frames
save(flowmet_historical_crop, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_historical_crop.RData")
save(flowmet_endofcen_crop, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_endofcen_crop.RData")
save(flowmet_midcen_crop, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_midcen_crop.RData")

