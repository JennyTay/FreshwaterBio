#Format the flow and temperature data


#calcuate the average, max and min of each of the temperature metrics by huc 8, 10, 12


library(tidyverse)
library(sf)
library(sp)
library(maptools)

# Spatial files

# read in zones 1, 2, 4, rbind to create one file

lines01 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds/spatial_01/truncatedFlowlines01.shp")
lines02 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds/spatial_02/truncatedFlowlines02.shp")
lines04 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds/spatial_04/truncatedFlowlines04.shp")

lines <- rbind(lines01, lines02)
lines <- rbind(lines, lines04)





#convert the lines to midpoints

lines_spatial <- as(lines, "Spatial") #make a spatial data file
lines_midpoints <- SpatialLinesMidPoints(lines_spatial) #get the midpoints





#crop by huc8

lines_midpoints <- as(lines_midpoints, "sf") #convert back to an sf object
huc8 <- st_read("C:/Users/jrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp") #get the polygon to crop with

st_crs(huc8) == st_crs(lines_midpoints) #check to see if they are the same projection - they are not
lines_midpoints <- st_transform(lines_midpoints, st_crs(huc8)) #transform the lines to match the nhd data files
st_crs(huc8) == st_crs(lines_midpoints) #confirm they are correctly projected.. they now are

lines_midpoints_crop <- st_crop(lines_midpoints, huc8) #crop the points from the 3 different zones (01,02, 04) to the new england states
#this is cropping the points to the bounding box of the huc8 - we want to crop the points to the actual shape



states <- st_read("C:/Users/jrogers/Documents/necascFreshwaterBio/SpatialData/newenglandshape/NEWENGLAND_POLY.shp")
states <- st_transform(states, st_crs(huc8)) #transform the lines to match the nhd data files
lines_midpoints_subset <- lines_midpoints[states, ] #this worked to crop



ggplot()+
  geom_sf(data = huc8, aes(fill = NULL))+
  geom_sf(data= lines_midpoints_subset, pwd = 21)


head(lines_midpoints_subset)
rm(lines_spatial, lines_midpoints, lines, lines01, lines02, lines04)





#lines_midpoints_subset - this is a point file for every stream that has modeled temperature data in our study region

#Now, we want to join the 'lines_midpoints_subset' to the huc8, 10, and 12 data, so that each point is associated with a specific huc

# we will use the same method we used to join the hucs to the fish survey event to instead join the temperature stream point 


huc8 <- st_read("C:/Users/jrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")
huc10 <- st_read("C:/Users/jrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU10/WBDHU10_NE.shp")
huc12 <- st_read("C:/Users/jrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU12/WBDHU12_NE.shp")


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





#spatial join the huc12, huc8, and huc10 to the temperature lines points
sf_use_s2(FALSE)
sheds_temp_huc_join <- st_join(lines_midpoints_subset, huc8, left = FALSE)
sheds_temp_huc_join <- st_join(sheds_temp_huc_join, huc10, left = FALSE)
sheds_temp_huc_join <- st_join(sheds_temp_huc_join, huc12, left = FALSE)


save(sheds_temp_huc_join, file = "C:/Users/jrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_huc_join.RData")





#read in annual temperature metrics, just keep the feature IDs that are in our region, and just keep the historical data for now.

load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_huc_join.RData")
names(sheds_temp_huc_join) <- tolower(names(sheds_temp_huc_join))

keep <- unique(sheds_temp_huc_join$featureid)

temp01 <- readRDS("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds/model-predict-year-01.rds") %>% 
  filter(featureid %in% keep,
         adjust_air_temp == 0)

temp02 <- readRDS("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds/model-predict-year-02.rds") %>% 
  filter(featureid %in% keep,
         adjust_air_temp == 0) 

temp04 <- readRDS("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds/model-predict-year-04.rds") %>% 
  filter(featureid %in% keep,
         adjust_air_temp == 0) 

temp <- rbind(temp01, temp02, temp04)


sheds_temp_metrics_huc_join <- left_join(sheds_temp_huc_join, temp, by = "featureid") %>% 
  data.frame() %>% 
  filter(!is.na(year)) %>% 
  select(-geometry)

save(sheds_temp_metrics_huc_join, file = "C:/Users/jrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc_join.RData")


rm(temp01, temp02, temp04)


# make three new dataframes, grouped by the huc 12names
# summarise across the huc by the year
#    need to select the metrics we summarise more carefully, but for now I just did a couple

load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc_join.RData")

metrics_annual <- sheds_temp_metrics_huc_join %>% 
  group_by(huc12_name, year) %>% 
  summarise(max_temp = mean(max_temp),
            mean_jul_temp = mean(mean_jul_temp),
            mean_summer_temp = mean(mean_summer_temp),
            mean_max_temp_30d = mean(max_temp_30d),
            mean_n_day_gt_22 = mean(n_day_temp_gt_22))
save(metrics_annual, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_annaul.RData")


# make three new dataframes, grouped by the huc 12,10, 8 names
# summarise across the huc by the pre and post timeperiod
#    need to select the metrics we summarise more carefully, but for now I just did a couple

load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc_join.RData")

sheds_temp_metrics_huc_join <- sheds_temp_metrics_huc_join %>% 
  mutate(timeperiod = ifelse(year <= 1999, "pre", "post"))

metrics12 <- sheds_temp_metrics_huc_join %>% 
  group_by(huc12_name, timeperiod) %>% 
  summarise(max_temp = mean(max_temp),
            mean_jul_temp = mean(mean_jul_temp),
            mean_summer_temp = mean(mean_summer_temp),
            mean_max_temp_30d = mean(max_temp_30d),
            mean_n_day_gt_22 = mean(n_day_temp_gt_22))
save(metrics12, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc12.RData")



metrics10 <- sheds_temp_metrics_huc_join %>% 
  group_by(huc10_name, timeperiod) %>% 
  summarise(max_temp = mean(max_temp),
            mean_jul_temp = mean(mean_jul_temp),
            mean_summer_temp = mean(mean_summer_temp),
            mean_max_temp_30d = mean(max_temp_30d),
            mean_n_day_gt_22 = mean(n_day_temp_gt_22))
save(metrics10, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc10.RData")



metrics08 <- sheds_temp_metrics_huc_join %>% 
  group_by(huc8_name, timeperiod) %>% 
  summarise(max_temp = mean(max_temp),
            mean_jul_temp = mean(mean_jul_temp),
            mean_summer_temp = mean(mean_summer_temp),
            mean_max_temp_30d = mean(max_temp_30d),
            mean_n_day_gt_22 = mean(n_day_temp_gt_22))
save(metrics08, file = "C:/Users/jrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc08.RData")



#plot temperatures

load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc12.RData")

dat <- left_join(huc12, metrics12, by = c("name" = "huc12_name"))

ggplot()+
  geom_sf(data = dat[dat$timeperiod == "pre",], 
          aes(fill = mean_summer_temp), color = NA)+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ggtitle(label = "mean summer temperature")+
  labs(fill = "Temperature (C)")


