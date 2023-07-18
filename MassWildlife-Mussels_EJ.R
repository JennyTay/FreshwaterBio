#plots of mussel distributions overlayed with the MA EJ data
#These plots only include the surveys of lotic sites that are <500m from a stream


library(sf)
library(tidyverse)  



#mussel data
load(file =  "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_occurrence.RData")
load(file =  "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_count_with_zeros.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_event_huc_join.RData")

#load new england state data
states <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/newenglandshape/NEWENGLAND_POLY.shp")
states <- states %>% 
  filter(NAME == "MASSACHUSETTS")

#watershed data
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")
huc12 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU12/WBDHU12_NE.shp")
huc12 <- huc12 %>% 
  filter(grepl("MA", states) )

#load mussel data
shp <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/sppdata/all_mussel_occurrence.shp")
#get the UIDs in the mussel_event_huc_join because these have been editted to remove lentic and to remove sites >500m from stream

#MA Environmental justice data
ej <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/EnvJustice/EJ_POLY.shp")



#remove NA names
shp <- shp %>% 
  filter(UID %in% mussel_event_huc_join$UID) %>% #remove the lentic sites
  select(UID) %>% 
  unique()



####Presence absence:



pres_total <- right_join(shp, mussel_occurrence, by = "UID") %>% 
  filter(!is.na(live_occurrence)) %>% 
  filter(grepl("^MA", UID)) %>% #select just MA
  filter(live_occurrence == 1) %>% 
  data.frame() %>% 
  select(UID, common_name) %>% 
  left_join(mussel_event_huc_join, by = "UID") %>% 
  select(common_name, huc12_name, huc12_tnmid) %>% 
  unique() %>% 
  group_by(huc12_name, huc12_tnmid) %>% 
  summarise(richness = n())

pres_total <- left_join(huc12, pres_total, by = c("name" = "huc12_name", "tnmid" = "huc12_tnmid"))

y <- st_point_on_surface(pres_total)


#map 1: EJ populations overlain with the Federally endangered dwarf wedgemussel
ggplot()+
  geom_sf(data = states)+
  geom_sf(data = ej, aes(fill = EJ_CRIT_DE, color = EJ_CRIT_DE))+
  geom_sf(data = y, pch = 21, aes(size = richness))+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text=element_text(size=6),
        legend.title =element_text(size=8))+
  ggtitle(label = "Mussel Richness and EJ Criteria")+
  guides(color = FALSE, size = FALSE)+
  labs(fill = "Environmental Justice Blocks")

ggsave(
  filename = "C:/Users/jenrogers/Documents/UMassPostDoc/Grants/MassWildlife/EJRichness.png",
  plot = last_plot(),
  width = 14,
  height = 9,
  units = "cm",
  dpi = 300)




#how many mussels observations fall within EJ communities

pres <- right_join(shp, mussel_occurrence, by = "UID") %>% 
  filter(!is.na(live_occurrence)) %>% 
  filter(grepl("^MA", UID)) %>% #select just MA
  filter(live_occurrence == 1)

st_crs(huc12) == st_crs(ej) #check to see if they are the same projection - they are not
ej <- st_transform(ej, st_crs(huc12))  #transform the states to match the nhd data files

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



#join the huc files to the mussel_event file using a spatial join
#each mussel survey event will be associated with a huc8, huc10, and huc12 TNMID
sf_use_s2(FALSE)


mussel_event_ej_join <- st_join(pres, ej, right = TRUE)

nrow(mussel_event_ej_join[!is.na(mussel_event_ej_join$EJ_CRIT_DE),])/nrow(mussel_event_ej_join) #0.1094017

df <- mussel_event_ej_join %>% 
  filter(!is.na(EJ_CRIT_DE)) %>%
  filter(common_name %in% "dwarf wedgemussel")
  group_by(EJ_CRIT_DE) %>% 
  summarise(count = n())
  
nrow(pres[pres$common_name == "dwarf wedgemussel",])
