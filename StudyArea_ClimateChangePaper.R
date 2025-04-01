#add a locational map of new england in the uS for figure 2 in paper 1
#watershed shape files
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")
#load new england state data
states <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/newenglandshape/NEWENGLAND_POLY.shp")
#load USA states
US <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USstates/cb_2018_us_state_5m.shp")
US <- US %>% 
  filter(STUSPS %in% c("ME", "NH", "VT", "MA", "CT", "RI", "NY", "PA", "NJ", "MD", "VA", "OH", "WV", "DE"))
st_crs(huc8) == st_crs(US)
states <- st_transform(states, crs = st_crs(huc8))
st_crs(huc8) == st_crs(states)


locationalMap <- ggplot(data = US)+
  geom_sf(color = "black", fill = "grey90")+
  geom_sf(data = states, fill = "red2")+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 7),
        legend.position = "none",
        title = element_text(size = 10))+
  ggtitle("Study Location")

ggsave(locationalMap, file = "tmpfigures/paper2_new/StudyArea.png", width = 18, height = 12, units = "cm")
