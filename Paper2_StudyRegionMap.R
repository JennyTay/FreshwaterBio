#This script is to make the Figure 1 study region map that shows both the number of counts of fish and mussel surveys



library(sf)
library(tidyverse)
library(gridExtra)
library(cowplot)



#Load datasets

#watershed data
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_event_huc_join.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_event_huc_join.RData")

#mussel data
load(file =  "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_occurrence.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_occurrence.RData")


#load new england state data
states <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/newenglandshape/NEWENGLAND_POLY.shp")

#watershed data
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")
huc10 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU10/WBDHU10_NE.shp")
huc12 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU12/WBDHU12_NE.shp")






############# Mussels #################





#calculate the number of repeat surveys at a particular NHD flowline, and each HUC size

#add the year to each event dataset

mussel_event_huc_join <- mussel_event_huc_join %>% 
  mutate(year = year(date))

# plot the number of survyes that have occurred in each HUC 12, 10, 8

#HUC12
dat <- mussel_event_huc_join %>% 
  data.frame() %>% 
  group_by(huc12_name, huc12_tnmid) %>% 
  summarise(surveynum = length(unique(UID))) %>% 
  mutate(count = ifelse(surveynum %in% c(1), "1",
                        ifelse(surveynum %in% c(2:10), "2-10", 
                               ifelse(surveynum %in% c(11:50), "11-50", ">50"))))

dat$count <- factor(dat$count, levels = c("1", "2-10", "11-50", ">50"))


dat <- left_join(huc12, dat, by = c("name" = "huc12_name"))

a <- ggplot()+
  geom_sf(data = dat[!is.na(dat$surveynum),], 
          aes(fill = count, color = count), lwd = .15)+
  scale_fill_brewer(palette = "PRGn", direction = -1)+
  scale_color_brewer(palette = "PRGn", direction = -1)+
  geom_sf(data = states, fill = NA, lwd = 0.5, color = "black")+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.8, 0.2))+
  ggtitle(label = "A. Mussels, HUC12")+
  labs(fill = "Count", color = "Count")



#HUC10
dat <- mussel_event_huc_join %>% 
  data.frame() %>% 
  group_by(huc10_name) %>% 
  summarise(surveynum = length(unique(UID))) %>% 
  mutate(count = ifelse(surveynum %in% c(1:3), "1-3",
                        ifelse(surveynum %in% c(4:10), "4-10",
                               ifelse(surveynum %in% c(11:50), "11-50", ">50"))))

dat$count <- factor(dat$count, levels = c("1-3", "4-10", "11-50", ">50"))


dat <- left_join(huc10, dat, by = c("Name" = "huc10_name"))

b <- ggplot()+
  geom_sf(data = dat[!is.na(dat$surveynum),], 
          aes(fill = count, color = count), lwd = 0.15)+
  scale_fill_brewer(palette = "PRGn", direction = -1)+
  scale_color_brewer(palette = "PRGn", direction = -1)+
  geom_sf(data = states, fill = NA, lwd = 0.5, color = "black")+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.8, 0.2))+
  ggtitle(label = "A. Mussels, HUC10")+
  labs(fill = "Count", color = "Count")



#HUC8
dat <- mussel_event_huc_join %>% 
  data.frame() %>% 
  group_by(huc8_name) %>% 
  summarise(surveynum = length(unique(UID))) %>% 
  mutate(count = ifelse(surveynum %in% c(1:50), "1-50",
                        ifelse(surveynum %in% c(51:100), "51-100",
                               ifelse(surveynum %in% c(101:150), "101-150", ">150"))))

dat$count <- factor(dat$count, levels = c("1-50", "51-100", "101-150", ">150"))


dat <- left_join(huc8, dat, by = c("name" = "huc8_name"))

c <- ggplot()+
  geom_sf(data = dat[!is.na(dat$surveynum),], 
          aes(fill = count, color = count), lwd = 0.15)+
  scale_fill_brewer(palette = "PRGn", direction = -1)+
  scale_color_brewer(palette = "PRGn", direction = -1)+
  geom_sf(data = states, fill = NA, lwd = 0.5, color = "black")+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.8, 0.2))+
  ggtitle(label = "A. Mussels, HUC8")+
  labs(fill = "Count", color = "Count")








############# Fish #################






#calcuate the number of repeat surveys at a particular NHD flowline, and each HUC size

#add the year to each event dataset

fish_event_huc_join <- fish_event_huc_join %>% 
  mutate(year = year(date))

# plot the number of survyes that have occurred in each HUC 12, 10, 8

#HUC12
dat <- fish_event_huc_join %>% 
  data.frame() %>% 
  group_by(huc12_name, huc12_tnmid) %>% 
  summarise(surveynum = length(unique(UID))) %>% 
  mutate(count = ifelse(surveynum %in% c(1), "1",
                        ifelse(surveynum %in% c(2:10), "2-10", 
                               ifelse(surveynum %in% c(11:50), "11-50", ">50"))))

dat$count <- factor(dat$count, levels = c("1", "2-10", "11-50", ">50"))


dat <- left_join(huc12, dat, by = c("name" = "huc12_name"))

d <- ggplot()+
  geom_sf(data = dat[!is.na(dat$surveynum),], 
          aes(fill = count, color = count), lwd = .15)+
  scale_fill_brewer(palette = "PRGn", direction = -1)+
  scale_color_brewer(palette = "PRGn", direction = -1)+
  geom_sf(data = states, fill = NA, lwd = 0.5, color = "black")+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.8, 0.2))+
  ggtitle(label = "B. Fish, HUC12")+
  labs(fill = "Count", color = "Count")


#HUC10
dat <- fish_event_huc_join %>% 
  data.frame() %>% 
  group_by(huc10_name) %>% 
  summarise(surveynum = length(unique(UID))) %>% 
  mutate(count = ifelse(surveynum %in% c(1:20), "1-20",
                        ifelse(surveynum %in% c(21:100), "21-100",
                               ifelse(surveynum %in% c(101:200), "101-200", ">200"))))

dat$count <- factor(dat$count, levels = c("1-20", "21-100", "101-200", ">200"))


dat <- left_join(huc10, dat, by = c("Name" = "huc10_name"))

e <- ggplot()+
  geom_sf(data = dat[!is.na(dat$surveynum),], 
          aes(fill = count, color = count), lwd = 0.15)+
  scale_fill_brewer(palette = "PRGn", direction = -1)+
  scale_color_brewer(palette = "PRGn", direction = -1)+
  geom_sf(data = states, fill = NA, lwd = 0.5, color = "black")+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.8, 0.2))+
  ggtitle(label = "B. Fish, HUC10")+
  labs(fill = "Count", color = "Count")



#HUC8
dat <- fish_event_huc_join %>% 
  data.frame() %>% 
  group_by(huc8_name) %>% 
  summarise(surveynum = length(unique(UID))) %>% 
  mutate(count = ifelse(surveynum %in% c(1:50), "1-50",
                        ifelse(surveynum %in% c(51:100), "51-100",
                               ifelse(surveynum %in% c(101:200), "101-200", ">200"))))

dat$count <- factor(dat$count, levels = c("1-50", "51-100", "101-200", ">200"))


dat <- left_join(huc8, dat, by = c("name" = "huc8_name"))

f <- ggplot()+
  geom_sf(data = dat[!is.na(dat$surveynum),], 
          aes(fill = count, color = count), lwd = 0.15)+
  scale_fill_brewer(palette = "PRGn", direction = -1)+
  scale_color_brewer(palette = "PRGn", direction = -1)+
  geom_sf(data = states, fill = NA, lwd = 0.5, color = "black")+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.8, 0.2))+
  ggtitle(label = "B. Fish, HUC8")+
  labs(fill = "Count", color = "Count")









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


g <-  ggplot(data = US)+
  geom_sf(color = "black", fill = "grey90")+
  geom_sf(data = states, fill = "red2")+
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        title = element_text(size = 10))




##### make final maps with the location map as an inset


#### HUC12 ####



map_huc12 <- ggdraw(a)+
  draw_plot({g}, x = 0.15, y = .60,  width = 0.35, height = 0.35)


map <- grid.arrange(map_huc12,d, ncol = 2)

ggsave(map, file = "tmpfigures/paper2_new/FinalMSFigs/map_huc12.png", width = 19, height = 12, units = "cm")






#### HUC10 ####


map_huc10 <- ggdraw(b)+
  draw_plot({g}, x = 0.19, y = .65,  width = 0.3, height = 0.3)


map <- grid.arrange(map_huc10,e, ncol = 2)

ggsave(map, file = "tmpfigures/paper2_new/FinalMSFigs/map_huc10.png", width = 19, height = 12, units = "cm")






#### HUC8 ####



map_huc8 <- ggdraw(c)+
  draw_plot({g}, x = 0.19, y = .65,  width = 0.3, height = 0.3)


map <- grid.arrange(map_huc8,f, ncol = 2)

ggsave(map, file = "tmpfigures/paper2_new/FinalMSFigs/map_huc8.png", width = 19, height = 12, units = "cm")



