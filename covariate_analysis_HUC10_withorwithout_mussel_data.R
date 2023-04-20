library(tidyverse)
library(sf)

load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_occurrence.RData")

#mussel event and hydrography join
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_event_huc_join.RData")
df <- left_join(mussel_event_huc_join, mussel_occurrence, by = "UID") %>% 
  select(huc10_name, huc10_tnmid, date, common_name, live_occurrence, shell_occurrence)

unique(mussel_event_huc_join$huc10_tnmid)
length(unique(mussel_event_huc_join$huc10_tnmid))

huc10 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU10/WBDHU10_NE.shp")


load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_mussel_covariates_HUC10.RData")

huc10 <- huc10 %>% 
  data.frame()

surveys <- mussel_event_huc_join %>% 
  data.frame() %>% 
  select(UID, huc10_name, huc10_tnmid) %>% 
  unique() 
occurrence <- mussel_occurrence %>% 
  left_join(surveys, by = "UID") %>% 
  select(huc10_name, huc10_tnmid, live_occurrence) %>% 
  filter(!is.na(huc10_name)) %>% 
  unique() %>% 
  group_by(huc10_name, huc10_tnmid) %>% 
  summarise(occurrence = sum(live_occurrence, na.rm = T))
  

dat <- left_join(huc10, NHDplusV2_NewEngCrop_mussel_covariates_HUC10, by = c("Name" = "huc10_name", "TNMID" = "huc10_tnmid"))
dat <- left_join(dat, occurrence, by = c("Name" = "huc10_name", "TNMID" = "huc10_tnmid"))
dat$occurrence[is.na(dat$occurrence)] <- "no survey"
dat$occurrence[dat$occurrence %in% c(0,1)] <- "survey"

dat <- dat %>% 
  select(1,5,11:39)

for(i in 3:length(names(dat))){
  
ggplot(data = dat, mapping = aes( x = occurrence, y = dat[[i]]))+
  geom_boxplot()+
    geom_jitter(width = .1, alpha = 0.3)+
    labs(y = names(dat)[i])
  
  ggsave(paste("tmpfigures/mussel_covariates_HUC10_w-wo_surveys/", names(dat)[i], ".png", sep = ""), 
         plot = last_plot(),
         width = 18,
         height = 12,
         units = "cm",
         dpi = 300) 
}
