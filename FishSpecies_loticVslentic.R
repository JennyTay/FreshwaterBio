

#install libraries
library(tidyverse)


#annotate lotic vs lentic



#load data
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_event.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_count.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_method.RData")

#ratio of lotic to lentic sampling events
rat <- nrow(event[event$waterbody == "lotic",])/nrow(event[event$waterbody == "lentic",])

tmp <- fish_count %>% 
  left_join(event, by = "UID") %>% 
  select(common_name, waterbody, count) %>% 
  group_by(common_name, waterbody) %>% 
  summarise(count = sum(count)) %>%   #spp count in lotic vs lentic.
  pivot_wider(names_from = waterbody, values_from = count) %>% 
  mutate(ratioLoticToLentic = round(lotic/lentic,2), #ratio of the counts in lotic to lentic
         final = round(ratioLoticToLentic/rat,2))  #divide the lotic to lentic fish count ratio by the ratio of lotic to lentic survey 

tmp <- tmp %>% 
  mutate(tmp = ifelse(final >= 2, "lotic",
                          ifelse(final > .5 & final < 2, "generalist", "lentic")))

tmp <- tmp %>% 
  mutate(habitat_found = ifelse(is.na(lentic) & !is.na(lotic), "lotic", tmp)) %>% 
  select(-tmp) %>% 
  mutate(lifehistory = ifelse(common_name %in% c("american eel", "sea lamprey", "american shad", "rainbow smelt", 
                                                  "blueback herring", "alewife"), 
                               "diadromous", 
                               "freshwater resident"),
         occurrence = ifelse(lotic < quantile(tmp$lotic, 0.2), "rare", 
                             ifelse(lotic < quantile(tmp$lotic, 0.5) & lotic >= quantile(tmp$lotic, 0.2), "common","very common")))



#calculate the fish found in lotic habitats/total lotic surveys and the fish found in lentic habitats/lentic surveys

# lotic <- nrow(event[event$waterbody == "lotic",])
# lentic <- nrow(event[event$waterbody == "lentic",])
# 
# tmp <- fish_count %>% 
#   left_join(event, by = "UID") %>% 
#   select(common_name, waterbody, count) %>% 
#   group_by(common_name, waterbody) %>% 
#   summarise(count = sum(count)) %>% 
#   mutate(fishpersurvey = round(ifelse(waterbody == "lotic", count/lotic, count/lentic),3)) %>% 
#   select(common_name, waterbody, fishpersurvey) %>% 
#   pivot_wider(names_from = waterbody, values_from = fishpersurvey)
