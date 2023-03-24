

#install libraries
library(tidyverse)


#annotate lotic vs lentic



#load data
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_event.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_count.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_presence.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/tidydata/all_fish_method.RData")


#add life history information
tmp1 <- fish_presence%>% 
  select(common_name) %>% 
  unique %>%
  mutate(lifehistory = 
           ifelse(common_name %in% c("american eel", "atlantic sturgeon", "sea lamprey", "american shad", "rainbow smelt", 
                                                 "blueback herring", "alewife", "hickory shad", "ninespine stickleback"), 
                              "diadromous", 
                  ifelse(common_name %in% c("mummichog", "hogchoker", "atlantic silverside", "spotfin killifish",
                                            "northern pipefish", "naked goby", "atlantic needlefish", "striped killifish",
                                            "bay anchovy", "fourspine stickleback", "atlantic menhaden", "sheepshead minnow", 
                                            "three-spined stickleback", "rainwater killifish", "inland silverside", 
                                            "atlantic tomcod", "white perch", "northern searobin", "striped searobin", 
                                            "striped bass", "spot", "spotfin mojarra", "weakfish", "crevalle jack", "blackspotted stickleback"), 
                         "estuarine",
                         ifelse(common_name %in% c("northern kingfish", "bluefish", "king mackerel","winter flounder", 
                                                   "summer flounder", "butterfish"),
                                                   "marine", 
                                                   "freshwater resident"))))


#sum the number of time a spp was found  in lotic vs lentic.
tmp <- fish_presence %>% 
  left_join(event, by = "UID") %>% 
  filter(stock == "natural" | is.na(stock)) %>% 
  select(common_name, waterbody) %>% 
  group_by(common_name, waterbody) %>% 
  summarise(count = n()) %>%   
  pivot_wider(names_from = waterbody, values_from = count)
names(tmp)[4] <- "unk_wtrbody"

#total counts of each spp
tmp2 <- fish_count %>% 
  group_by(common_name, scientific_name) %>% 
  summarize(total_count = sum(count))

#sum of the number of surveys where each spp was identified by state
tmp3 <- fish_presence %>% 
  separate(UID, into = c("state", "delete"), sep = 2) 
tmp3$state[tmp3$state == "de"] <- "VT"
tmp3$state[tmp3$state == "fg"] <- "NH"
tmp3 <- tmp3 %>% 
  select(state, common_name) %>% 
  group_by(state, common_name) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "state", values_from = count)



final <- left_join(tmp1, tmp, by = "common_name")
final <- left_join(final, tmp2, by = "common_name")
final <- left_join(final, tmp3, by = "common_name")

final <- final %>% 
  select(common_name, scientific_name, lifehistory, total_count, lotic, lentic, unk_wtrbody, RI, CT, MA, VT, NH, ME) %>% 
  arrange(lifehistory, lotic)

write.csv(final, "tmpfigures/fishannotation.csv")


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
