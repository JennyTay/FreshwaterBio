library(readxl) 
library(tidyverse)





dat<- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/FishTraits/Copy Of Fish_Traits.xlsx")
dat$COMMONNAME <- tolower(dat$COMMONNAME)
dat$COMMONNAME[dat$COMMONNAME == "trout-perch"] <- "trout perch"
dat$COMMONNAME[dat$COMMONNAME == "pearl dace"] <- "allegheny pearl dace"


#read in fish occurrence data just to get the nameing conventions
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_occurrence.RData")
names <- fish_occurrence %>% 
  select(common_name, scientific_name) %>% 
  unique()


# test <- dat %>% 
#   select(COMMONNAME, GENUS, SPECIES) %>% 
#   rename(common_name = COMMONNAME)
# 
# test3 <- left_join(test, names, by = "common_name")
# 
# incorrect <- names %>% 
#   filter(!common_name %in% test3$common_name)
# 

#remove spp identifies that we don't need, and remove all the food items, which likely isnt applicable ot our study
traits <- dat %>% 
  select(FID, COMMONNAME, 29:115) %>% 
  filter(COMMONNAME %in% names$common_name)

spawntime <- traits %>% 
  select(COMMONNAME, 7:18) %>% 
  pivot_longer(cols = 2:13, names_to = "month", values_to = "value") %>% 
  mutate(season = ifelse(month %in% c("MAR", "APR", "MAY"), "spring",
                         ifelse(month %in% c("JUN", "JUL", "AUG"), "summer",
                                ifelse(month %in% c("SEP", "OCT", "NOV"), "fall", "winter"))))
spawntime$month[spawntime$month == "JAN"] <- 1
spawntime$month[spawntime$month == "FEB"] <- 2
spawntime$month[spawntime$month == "MAR"] <- 3
spawntime$month[spawntime$month == "APR"] <- 4
spawntime$month[spawntime$month == "MAY"] <- 5
spawntime$month[spawntime$month == "JUN"] <- 6
spawntime$month[spawntime$month == "JUL"] <- 7
spawntime$month[spawntime$month == "AUG"] <- 8
spawntime$month[spawntime$month == "SEP"] <- 9
spawntime$month[spawntime$month == "OCT"] <- 10
spawntime$month[spawntime$month == "NOV"] <- 11
spawntime$month[spawntime$month == "DEC"] <- 12

#make plot of spawning
ggplot(data = spawntime, mapping = aes(x = as.integer(month), y = value))+
  geom_point()+
  geom_line()+
  facet_wrap(~COMMONNAME)+
  scale_x_continuous(breaks = seq(1,12, by = 1))
ggsave(
  filename = "C:/Users/jenrogers/Documents/git/FreshwaterBio/FreshwaterBio/tmpfigures/spawntime.png",
  plot = last_plot(),
  width = 21,
  height = 17,
  units = "cm",
  dpi = 200
)

test <- spawntime %>% 
  group_by(COMMONNAME, season) %>% 
  summarise(tot = sum(value)) %>% 
  filter(tot != 0) %>% 
  group_by(COMMONNAME) %>% 
  pivot_wider(names_from = season, values_from = tot) %>% 
  mutate(timing = ifelse(spring > 0 & is.na(summer) & is.na(winter) & is.na(fall), "sp", 
                         ifelse(fall > 0 & is.na(summer) & is.na(winter) & is.na(spring),"f", 
                                ifelse(spring > 0 & winter > 0 & is.na(fall) & is.na(summer), "sp/w",
                                ifelse(fall > 0 & winter > 0 & is.na(spring) & is.na(summer), "f/w",
                                ifelse(spring > 0 & summer > 0 & is.na(winter) & is.na(fall), "sp/su",
                                  ifelse(spring > 0 & summer > 0 & fall> 0  & is.na(winter), "sp/su/f",
                                       ifelse(spring > 0 & summer > 0 & fall> 0  & winter > 0, "sp/su/f/w",
                                              "su"))))))))
                                       
#for some reason the ifelse statement is not completing with the 'else' so ill add those manuall
test$timing[test$COMMONNAME == "blacknose shiner"] <- "su"
test$timing[test$COMMONNAME == "longnose sucker"] <- "su"

    


spawntime <- traits %>% 
  select(COMMONNAME, 7:19) %>% 
  mutate(strategy = ifelse(SEASON > 2, "extended", "narrow")) %>% 
  left_join(test, by = "COMMONNAME") %>% 
  select(1:15, 20) 

write.csv(spawntime, "C:/Users/jenrogers/Documents/git/FreshwaterBio/FreshwaterBio/tmpfigures/spawntime.csv")


  

spawnsub <- traits %>% 
  select(COMMONNAME, 21:45)

sub <- traits %>% 
  select(COMMONNAME, 51:60)
  
vel <- traits %>% 
  select(COMMONNAME, 74:76)

habtype <- traits %>% 
  select(COMMONNAME, 62:73)


#left join with the traits assigned in New England
temp <- read_xlsx("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/ThermalPreferences.xlsx")
temp <- temp %>% 
  select(common_name, `Temperatures Notes`) %>% 
  rename(tmp = `Temperatures Notes`)

table(temp$tmp)
#use the traits database to fill in the ones we dont have
