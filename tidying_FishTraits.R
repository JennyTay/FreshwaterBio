library(readxl) 
library(tidyverse)




#read in the fish traits database and fix some of the names
dat<- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/FishTraits/Copy Of Fish_Traits.xlsx")
names(dat)[11] <- "common_name"
dat$common_name <- tolower(dat$common_name)
dat$common_name[dat$common_name == "trout-perch"] <- "trout perch"
dat$common_name[dat$common_name == "pearl dace"] <- "allegheny pearl dace"


#read in fish occurrence data just to get the nameing conventions
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_occurrence.RData")
names <- fish_occurrence %>% 
  select(common_name, scientific_name) %>% 
  unique()


# test <- dat %>%
#   select(common_name, GENUS, SPECIES) %>%
#   rename(common_name = common_name)
# 
# test3 <- left_join(test, names, by = "common_name")
# 
# incorrect <- names %>%
#   filter(!common_name %in% test3$common_name)


#remove spp identifies that we don't need, and remove all the food items, which likely isnt applicable ot our study
traits <- dat %>% 
  select(FID, common_name, 29:115) %>% 
  filter(common_name %in% names$common_name)

spawntime <- traits %>% 
  select(common_name, 7:18) %>% 
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
  facet_wrap(~common_name)+
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
  group_by(common_name, season) %>% 
  summarise(tot = sum(value)) %>% 
  filter(tot != 0) %>% 
  group_by(common_name) %>% 
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
test$timing[test$common_name == "blacknose shiner"] <- "su"
test$timing[test$common_name == "longnose sucker"] <- "su"

    


spawntime <- traits %>% 
  select(common_name, 7:19) %>% 
  mutate(strategy = ifelse(SEASON > 2, "extended", "narrow")) %>% 
  left_join(test, by = "common_name") %>% 
  select(1:15, 20) 

write.csv(spawntime, "C:/Users/jenrogers/Documents/git/FreshwaterBio/FreshwaterBio/tmpfigures/spawntime.csv")


  

spawnsub <- traits %>% 
  select(common_name, 21:45)

sub <- traits %>% 
  select(common_name, 51:60)
  
vel <- traits %>% 
  select(common_name, 74:76)


vel <- vel %>% 
  mutate(velocity = ifelse(SLOWCURR == 1 & MODCURR == 0 & FASTCURR == 0, "slow",
                           ifelse(MODCURR == 1 & SLOWCURR == 0 & FASTCURR == 0, "mod",
                                  ifelse(FASTCURR == 1 & SLOWCURR == 0 & MODCURR == 0, "fast", 
                                         ifelse(SLOWCURR == 1 & MODCURR == 1 & FASTCURR == 0, "slow-mod",
                                                ifelse(SLOWCURR == 0 & MODCURR == 1 & FASTCURR == 1, "fast-mod", "any"))))))
write.csv(vel, "C:/Users/jenrogers/Documents/git/FreshwaterBio/FreshwaterBio/tmpfigures/velocity.csv")


habtype <- traits %>% 
  select(common_name, 62:73)



#left join with the traits assigned in New England
temp <- read_xlsx("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/ThermalPreferences.xlsx")
temp <- temp %>% 
  select(common_name, `Temperatures Notes`) %>% 
  rename(tmp = `Temperatures Notes`)

table(temp$tmp)




#merge traits into a single dataframe
names(spawntime)
names(vel)
names(temp)
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_count_with_zeros.RData")

final_traits <- left_join(temp, spawntime, by = "common_name") %>% 
  left_join(vel, by = "common_name") %>% 
  select(common_name, tmp, strategy, timing, velocity) %>% 
  filter(common_name %in% fish_count_with_zeros$common_name)


save(final_traits, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_traits.RData")
