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


traits <- dat %>% 
  select(FID, COMMONNAME, 29:115) %>% 
  filter(COMMONNAME %in% names$common_name)

spawntime <- traits %>% 
  select(COMMONNAME, 7:18)

spawnsub <- traits %>% 
  select(COMMONNAME, 21:45)

sub <- traits %>% 
  select(COMMONNAME, 51:60)
  
vel <- traits %>% 
  select(COMMONNAME, 74:76)

habtype <- traits %>% 
  select(COMMONNAME, 62:73)


#left join with the traits assigned in New England
traits <- read_xlsx("C:/Users/jrogers/Documents/necascFreshwaterBio/spp_data/ThermalPreferences.xlsx")
traits <- traits %>% 
  select(common_name, `Temperatures Notes`) %>% 
  rename(tmp = `Temperatures Notes`)

table(traits$tmp)

