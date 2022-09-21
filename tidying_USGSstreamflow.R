#create site and date csv

library(sf)
library(tidyverse)
library(readxl)
library(lubridate)
#########loading event data files#############




#Create properly formated flow file

#create initial file
qin <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/USGSFlowData/Allagash.xlsx",
                  col_types = c("text", "numeric", "date", "numeric", "text", "text"))
names(qin) <- c("agency", "site_no", "Date", "Allagash", "status", "delete")
qin <- qin %>% 
  select(3:4)


file.list <- list.files("C:/Users/jenrogers/Documents/necascFreshwaterBio/USGSFlowData", pattern='*.xlsx')


for (i in 2:41) { #start at 2 because we already made the allagash above
  
  
  tmp <- read_excel(paste("C:/Users/jenrogers/Documents/necascFreshwaterBio/USGSFlowData/", file.list[i], sep = ""),
                    col_types = c("text", "numeric", "date", "numeric", "text", "text"))
  names(tmp) <- c("agency", "site_no", "Date", tmp$huc8_name[1], "status", "delete")
  tmp <- tmp %>% 
    select(3:4)
  
  qin <- full_join(qin, tmp, by = "Date")
  
}

#add together the flows from the three watersheds where I used two USGS gauges
qin <- qin %>% 
  mutate(Deerfield = Deerfield_1 + Deerfield_2,
         Housatonic = Housatonic_1 + Housatonic_2,
         Shetucket = Shetucket_1 + Shetucket_2) %>% 
  select(-Deerfield_1, -Deerfield_2, 
         -Housatonic_1, -Housatonic_2, 
         -Shetucket_1, -Shetucket_2)

qin <- qin[-1,]







#create the sitefile and remove the dates or locations that we do not have flow data for
#load data
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_event_huc_join.RData")
dat <- fish_event_huc_join%>% 
  select(huc8_name, month, year) %>% 
  unique()



#Create "sitefile"
#the sites will be joined back to the event data based on the HUC8 name and the
#year of survey.
sitefile_create<- data.frame(
  "stid" = dat$huc8_name,
  "styr" = NA,
  "stmn" = dat$month,
  "stdy" = 1, 
  "endyr" = dat$year, 
  "endmn" = dat$month,
  "enddy" = dat$year)

sitefile_create<-sitefile_create %>% 
  mutate(styr = endyr-5) %>% 
  filter(stid %in% names(qin)[2:39],
         styr >= 1980) %>% 
  mutate(wateryr = ifelse(endmn %in% c(10,11,12), endyr+1, endyr)) 



save(qin, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/flowmetrics/USGSflowFormatted.RData")
save(sitefile_create, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/flowmetrics/sitefile.RData")





#First we will calucate average values over then entire length of the flow record
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/flowmetrics/USGSflowFormatted.RData")
metrics_all <- qin %>% 
  mutate(year = year(Date),
         month = month(Date),
         wateryr = ifelse(month %in% c(10,11,12), year+1, year)) %>% 
  select(wateryr, month, 2:39) %>% 
  pivot_longer(3:40, names_to = "stid", values_to = "q_cfs") %>% 
  group_by(stid) %>% 
  summarise(q_mean_all = mean(q_cfs, na.rm=T),
            q_max_all = max(q_cfs, na.rm=T),
            q_min_all = min(q_cfs, na.rm=T),
            q_0.1_all = quantile(q_cfs, probs = 0.1, na.rm = T),
            q_0.9_all = quantile(q_cfs, probs = 0.9, na.rm = T))

#then we will calcuate the mean variable for each wateryear/stid of survey

#for the magnitude metrics, we will divide them by the metrics_all so we get relative values
metrics_year_magnitude <- qin %>% 
  mutate(year = year(Date),
         month = month(Date),
         wateryr = ifelse(month %in% c(10,11,12), year+1, year)) %>% 
  select(wateryr, month, 2:39) %>% 
  pivot_longer(3:40, names_to = "stid", values_to = "q_cfs") %>% 
  group_by(stid, wateryr) %>% 
  summarise(q_mean_year = mean(q_cfs, na.rm=T),
            q_max_year = max(q_cfs, na.rm=T),
            q_min_year = min(q_cfs, na.rm=T))

#there is no need to normalize these because they are based on percentiles...
metrics_year_duration <- qin %>% 
  mutate(year = year(Date),
         month = month(Date),
         wateryr = ifelse(month %in% c(10,11,12), year+1, year)) %>% 
  select(wateryr, month, 2:39) %>% 
  pivot_longer(3:40, names_to = "stid", values_to = "q_cfs") %>% 
  left_join(metrics_all, by = "stid") %>% 
  group_by(stid, wateryr) %>% 
  summarise(low_dur_days = length(q_cfs[q_cfs < q_0.1_all]),
            high_dur_days = length(q_cfs[q_cfs > q_0.9_all]))

#metrics year time - driest month
#metrics year rate of change - RBI?
#metrics year frequency - number of storms


load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/flowmetrics/sitefile.RData")
sitefile_create <- left_join(sitefile_create, metrics_all, by = "stid")
sitefile_create <- left_join(sitefile_create, metrics_year, by = c("stid", "wateryr"))

sitefile_create <- sitefile_create %>% 
  mutate(q_mean_rel = q_mean_year/q_mean_all,
         q_max_rel = q_max_year/q_max_all,
         q_min_rel = q_min_year/q_min_all)
