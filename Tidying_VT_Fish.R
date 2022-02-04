library(tidyverse)
library(lubridate)
library(sf)
library(readxl)



#########################################################

################### Vermont DEC ########################

#########################################################
vt <- read_excel("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT DEC Fish Data/VT DEC Fish Data 1990.xlsx",
                 col_names = TRUE)

range(vt$Date)
