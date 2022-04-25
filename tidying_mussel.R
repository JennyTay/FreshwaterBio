#linking EO data (which is the data about the time of survey and what was found) to the source file, which 

library(sf)
library(tidyverse)

#Vermont natural heritage
st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb")

rare <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb", layer = "VT_RTE_Animals")
uncom <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/VT Mussel Data/Heritage_data.gdb", layer = "VT_Uncommon_animals")


#select just the freshwater mussels
rare <- rare %>% 
  filter(INFORMAL_TAXONOMY == 'Freshwater Mussels')

uncom <- uncom %>% 
  filter(INFORMAL_TAXONOMY == 'Freshwater Mussels')




#MA natural heritage
st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_EOReps.gdb")
ma_eo <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_EOReps.gdb", layer = "query_result")


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcLine.gdb")
ma_srcline <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcLine.gdb", layer = "query_result")


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPolys.gdb")
ma_srcpoly <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPolys.gdb", layer = "query_result")


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPts.gdb")
ma_srcpoint <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/NHESP_Aquatics_SrcPts.gdb", layer = "query_result")


#select just the freshwater mussels
unique(ma_eo$Sname)
ma_eo <- ma_eo %>% 
  filter(Sname %in% c("Ligumia nasuta", "Alasmidonta undulata", "Lampsilis cariosa", "Strophitus undulatus", 
                      "Leptodea ochracea", "Alasmidonta heterodon", "Alasmidonta varicosa"))

ma_srcline <- ma_srcline %>% 
  filter(Sname %in% c("Ligumia nasuta", "Alasmidonta undulata", "Lampsilis cariosa", "Strophitus undulatus", 
                      "Leptodea ochracea", "Alasmidonta heterodon", "Alasmidonta varicosa"))

ma_srcpoly <- ma_srcpoly %>% 
  filter(Sname %in% c("Ligumia nasuta", "Alasmidonta undulata", "Lampsilis cariosa", "Strophitus undulatus", 
                      "Leptodea ochracea", "Alasmidonta heterodon", "Alasmidonta varicosa"))

ma_srcpoint <- ma_srcpoint %>% 
  filter(Sname %in% c("Ligumia nasuta", "Alasmidonta undulata", "Lampsilis cariosa", "Strophitus undulatus", 
                      "Leptodea ochracea", "Alasmidonta heterodon", "Alasmidonta varicosa"))

# remove spatial information from MA shapefiles
ma_eo <- data.frame(ma_eo) %>% 
  select(Sname, SComName, EO_ID, EO_DATA)

ma_srcline <- data.frame(ma_srcline) %>% 
  select(Sname, EO_ID, src_descr, src_lastobs)

ma_srcpoly <- data.frame(ma_srcpoly) %>% 
  select(Sname, EO_ID, src_descr, src_lastobs)

ma_srcpoint <- data.frame(ma_srcpoint) %>% 
  select(Sname, EO_ID, src_descr, src_lastobs)

#join the ma_eo df to the three src files so the src files can have the EO_DATA field
ma_srcline <- left_join(ma_srcline, ma_eo, by = c("EO_ID", "Sname")) %>% 
  arrange(EO_ID)
ma_srcpoint <- left_join(ma_srcpoint, ma_eo, by = c("EO_ID", "Sname"))
ma_srcpoly <- left_join(ma_srcpoly, ma_eo, by = c("EO_ID", "Sname"))

write.csv(ma_srcline, "C:/Users/jenrogers/Documents/necascFreshwaterBio/spp_data/MA NHESP Mussel Data/srcline_eo_join.csv")

          


st_layers(dsn = "C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/NHDPLUS_H_0101_HU4_GDB.gdb")
