

library(corrplot)
library(sf)
library(tidyverse)
library(caret)



#fish data
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_occurrence.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_count_with_zeros.RData")
fish_shp <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/sppdata/all_fish_event.shp")

#fish event and hydrography join
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_event_huc_join.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_event_flowline_join.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_event_flowlineV2_join.RData")

#USGS gauge data
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/flowmetrics/survey_huc8flowmetrics.RData")

#SHEDs temp data
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_huc12.RData")
load(file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/sheds_temp_metrics_annaul.RData")


#watershed shape files
huc8 <- st_read("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/NHDplus/WBDHU8/WBDHU8_NE.shp")


#USFS flow metrics
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_historical_crop.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_endofcen_crop.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/SpatialData/USFS flow metrics/flowmet_midcen_crop.RData")


#StreamCat
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_census.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_landcov.RData") 
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcatByyr_imp.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcat_byCOMID.RData")


#dams
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc12dams.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc10dams.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/huc8dams.RData")

#mussel event and hydrography join:
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_event_huc_join.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_event_flowlineV2_join.RData")

#water quality - I added this in later because we'll use it in the mussel model.
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcat_byCOMID_waterquality.RData")



event <- fish_event_huc_join %>% 
  data.frame() %>% 
  dplyr::select(UID, state, year, month, huc8_name, huc10_name, huc12_name, huc8_tnmid, huc10_tnmid, huc12_tnmid, huc8_areasqkm, huc10_areasqkm, huc12_areasqkm)

##these are the USGS gauges at the HUC8 pour points. were not going to include these
#flow <- sites %>% 
#  select(huc8_name, year, month, 14:22)
##join huc8 flow to event data
#dat <- left_join(event, flow, by = c("huc8_name", "year", "month"))



#join the temperature data at the huc12 scale by time period
dat <- event %>% 
  mutate(timeperiod = ifelse(year <= 1999, "pre", "post")) %>% 
  left_join(metrics12, by = c("huc12_name", "timeperiod"))

#join the temperature data at the huc12 scale by year
names(metrics_annual)[3:7] <- paste("annual", names(metrics_annual)[3:7], sep = "_")

#the temperature model is only from 1980 through 2020.  Its okay for pre 1980 bc we weren't going to use earlier data, but for 2021 fish surveys, well join them to the 2020 temperature
dat$year2 <- ifelse(dat$year == 2021, 2020, dat$year)
metrics_annual <- metrics_annual %>% 
  rename(year2 = year)
dat <- left_join(dat, metrics_annual, by = c("huc12_name", "year2")) %>% 
  dplyr::select(-year2)


#join the flowlineV2 with the COMID
fish_event_flowlineV2_join <- fish_event_flowlineV2_join %>% 
  mutate(long = unlist(map(fish_event_flowlineV2_join$geometry, 1)),
         lat = unlist(map(fish_event_flowlineV2_join$geometry, 2))) %>% 
  data.frame() %>% 
  dplyr::select(-geometry)

dat <- dat %>% 
  dplyr::select(-c(state, year, month)) %>% 
  left_join(fish_event_flowlineV2_join, by = "UID")



#join to the USFS flow metrics
flowmet_historical_crop <- flowmet_historical_crop %>% 
  data.frame() %>% 
  dplyr::select(COMID, TOTDASQKM, WBAREATYPE, 9:34) %>% 
  mutate(COMID = as.integer(COMID))
dat <- dat %>% 
  left_join(flowmet_historical_crop, by = "COMID")


#join to the streamcat data that is not varied by year
strmcatByyr_census <- strmcatByyr_census %>% 
  dplyr::select(-year)
dat <- dat %>% 
  left_join(strmcat_byCOMID, by = "COMID")
dat <- dat %>% 
  left_join(strmcatByyr_census, by = "COMID")#this is the 2010 data... we might want to do this year in the future..


#now we want to join to the land cover data that does vary by year.  This layer has 2001, 2011 and 2019.  
#we can assign any fish survey <2004 to 2001, 2005-2014 to 2011, and 2015 to 2019 to 2019
dat <- dat %>% 
  mutate(year2 = ifelse(year <=2004, 2001,
                ifelse(year >= 2005 & year <= 2014, 2011, 2019)))
strmcatByyr_landcov <- strmcatByyr_landcov %>% 
  mutate(year2 = as.numeric(year)) %>% 
  dplyr::select(-year)

dat <- dat %>% 
  left_join(strmcatByyr_landcov, by = c("COMID", "year2")) %>% 
  dplyr::select(-year2)

#now we want to join to the imperviousness that does vary by year.  This layer has 2001" "2004" "2006" "2008" "2011" "2013" "2016" "2019"  
#we can assign any fish survey 
dat <- dat %>% 
  mutate(year2 = ifelse(year <=2002, 2001,
                        ifelse(year >= 2003 & year <= 2004, 2004,
                               ifelse(year >=2005 & year <=2006, 2006,
                                      ifelse(year >=2007 & year <=2008, 2008,
                                             ifelse(year>= 2009 & year <=2011, 2011,
                                                    ifelse(year >= 2012 & year <= 2014, 2013,
                                                           ifelse(year >= 2015 & year <= 2017, 2016,
                                                                  2019))))))))

strmcatByyr_imp <- strmcatByyr_imp %>% 
  mutate(year2 = as.numeric(year)) %>% 
  dplyr::select(-year)

dat <- dat %>% 
  left_join(strmcatByyr_imp, by = c("COMID", "year2")) %>% 
  dplyr::select(-year2)


#combine forest, urban, ag, and wetland

dat <- dat %>% 
  mutate(pctForest_ws = PctDecid_Ws + PctConif_Ws + PctMxFst_Ws,
         pctForest_Cat = PctDecid_Cat + PctConif_Cat + PctMxFst_Cat,
         pctUrban_ws = PctUrbOp_Ws + PctUrbLo_Ws + PctUrbMd_Ws + PctUrbHi_Ws,
         pctUrban_Cat = PctUrbOp_Cat + PctUrbLo_Cat + PctUrbMd_Cat + PctUrbHi_Cat,
         pctAg_Ws = PctHay_Ws + PctCrop_Ws,
         pctAg_Cat = PctHay_Cat + PctCrop_Cat,
         pctWetland_Ws = PctWdWet_Ws + PctHbWet_Ws,
         pctWetland_Cat = PctWdWet_Cat + PctHbWet_Cat) %>% 
  dplyr::select(-c(PctDecid_Ws, PctConif_Ws, PctMxFst_Ws,
            PctDecid_Cat, PctConif_Cat, PctMxFst_Cat,
            PctUrbOp_Ws, PctUrbLo_Ws, PctUrbMd_Ws, PctUrbHi_Ws,
            PctUrbOp_Cat, PctUrbLo_Cat, PctUrbMd_Cat, PctUrbHi_Cat,
            PctHay_Ws, PctCrop_Ws,
            PctHay_Cat, PctCrop_Cat,
            PctWdWet_Ws, PctHbWet_Ws,
            PctWdWet_Cat, PctHbWet_Cat))

#join state dam data
dat <- left_join(dat, huc12dams, by = c("huc12_tnmid", "huc12_name"))
dat <- left_join(dat, huc10dams, by = c("huc10_tnmid", "huc10_name"))
dat <- left_join(dat, huc8dams, by = c("huc8_tnmid", "huc8_name"))

#organize dataframe
dat <- dat %>% 
  dplyr::select("UID", "COMID", "lat", "long",
         "GNIS_ID", "GNIS_NAME", "state", 
         "date", "year", "month", "timeperiod",   
         "waterbody", "WBAREATYPE", "project", "source", "event_to_flowln_dist_m","REACHCODE", "huc8_name", "huc10_name", "huc12_name",
         "huc8_tnmid", "huc10_tnmid", "huc12_tnmid",
         "LENGTHKM", "huc8_area_new", "huc10_area_new", "huc12_area_new", "WsAreaSqKm", 
         12:21, 39:120, 122:123, 125:126)

#we did not include 'TOTDASQKM' from the USGS flow metrics because it was identical and sourced from 'WsAreaSqKm' in StreamCat
#huc10_area_new --> these are new areas caluated in R using the huc12,10,8 spatial datafiles after realzieing that some of the 
#calulations inherent in these files were wrong. They are in m2, which is confusing and best to convert to sqkm!




colSums(is.na(dat))


str(dat)

#variance across study area

nzv <- nearZeroVar(dat[,28:122], saveMetrics = T)

nzv$metric <- row.names(nzv)
remove <- nzv$metric[nzv$zeroVar == TRUE | nzv$nzv == TRUE]
#removes: "mean_n_day_gt_22" "DamDensCat"       "DamNIDStorCat"    "DamNrmStorCat"    "DamDensWs"        "DamNIDStorWs"     "DamNrmStorWs"     "NABD_DensCat"    
# "NABD_NIDStorCat"  "NABD_NrmStorCat"  "NABD_DensWs"      "NABD_NIDStorWs"   "NABD_NrmStorWs"   "PctIce_Cat"       "PctBl_Cat"        "PctIce_Ws"       

dat2 <- dat %>% 
  dplyr::select(-all_of(remove))

#look for correlation between variables from the same dataset (SHEDs, huc8 streamflow, USGS flow metrics, Streamcat, metadata (lat,long, watershed size, elev, length) )

sheds <- dat2 %>% 
  dplyr::select(29:37)
sheds <- sheds[complete.cases(sheds),]
cor <- cor(sheds, method = c("spearman"))

png(height = 10, width = 15, file = "tmpfigures/shedscorrplot.png", units = "in", res = 150, type = "cairo")
corrplot(cor, type = "lower")
dev.off()
#these are all incredible correlated so we will just keep one - annual_mean_summer_temp

keep2 <- "annual_mean_summer_temp"  
#this removes "max_temp", "mean_jul_temp", "mean_summer_temp", "mean_max_temp_30d", "annual_max_temp",
#              "annual_mean_jul_temp", "annual_mean_summer_temp", "annual_mean_n_day_gt_22" "annual_mean_max_temp_30d"


usfs <- dat2 %>% 
  dplyr::select(38:63)
usfs <- usfs[complete.cases(usfs),] #remove rows with NA
cor <- cor(usfs, method = c("spearman")) #make correlation matrix

png(height = 10, width = 15, file = "tmpfigures/usfscorrplot.png", units = "in", res = 150, type = "cairo")
corrplot(cor, type = "lower") #plot
dev.off()

round(colMeans(cor[1:22, 1:22]), 2)
#all of the magnitude variables are very correlated so we will just keep one, Mean JJA, and then we will keep the other non-magnitude variables, which were not correlated at all to anything esle

keep3 <- c("MJJA_HIST","BFI_HIST", "LO7Q1DT_HIST", "CFM_HIST", "W95_HIST") 
#this removes "MA_HIST", "MJAN_HIST","MFEB_HIST", "MMAR_HIST", "MAPR_HIST", "MMAY_HIST", "MJUN_HIST", 
#             "MJUL_HIST","MAUG_HIST", "MSEP_HIST", "MOCT_HIST", "MNOV_HIST", "MDEC_HIST", "MDJF_HIST", 
#             "MMAM_HIST", "MSON_HIST", "HIQ1_5_HIST", "HIQ10_HIST", "HIQ25_HIST", 
#             "LO7Q1_HIST", "LO7Q10_HIST"

strmct <- dat2 %>% 
  dplyr::select(64:100)
strmct <- strmct[complete.cases(strmct),] #remove rows with NA
cor <- cor(strmct, method = c("spearman")) #make correlation matrix

png(height = 10, width = 15, file = "tmpfigures/strmcatcorrplot.png", units = "in", res = 150, type = "cairo")
corrplot(cor, type = "lower") #plot
dev.off()

keep4 <-  c("BFIWs", "ElevCat", "RdDensCatRp100", "RdDensWsRp100", "RdCrsCat", "RdCrsWs",
            "WtDepWs", "PopDen_Ws", "PctOw_Ws", "PctOw_Cat", 
            "PctImp_Cat", "PctImp_Ws", "PctImp_CatRp100", "PctImp_WsRp100",
            "pctForest_Cat", "pctForest_ws", "pctUrban_Cat", "pctUrban_ws",
            "pctAg_Ws", "pctWetland_Cat", "pctWetland_Ws")



statedams <- dat2 %>% 
  dplyr::select(101:106)
statedams <- statedams[complete.cases(statedams),] #remove rows with NA
cor <- cor(statedams, method = c("spearman")) #make correlation matrix

corrplot(cor, type = "lower") #plot


keep5 <-  c("huc12_damden_sqkm", "huc8_damcount")




dat3 <- dat2 %>% 
  dplyr::select(UID, lat, long, state, year, month, source, huc8_name,WsAreaSqKm, all_of(keep2), all_of(keep3), all_of(keep4), all_of(keep5)) 

#now check for correlation among the retained predictors
final <- dat3 %>% 
  dplyr::select(2,3, 9:38)
final <- final[complete.cases(final),] #remove rows with NA
cor <- cor(final, method = c("spearman")) #make correlation matrix

png(height = 10, width = 15, file = "tmpfigures/fishcovcorrplot.png", units = "in", res = 150, type = "cairo")
corrplot(cor, type = "lower") #plot
dev.off()



#first make histograms of covariates to see whats normally distributed
dat4 <- dat3 %>% 
  dplyr::select(2,3, 9:38)

for (i in 1:32) {
  
  ggplot(data = dat4, mapping = aes(x = dat4[[i]]))+
    geom_histogram(binwidth = )+
    labs(x = names(dat4)[i])
  
  ggsave(filename = paste("C:/Users/jenrogers/Documents/git/FreshwaterBio/FreshwaterBio/tmpfigures/covariate_histograms/", names(dat4)[i], ".png", sep = ""),
         plot = last_plot())
  
}


#log transform certain variables
fishcovariates <- dat3 %>% 
  mutate(logWsAreaSqKm = log(WsAreaSqKm+1),
         logMJJA_HIST = log(MJJA_HIST+1),
         logRdCrsCat = log(RdCrsCat+1),
         logPctOw_Cat = log(PctOw_Cat+1)) %>% 
  dplyr::select(-WsAreaSqKm, -MJJA_HIST, -RdCrsCat, -PctOw_Cat )



#this is the covariates for the fish covariates in the baseline years
save(fishcovariates, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")

load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")





#make this df by huc12
fishcovariates_byhuc12 <- left_join(fishcovariates, fish_event_huc_join, by = c("UID", "state", "year", "month", "source", "huc8_name")) %>% 
  group_by(huc12_tnmid, huc12_name) %>% 
  summarise(lat = mean(lat, na.rm = T),
            long = mean(long, na.rm = T),
            annual_mean_summer_temp = mean(annual_mean_summer_temp, na.rm = T),
            BFI_HIST = mean(BFI_HIST, na.rm = T),
            LO7Q1DT_HIST = mean(LO7Q1DT_HIST, na.rm = T),
            CFM_HIST = mean(CFM_HIST, na.rm = T),
            W95_HIST = mean(W95_HIST, na.rm = T),
            BFIWs = mean(BFIWs, na.rm = T),
            ElevCat = mean(ElevCat, na.rm = T),
            RdDensCatRp100 = mean(RdDensCatRp100, na.rm = T),
            RdDensWsRp100 = mean(RdDensWsRp100, na.rm = T),
            RdCrsWs = mean(RdCrsWs, na.rm = T),
            WtDepWs = mean(WtDepWs, na.rm = T),
            PopDen_Ws = mean(PopDen_Ws, na.rm = T),
            PctOw_Ws = mean(PctOw_Ws, na.rm = T),
            PctImp_Cat = mean(PctImp_Cat, na.rm = T),
            PctImp_Ws = mean(PctImp_Ws, na.rm = T),
            PctImp_CatRp100 = mean(PctImp_CatRp100, na.rm = T),
            PctImp_WsRp100 = mean(PctImp_WsRp100, na.rm = T),
            pctForest_Cat = mean(pctForest_Cat, na.rm = T),
            pctForest_ws = mean(pctForest_ws, na.rm = T),
            pctUrban_Cat = mean(pctUrban_Cat, na.rm = T),
            pctUrban_ws = mean(pctUrban_ws, na.rm = T),
            pctAg_Ws = mean(pctAg_Ws, na.rm = T),
            pctWetland_Cat = mean(pctWetland_Cat, na.rm = T),
            pctWetland_Ws = mean(pctWetland_Ws, na.rm = T),
            huc12_damden_sqkm = mean(huc12_damden_sqkm, na.rm = T),
            huc8_damcount = mean(huc8_damcount, na.rm = T),
            logWsAreaSqKm = mean(logWsAreaSqKm, na.rm = T),
            logMJJA_HIST = mean(logMJJA_HIST, na.rm = T),
            logRdCrsCat = mean(logRdCrsCat, na.rm = T),
            logPctOw_Cat = mean(logPctOw_Cat, na.rm = T))

save(fishcovariates_byhuc12, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates_byhuc12.RData")





##########################################################

##########################################################

##########################################################


#prepare the mussel model covariates

#these will be at the HUC10 scale

#assign every COMID in the region a HUC10 name.  
#We did this in the tidying_NHDplusV2.R script, and also joined all the fish covariates.
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_covariates.RData")


#join to the covariate data by COMID
#we already did that in the file loaded above, but we want to add in the water quality data specifically for mussels
#remove highly correlated water quality variables
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/strmcat_byCOMID_waterquality.RData")

dat <- left_join(NHDplusV2_NewEngCrop, strmcat_byCOMID, by = "COMID")

wq <- dat %>% 
  data.frame %>% 
  dplyr::select(44:76, -NRSA_Frame, -NARS_Region) 
wq <- wq[complete.cases(wq),] #remove rows with NA
cor <- cor(wq, method = c("spearman")) #make correlation matrix

corrplot(cor, type = "lower") #plot

dat <- dat %>% 
  mutate(pollutionWs = SuperfundDensWs + NPDESDensWs + TRIDensWs,
         nitWs = ManureWs + FertWs) %>% 
  select(-c(SuperfundDensWs, NPDESDensWs, TRIDensWs, ManureWs, FertWs,
            AgKffactCat, AgKffactWs, KffactCat,
            WWTPAllDensCat, WWTPAllDensWs,
            WWTPMinorDensCat, WWTPMajorDensCat,
            CaOCat, 
            CBNFCat, CBNFWs, FertCat,
            ManureCat, SuperfundDensCat, NPDESDensCat, TRIDensCat, NRSA_Frame,NARS_Region,prG_BMMI,
            NANICat, NANIWs, Pestic97Cat, Pestic97Ws, RunoffCat))


wq <- dat %>% 
  data.frame %>% 
  dplyr::select(10:50) 
wq <- wq[complete.cases(wq),] #remove rows with NA
cor <- cor(wq, method = c("spearman")) #make correlation matrix

corrplot(cor, type = "lower") #plot


#add the superfun, TRI, and NPDES densities
#add manure and fertelizer used in watershed
#keep CaO
#remove AgKffact becuase its highly correlated with nitrogen
#keep Kffact in watershed overall
#keep runoff
#keep major and minor wastewater treatment plants


#average each covariate by HUC10 (either take the max or mean value)

dat_huc10 <- dat %>% 
  data.frame() %>% 
  group_by(huc10_name, huc10_tnmid) %>% 
  summarise(lat = mean(lat, na.rm = T),
            long = mean(long, na.rm = T),
            annual_mean_summer_temp = mean(annual_mean_summer_temp, na.rm = T),
            BFI_HIST = mean(BFI_HIST, na.rm = T),
            LO7Q1DT_HIST = mean(LO7Q1DT_HIST, na.rm = T), 
            CFM_HIST = mean(CFM_HIST, na.rm = T), 
            W95_HIST = mean(W95_HIST, na.rm = T),
            ElevCat = mean(ElevCat, na.rm = T),
            RdCrsWs = mean(RdCrsWs, na.rm = T),
            WtDepWs = mean(WtDepWs, na.rm = T),
            PctOw_Ws = mean(PctOw_Ws, na.rm = T),
            PctImp_WsRp100 = mean(PctImp_WsRp100, na.rm = T),
            pctForest_ws = mean(pctForest_ws, na.rm = T),
            pctAg_Ws = mean(pctAg_Ws, na.rm = T),
            pctWetland_Ws = mean(pctWetland_Ws, na.rm = T),
            huc12_damden_sqkm = mean(huc12_damden_sqkm, na.rm = T),
            huc8_damcount = mean(huc8_damcount, na.rm = T),
            logWsAreaSqKm = mean(logWsAreaSqKm, na.rm = T),
            logMJJA_HIST = mean(logMJJA_HIST, na.rm = T),
            logRdCrsCat = mean(logRdCrsCat, na.rm = T),
            logPctOw_Cat = mean(logPctOw_Cat, na.rm = T),
            CaOWs = mean(CaOWs, na.rm = T),
            KffactWs = mean(KffactWs, na.rm = T),
            RunoffWs = mean(RunoffWs, na.rm = T),
            WWTPMajorDensWs = mean(WWTPMajorDensWs, na.rm = T),
            WWTPMinorDensWs = mean(WWTPMinorDensWs, na.rm = T),
            pollutionWs = mean(pollutionWs, na.rm = T),
            nitWs = mean(nitWs, na.rm = T))

#save the mussel covariate datafile that has all of the HUC10 inclued
NHDplusV2_NewEngCrop_mussel_covariates_HUC10 <- dat_huc10
save(NHDplusV2_NewEngCrop_mussel_covariates_HUC10, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_mussel_covariates_HUC10.RData")


#join to the mussel_event_huc_join so we can get the UIDs of each sampling event that occurred in that watershed
dat_huc10 <- left_join(mussel_event_huc_join, dat_huc10, by = c("huc10_name", "huc10_tnmid"))
mussel_covariates_huc10 <- dat_huc10

#save the mussel covariate datafile that is linked to the mussel observation data
save(mussel_covariates_huc10, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_covariates_byhuc10.RData")





#update July 5, 2023 - instead of modeling mussels at the huc10 scle, we want to model them at the stream reach scale, 
#and then average the results to the HUC10 scale - this is because 
# Actually - we will try modeling at the HUC12 scale as a compromise, it turns out the mussel event flowline file losing a lot of the mussel data
#because the mussel surveys are too far from the flow lines to join them properly.

#average each covariate by HUC10 (either take the max or mean value)

dat_huc12 <- dat %>% 
  data.frame() %>% 
  group_by(huc12_name, huc12_tnmid) %>% 
  summarise(lat = mean(lat, na.rm = T),
            long = mean(long, na.rm = T),
            annual_mean_summer_temp = mean(annual_mean_summer_temp, na.rm = T),
            BFI_HIST = mean(BFI_HIST, na.rm = T),
            LO7Q1DT_HIST = mean(LO7Q1DT_HIST, na.rm = T), 
            CFM_HIST = mean(CFM_HIST, na.rm = T), 
            W95_HIST = mean(W95_HIST, na.rm = T),
            ElevCat = mean(ElevCat, na.rm = T),
            RdCrsWs = mean(RdCrsWs, na.rm = T),
            WtDepWs = mean(WtDepWs, na.rm = T),
            PctOw_Ws = mean(PctOw_Ws, na.rm = T),
            PctImp_WsRp100 = mean(PctImp_WsRp100, na.rm = T),
            pctForest_ws = mean(pctForest_ws, na.rm = T),
            pctAg_Ws = mean(pctAg_Ws, na.rm = T),
            pctWetland_Ws = mean(pctWetland_Ws, na.rm = T),
            huc12_damden_sqkm = mean(huc12_damden_sqkm, na.rm = T),
            huc8_damcount = mean(huc8_damcount, na.rm = T),
            logWsAreaSqKm = mean(logWsAreaSqKm, na.rm = T),
            logMJJA_HIST = mean(logMJJA_HIST, na.rm = T),
            logRdCrsCat = mean(logRdCrsCat, na.rm = T),
            logPctOw_Cat = mean(logPctOw_Cat, na.rm = T),
            CaOWs = mean(CaOWs, na.rm = T),
            KffactWs = mean(KffactWs, na.rm = T),
            RunoffWs = mean(RunoffWs, na.rm = T),
            WWTPMajorDensWs = mean(WWTPMajorDensWs, na.rm = T),
            WWTPMinorDensWs = mean(WWTPMinorDensWs, na.rm = T),
            pollutionWs = mean(pollutionWs, na.rm = T),
            nitWs = mean(nitWs, na.rm = T))

#save the mussel covariate datafile that has all of the HUC12 inclued
NHDplusV2_NewEngCrop_mussel_covariates_HUC12 <- dat_huc12
save(NHDplusV2_NewEngCrop_mussel_covariates_HUC12, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/NHDplusV2_NewEngCrop_mussel_covariates_HUC12.RData")


#join to the mussel_event_huc_join so we can get the UIDs of each sampling event that occurred in that watershed
dat_huc12 <- left_join(mussel_event_huc_join, dat_huc12, by = c("huc12_name", "huc12_tnmid"))
mussel_covariates_huc12 <- dat_huc12

#save the mussel covariate datafile that is linked to the mussel observation data
save(mussel_covariates_huc12, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_covariates_byhuc12.RData")


