
library(corrplot)

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







event <- fish_event_huc_join %>% 
  data.frame() %>% 
  select(UID, state, year, month, huc8_name, huc12_name, huc8_areasqkm, huc12_areasqkm)

flow <- sites %>% 
  select(huc8_name, year, month, 14:22)


#join huc8 flow to event data
dat <- left_join(event, flow, by = c("huc8_name", "year", "month"))



#join the temperature data at the huc12 scale by time period
dat <- dat %>% 
  mutate(timeperiod = ifelse(year <= 1999, "pre", "post")) %>% 
  left_join(metrics12, by = c("huc12_name", "timeperiod"))

#join the temperature data at the huc12 scale by year
names(metrics_annual)[3:7] <- paste("annual", names(metrics_annual)[3:7], sep = "_")
dat <- left_join(dat, metrics_annual, by = c("huc12_name", "year"))



#join the flowlineV2 with the COMID
fish_event_flowlineV2_join <- fish_event_flowlineV2_join %>% 
  mutate(long = unlist(map(fish_event_flowlineV2_join$geometry, 1)),
         lat = unlist(map(fish_event_flowlineV2_join$geometry, 2))) %>% 
  data.frame() %>% 
  select(-geometry)

dat <- dat %>% 
  select(-c(state, year, month)) %>% 
  left_join(fish_event_flowlineV2_join, by = "UID")



#join to the USFS flow metrics
flowmet_historical_crop <- flowmet_historical_crop %>% 
  data.frame() %>% 
  select(COMID, TOTDASQKM, WBAREATYPE, 9:34) %>% 
  mutate(COMID = as.integer(COMID))
dat <- dat %>% 
  left_join(flowmet_historical_crop, by = "COMID")


#join to the streamcat data
strmcatByyr_census <- strmcatByyr_census %>% 
  select(-year)
strmcatByyr_landcov <- strmcatByyr_landcov %>% 
  select(-year)

dat <- dat %>% 
  left_join(strmcat_byCOMID, by = "COMID")
dat <- dat %>% 
  left_join(strmcatByyr_census, by = "COMID")#this is the 2010 data... we might want to do this year in the future..
dat <- dat %>% 
  left_join(strmcatByyr_landcov, by = "COMID")#this is 2001 NLCD data.. we probably will want to download a coupel more, but for now, I just did one year. they also have some that just look at the riparian buffer..
dat <- dat %>% 
  left_join(strmcatByyr_imp, by = c("COMID", "timeperiod"))


#we probably want to add in data for road crossings and riparian area 

#organize dataframe
dat <- dat %>% 
  select("UID", "COMID", "lat", "long",
         "GNIS_ID", "GNIS_NAME", "state", 
         "date", "year", "month", "timeperiod",   
         "waterbody", "WBAREATYPE", "project", "source", "event_to_flowln_dist_m","REACHCODE", "huc8_name", "huc12_name", 
         "LENGTHKM", "huc8_areasqkm",  "huc12_areasqkm", "WsAreaSqKm", 
         "q_mean_rel", "q_max_rel" ,"low_dur_days", "high_dur_days", "min_month", "monthly_min_flow", 
         "max_month", "monthly_max_flow", "max_temp", "mean_jul_temp", "mean_summer_temp", 
         "mean_max_temp_30d", "mean_n_day_gt_22",  "annual_max_temp", "annual_mean_jul_temp", 
         "annual_mean_summer_temp", "annual_mean_max_temp_30d", "annual_mean_n_day_gt_22", 
         "MA_HIST", "MJAN_HIST",  "MFEB_HIST", "MMAR_HIST", "MAPR_HIST", "MMAY_HIST", 
         "MJUN_HIST", "MJUL_HIST", "MAUG_HIST", "MSEP_HIST", "MOCT_HIST", "MNOV_HIST", "MDEC_HIST", "MDJF_HIST", 
         "MMAM_HIST" , "MJJA_HIST" , "MSON_HIST", "HIQ1_5_HIST", "HIQ10_HIST", "HIQ25_HIST", "LO7Q1_HIST", 
         "LO7Q10_HIST" ,"BFI_HIST", "LO7Q1DT_HIST", "CFM_HIST", "W95_HIST",
         "BFIWs", "DamDensWs", "DamNIDStorWs", "DamNrmStorWs", "ElevWs", "NABD_DensWs", 
         "NABD_NIDStorWs", "NABD_NrmStorWs", "WtDepWs", "HUDen_Ws",
         "PopDen_Ws", "PctOw_Ws", "PctIce_Ws","PctUrbOp_Ws", "PctUrbLo_Ws", "PctUrbMd_Ws", "PctUrbHi_Ws",
         "PctBl_Ws", "PctDecid_Ws", "PctConif_Ws", "PctMxFst_Ws", "PctShrb_Ws", "PctGrs_Ws", "PctHay_Ws", 
         "PctCrop_Ws", "PctWdWet_Ws", "PctHbWet_Ws", "PctImp_Ws", "PctImp_WsRp100")

#we did not include 'TOTDASQKM' from the USGS flow metrics because it was identical and sourced from 'WsAreaSqKm' in StreamCat








colSums(is.na(dat))


str(dat)

#variance across study area

nzv <- nearZeroVar(dat[,23:96], saveMetrics = T)

nzv$metric <- row.names(nzv)
remove <- nzv$metric[nzv$zeroVar == TRUE | nzv$nzv == TRUE]


dat2 <- dat %>% 
  select(-remove)

#look for correlation between variables from the same dataset (SHEDs, huc8 streamflow, USGS flow metrics, Streamcat, metadata (lat,long, watershed size, elev, length) )


usgsgague <- dat2 %>% 
  select(24:31) 
usgsgague <- usgsgague[complete.cases(usgsgague),] #remove rows with NA
cor <- cor(usgsgague, method = c("spearman")) #make correlation matrix
corrplot(cor, type = "lower") #plot
#removes correlatioms 0.5 and higher
keep1 <- c("low_dur_days", "high_dur_days", "min_month", "monthly_min_flow", "max_month") 
#removes "q_mean_rel", "q_max_rel", "monthly_max_flow

sheds <- dat2 %>% 
  select(32:40)
sheds <- sheds[complete.cases(sheds),]
cor <- cor(sheds, method = c("spearman"))
corrplot(cor, type = "lower")
#these are all incredible correlated so we will just keep one - annual_mean_max_temp_30d

keep2 <- "annual_mean_max_temp_30d" 
#this removes "max_temp", "mean_jul_temp", "mean_summer_temp", "mean_max_temp_30d", "annual_max_temp",
#              "annual_mean_jul_temp", "annual_mean_summer_temp", "annual_mean_n_day_gt_22"


usfs <- dat2 %>% 
  select(41:66)
usfs <- usfs[complete.cases(usfs),] #remove rows with NA
cor <- cor(usfs, method = c("spearman")) #make correlation matrix
corrplot(cor, type = "lower") #plot
#all of the magnitude variables are very correlated so we will just keep one, Mean JJA, and then we will keep the other non-magnitude variables, which were not correlated at all to anything esle

keep3 <- c("MJJA_HIST","BFI_HIST", "LO7Q1DT_HIST", "CFM_HIST", "W95_HIST") 
#this removes "MA_HIST", "MJAN_HIST","MFEB_HIST", "MMAR_HIST", "MAPR_HIST", "MMAY_HIST", "MJUN_HIST", 
#             "MJUL_HIST","MAUG_HIST", "MSEP_HIST", "MOCT_HIST", "MNOV_HIST", "MDEC_HIST", "MDJF_HIST", 
#             "MMAM_HIST", "MSON_HIST", "HIQ1_5_HIST", "HIQ10_HIST", "HIQ25_HIST", 
#             "LO7Q1_HIST", "LO7Q10_HIST"

strmct <- dat2 %>% 
  select(67:87)
strmct <- strmct[complete.cases(strmct),] #remove rows with NA
cor <- cor(strmct, method = c("spearman")) #make correlation matrix
corrplot(cor, type = "lower") #plot
#the high, med, low, and impervious surface, and housing density, and population are all correlated, but not very very high... we migth want to do a pca with these, though that will make it harder to interpret.
keep4 <-  c("BFIWs", "ElevWs", "WtDepWs", "PctOw_Ws", "PctUrbOp_Ws",
            "PctBl_Ws", "PctDecid_Ws", "PctConif_Ws", "PctMxFst_Ws", "PctShrb_Ws", "PctGrs_Ws",
            "PctHay_Ws", "PctWdWet_Ws", "PctHbWet_Ws", "PctImp_Ws", "PctImp_WsRp100")
#removes: "PctUrbLo_Ws", "PctUrbMd_Ws", "PctUrbHi_Ws", "PopDen_Ws", "HUDen_Ws" --> all were correlated with pctimpervious


dat3 <- dat2 %>% 
  select(UID, lat, long, state, year, month, source, huc8_name,WsAreaSqKm, all_of(keep1), all_of(keep2), all_of(keep3), all_of(keep4)) %>% 
  mutate(pctforest_Ws = PctDecid_Ws + PctConif_Ws + PctMxFst_Ws) %>%  #make a new variable for percent forest in genearl
  select(-PctDecid_Ws, -PctConif_Ws, -PctMxFst_Ws) #remove the three specific types of forest

#now check for correlation among the retained predictors
final <- dat3 %>% 
  select(2,3, 9:34)
final <- final[complete.cases(final),] #remove rows with NA
cor <- cor(final, method = c("spearman")) #make correlation matrix
corrplot(cor, type = "lower") #plot




#first make histograms of covariates to see whats normally distributed
dat4 <- dat3 %>% 
  select(2,3, 9:34)

for (i in 1:28) {
  
  ggplot(data = dat4, mapping = aes(x = dat4[[i]]))+
    geom_histogram(binwidth = )+
    labs(x = names(dat4)[i])
  
  ggsave(filename = paste("C:/Users/jenrogers/Documents/git/FreshwaterBio/FreshwaterBio/tmpfigures/covariate_histograms/", names(dat4)[i], ".png", sep = ""),
         plot = last_plot())
  
}


#log transform certain variables
dat3 <- dat3 %>% 
  mutate(logWsAreaSqKm = log(WsAreaSqKm),
         logMJJA_HIST = log(MJJA_HIST)) %>% 
  select(-WsAreaSqKm, -MJJA_HIST )

save(dat3, file = "C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")
