




#####  cluster spp based on the variable values by streamflow variables
##### in this code we are using weighted means for each species.
##### for each species, we calucated the weighted mean of each variable (weighted by the proportional abundance at a survey site)
##### then we are using K-means clusting, following a tutorial from this website https://uc-r.github.io/kmeans_clustering

load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_count_with_zeros.RData")

#calculate proportional abundance and then join to the covariate data
test <- fish_count_with_zeros %>% 
  filter(count >0) %>% 
  group_by(UID) %>% 
  mutate(totalcount = sum(count)) %>% 
  ungroup() %>% 
  mutate(propabun = (count / totalcount)) %>% 
  select(UID, common_name, propabun) %>% 
  left_join(fishcovariates, by = "UID")

#remove rows with incomplete data
test <- test[complete.cases(test),]

#select only the variables used in the model, calcuate weighted means of each varaible by species, filter for the species that the proportional abunance model converged for.
test <- test %>% 
  select(common_name, 
         propabun, 
         BFI_HIST, 
         LO7Q1DT_HIST,
         W95_HIST, 
         ElevCat,  
         logWsAreaSqKm, 
         logMJJA_HIST,
         CFM_HIST) %>% 
  mutate(BFI_HIST_2 = BFI_HIST*propabun, 
         LO7Q1DT_HIST_2 = LO7Q1DT_HIST*propabun,
         W95_HIST_2 = W95_HIST*propabun, 
         ElevCat_2 =  ElevCat*propabun,  
         logWsAreaSqKm_2 = logWsAreaSqKm*propabun, 
         CFM_HIST_2 = CFM_HIST*propabun, 
         logMJJA_HIST_2 = logMJJA_HIST*propabun) %>% 
  group_by(common_name) %>% 
  summarise(sum_propabun = sum(propabun), 
            sum_BFI_HIST_2 = sum(BFI_HIST_2), 
            sum_LO7Q1DT_HIST_2 = sum(LO7Q1DT_HIST_2),
            sum_W95_HIST_2 = sum(W95_HIST_2), 
            sum_ElevCat_2 =  sum(ElevCat_2),  
            sum_logWsAreaSqKm_2 = sum(logWsAreaSqKm_2), 
            sum_CFM_HIST_2 = sum(CFM_HIST_2),
            sum_logMJJA_HIST_2 = sum(logMJJA_HIST_2)) %>% 
  ungroup() %>% 
  mutate(BFI_HIST_wm = sum_BFI_HIST_2/sum_propabun, 
         LO7Q1DT_HIST_wm = sum_LO7Q1DT_HIST_2/sum_propabun,
         W95_HIST_wm = sum_W95_HIST_2/sum_propabun, 
         ElevCat_wm =  sum_ElevCat_2/sum_propabun,  
         logWsAreaSqKm_wm = sum_logWsAreaSqKm_2/sum_propabun, 
         CFM_HIST_wm = sum_CFM_HIST_2/sum_propabun,
         logMJJA_HIST_wm = sum_logMJJA_HIST_2/sum_propabun) %>% 
  filter(common_name %in% c("american eel", "white perch", "rosyside dace",  
                            "spotfin shiner", "mimic shiner", "finescale dace", "american brook lamprey", 
                            "eastern silvery minnow", "rosyface shiner", "cutlips minnow", "green sunfish", 
                            "brook stickleback", "common carp", "central mudminnow", "northern pike", "bridle shiner", 
                            "swamp darter", "lake chub", "banded killifish", "margined madtom", "burbot", "black crappie", 
                            "spottail shiner", "redbelly dace", "creek chubsucker", "banded sunfish", "fathead minnow", 
                            "bluntnose minnow", "redbreast sunfish", "rock bass", "rainbow trout", "longnose sucker", 
                            "smallmouth bass", "yellow bullhead", "redfin pickerel", "yellow perch", "brown bullhead", 
                            "atlantic salmon", "golden shiner", "chain pickerel", "bluegill", "brown trout", "tessellated darter", 
                            "largemouth bass", "fallfish", "common shiner", "creek chub", "pumpkinseed", "slimy sculpin", 
                            "longnose dace", "white sucker", "eastern blacknose dace", "brook trout")) %>% 
  data.frame()

#make the common names column into row names so we can cluster and keep only the final weighted mean values
test2 <- test[,-1]
rownames(test2) <- test[,1]
test2 <- test2[,9:15]

#scale the variables
cluster2 <- scale(test2)

sf_cluster <- read.csv("tmpfigures/fishclusters_bystreamflow.csv")


dat <- data.frame(cluster2) %>% 
  mutate(common_name = rownames(cluster2)) %>% 
  left_join(sf_cluster, by = "common_name") 

rownames(dat) <- str_to_title(dat$common_name)
names(dat) <- c("BFI (Ws)", "Low Flow Date", "Winter Floods", "Elev (Cat)", "Watershed Area", 
                "Center Flow Mass", "Summer Flow", "Common Name", "X", "Cluster")

pca <- prcomp(dat[,1:7])


png(height = 10, width = 10, file = "tmpfigures/paper1/paper1rev/fish_cluster_biplot_sf.png", units = "in", res = 300, type = "cairo")


fviz_pca_biplot(pca,  habillage = dat$Cluster, repel = TRUE,
                palette =  "RdBu",
                title = "A",
                pointsize = 3, labelsize = 5,)+
  theme(axis.title  = element_text(size = 20),
        title = element_text(size = 30),
        legend.text = element_text(size=20),
        axis.text = element_text(size = 20))
dev.off()


a <- fviz_pca_biplot(pca,  habillage = dat$Cluster, repel = TRUE,
                     palette =  "RdBu",
                     title = "A",
                     pointsize = 3, labelsize = 5,)+
  theme(axis.title  = element_text(size = 20),
        title = element_text(size = 30),
        legend.text = element_text(size=20),
        axis.text = element_text(size = 20))




#####  cluster spp based on the variable values by stream temperature
##### in this code we are using weighted means for each species.
##### for each species, we calucated the weighted mean of each variable (weighted by the proportional abundance at a survey site)
##### then we are using K-means clusting, following a tutorial from this website https://uc-r.github.io/kmeans_clustering

load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")
load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_count_with_zeros.RData")

#calculate proportional abundance and then join to the covariate data
test <- fish_count_with_zeros %>% 
  filter(count >0) %>% 
  group_by(UID) %>% 
  mutate(totalcount = sum(count)) %>% 
  ungroup() %>% 
  mutate(propabun = (count / totalcount)) %>% 
  select(UID, common_name, propabun) %>% 
  left_join(fishcovariates, by = "UID")

#remove rows with incomplete data
test <- test[complete.cases(test),]

#select only the variables used in the model, calcuate weighted means of each varaible by species, filter for the species that the proportional abunance model converged for.
test <- test %>% 
  select(common_name, 
         propabun, 
         annual_mean_summer_temp,
         ElevCat,  
         logWsAreaSqKm) %>% 
  mutate(annual_mean_summer_temp_2 = annual_mean_summer_temp*propabun,
         ElevCat_2 =  ElevCat*propabun,  
         logWsAreaSqKm_2 = logWsAreaSqKm*propabun) %>% 
  group_by(common_name) %>% 
  summarise(sum_propabun = sum(propabun),
            sum_annual_mean_summer_temp_2 = sum(annual_mean_summer_temp_2),
            sum_ElevCat_2 =  sum(ElevCat_2),  
            sum_logWsAreaSqKm_2 = sum(logWsAreaSqKm_2)) %>% 
  ungroup() %>% 
  mutate(annual_mean_summer_temp_wm = sum_annual_mean_summer_temp_2/sum_propabun,
         ElevCat_wm =  sum_ElevCat_2/sum_propabun,  
         logWsAreaSqKm_wm = sum_logWsAreaSqKm_2/sum_propabun) %>% 
  filter(common_name %in% c("american eel", "white perch", "rosyside dace",  
                            "spotfin shiner", "mimic shiner", "finescale dace", "american brook lamprey", 
                            "eastern silvery minnow", "rosyface shiner", "cutlips minnow", "green sunfish", 
                            "brook stickleback", "common carp", "central mudminnow", "northern pike", "bridle shiner", 
                            "swamp darter", "lake chub", "banded killifish", "margined madtom", "burbot", "black crappie", 
                            "spottail shiner", "redbelly dace", "creek chubsucker", "banded sunfish", "fathead minnow", 
                            "bluntnose minnow", "redbreast sunfish", "rock bass", "rainbow trout", "longnose sucker", 
                            "smallmouth bass", "yellow bullhead", "redfin pickerel", "yellow perch", "brown bullhead", 
                            "atlantic salmon", "golden shiner", "chain pickerel", "bluegill", "brown trout", "tessellated darter", 
                            "largemouth bass", "fallfish", "common shiner", "creek chub", "pumpkinseed", "slimy sculpin", 
                            "longnose dace", "white sucker", "eastern blacknose dace", "brook trout")) %>% 
  data.frame()

#make the common names column into row names so we can cluster and keep only the final weighted mean values
test2 <- test[,-1]
rownames(test2) <- test[,1]
test2 <- test2[,5:7]

#scale the variables
cluster2 <- scale(test2)

tmp_cluster <- read.csv("tmpfigures/fishclusters_bytemperature.csv")


dat <- data.frame(cluster2) %>% 
  mutate(common_name = rownames(cluster2)) %>% 
  left_join(tmp_cluster, by = "common_name") 

rownames(dat) <- str_to_title(dat$common_name)
names(dat) <- c("Mean Summer Temp.", "Elev (Cat)", "Watershed Area", "Common Name", "X", "Cluster")

pca <- prcomp(dat[,1:3])


png(height = 10, width = 10, file = "tmpfigures/paper1/paper1rev/fish_cluster_biplot_temp.png", units = "in", res = 300, type = "cairo")


fviz_pca_biplot(pca,  habillage = dat$Cluster, repel = TRUE,
                palette =  "RdBu",
                title = "B",
                pointsize = 3, labelsize = 5,)+
  theme(axis.title  = element_text(size = 20),
        title = element_text(size = 30),
        legend.text = element_text(size=20),
        axis.text = element_text(size = 20))
dev.off()


b <- fviz_pca_biplot(pca,  habillage = dat$Cluster, repel = TRUE,
                     palette =  "RdBu",
                     title = "B",
                     pointsize = 3, labelsize = 5,)+
  theme(axis.title  = element_text(size = 20),
        title = element_text(size = 30),
        legend.text = element_text(size=20),
        axis.text = element_text(size = 20))


library(patchwork)
a/b
ggsave("tmpfigures/paper1/paper1rev/Figure1.png", width = 30, height = 40, dpi = 300, units= "cm")

