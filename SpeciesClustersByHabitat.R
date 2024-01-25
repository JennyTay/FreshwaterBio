#load libraries
library(tidyverse)
library(sf)
library(gridExtra)
library(factoextra)



#####  cluster spp based on the variable values in locations the spp occurrs
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
         BFI_HIST, 
         LO7Q1DT_HIST,
         W95_HIST, 
         BFIWs,  
         WtDepWs, 
         PctOw_Ws,  
         PctImp_Ws,  
         pctAg_Ws, 
         pctWetland_Cat, 
         logMJJA_HIST, 
         logRdCrsCat, 
         logPctOw_Cat,
         huc12_damden_sqkm,
         huc8_damcount) %>% 
  mutate(annual_mean_summer_temp_2 = annual_mean_summer_temp*propabun, 
         BFI_HIST_2 = BFI_HIST*propabun, 
         LO7Q1DT_HIST_2 = LO7Q1DT_HIST*propabun,
         W95_HIST_2 = W95_HIST*propabun, 
         BFIWs_2 =  BFIWs*propabun,  
         WtDepWs_2 = WtDepWs*propabun, 
         PctOw_Ws_2 = PctOw_Ws*propabun,  
         PctImp_Ws_2 = PctImp_Ws*propabun,  
         pctAg_Ws_2 = pctAg_Ws*propabun, 
         pctWetland_Cat_2 = pctWetland_Cat*propabun, 
         logMJJA_HIST_2 = logMJJA_HIST*propabun, 
         logRdCrsCat_2 = logRdCrsCat*propabun, 
         logPctOw_Cat_2 = logPctOw_Cat*propabun,
         huc12_damden_sqkm_2 = huc12_damden_sqkm*propabun,
         huc8_damcount_2 = huc8_damcount*propabun) %>% 
  group_by(common_name) %>% 
  summarise(sum_propabun = sum(propabun),
            sum_annual_mean_summer_temp_2 = sum(annual_mean_summer_temp_2), 
            sum_BFI_HIST_2 = sum(BFI_HIST_2), 
            sum_LO7Q1DT_HIST_2 = sum(LO7Q1DT_HIST_2),
            sum_W95_HIST_2 = sum(W95_HIST_2), 
            sum_BFIWs_2 =  sum(BFIWs_2),  
            sum_WtDepWs_2 = sum(WtDepWs_2), 
            sum_PctOw_Ws_2 = sum(PctOw_Ws_2),  
            sum_PctImp_Ws_2 = sum(PctImp_Ws_2),  
            sum_pctAg_Ws_2 = sum(pctAg_Ws_2), 
            sum_pctWetland_Cat_2 = sum(pctWetland_Cat_2), 
            sum_logMJJA_HIST_2 = sum(logMJJA_HIST_2), 
            sum_logRdCrsCat_2 = sum(logRdCrsCat_2), 
            sum_logPctOw_Cat_2 = sum(logPctOw_Cat_2),
            sum_huc12_damden_sqkm_2 = sum(huc12_damden_sqkm_2),
            sum_huc8_damcount_2 = sum(huc8_damcount_2)) %>% 
  ungroup() %>% 
  mutate(annual_mean_summer_temp_wm = sum_annual_mean_summer_temp_2/sum_propabun, 
         BFI_HIST_wm = sum_BFI_HIST_2/sum_propabun, 
         LO7Q1DT_HIST_wm = sum_LO7Q1DT_HIST_2/sum_propabun,
         W95_HIST_wm = sum_W95_HIST_2/sum_propabun, 
         BFIWs_wm =  sum_BFIWs_2/sum_propabun,  
         WtDepWs_wm = sum_WtDepWs_2/sum_propabun, 
         PctOw_Ws_wm = sum_PctOw_Ws_2/sum_propabun,  
         PctImp_Ws_wm = sum_PctImp_Ws_2/sum_propabun,  
         pctAg_Ws_wm = sum_pctAg_Ws_2/sum_propabun, 
         pctWetland_Cat_wm = sum_pctWetland_Cat_2/sum_propabun, 
         logMJJA_HIST_wm = sum_logMJJA_HIST_2/sum_propabun, 
         logRdCrsCat_wm = sum_logRdCrsCat_2/sum_propabun, 
         logPctOw_Cat_wm = sum_logPctOw_Cat_2/sum_propabun,
         huc12_damden_sqkm_wm = sum_huc12_damden_sqkm_2/sum_propabun,
         huc8_damcount_wm = sum_huc8_damcount_2/sum_propabun) %>% 
  filter(common_name %in% c("american eel", "white perch", "finescale dace", "american brook lamprey", 
                            "cutlips minnow", "green sunfish", "central mudminnow", "bridle shiner", 
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
test2 <- test2[,17:31]

#scale the variables
cluster2 <- scale(test2)

#make distance matrix using the factoexta package
#default distance computed is the Euclidean
distance <- get_dist(cluster2)

# visualize distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#compute k-means into 2-5 clusters
k2 <- kmeans(distance, centers = 2, nstart = 25)
k3 <- kmeans(distance, centers = 3, nstart = 25)
k4 <- kmeans(distance, centers = 4, nstart = 25)
k5 <- kmeans(distance, centers = 5, nstart = 25)
str(k2)
k2

#view clusters. Plotted along the two most influential compoents according to PCA
fviz_cluster(k4, data = distance)

#make plots of the clusters using 2-5 centroids
p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = distance) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


#determine optimal number of clusters
# function to compute total within-cluster sum of square 
# elbow method
# plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
set.seed(123)

wss <- function(k) {
  kmeans(distance, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



# Another method to determine best number of clusters (the highest point is the best)
# function to compute average silhouette for k clusters
library(cluster)
avg_sil <- function(k) {
  km.res <- kmeans(distance, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(distance))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#well use 5 as our final cluster number

set.seed(123)
final <- kmeans(distance, 5, nstart = 25)
print(final)
finalclust <- data.frame(final$cluster)
finalclust$common_name <- rownames(finalclust)
rownames(finalclust) <- NULL
finalclust <- finalclust %>% 
  arrange(final.cluster)
write.csv(finalclust, "tmpfigures/fishclusters.csv")

png(height = 15, width = 15, file = "tmpfigures/kmeanscluster.png", units = "in", res = 150, type = "cairo")

fviz_cluster(final, data = distance,
             pointsize = 3,
             labelsize = 15,
             main = NULL)+
  theme(legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

dev.off()

stats <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("mean") %>% 
  pivot_longer(cols = 2:16, names_to = "variable") %>% 
  mutate(mean = round(value, 2)) %>% 
  select(cluster, variable, mean) %>% 
  pivot_wider(names_from = variable, values_from = mean)
write.csv(stats, "tmpfigures/fishclusters_stats_mean.csv")
stats2 <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("sd") %>% 
  pivot_longer(cols = 2:16, names_to = "variable") %>% 
  mutate(sd = round(value, 2)) %>% 
  select(cluster, variable, sd)%>% 
  pivot_wider(names_from = variable, values_from = sd)
write.csv(stats2, "tmpfigures/fishclusters_stats_sd.csv")

stats <- test2 %>% 
  mutate(cluster = final$cluster)
names(stats) <- gsub(pattern = "_wm", replacement = "", x = names(stats))

for (i in 1:15) {
  ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
    geom_boxplot()+
    labs(y = names(stats)[i],
         x = "cluster")+
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  ggsave(
    filename = paste("tmpfigures/fish_cluster_stats/", names(stats)[i], ".png", sep = ""),
    plot = last_plot(),
    width = 11,
    height = 9,
    units = "cm",
    dpi = 150)
}










#####  cluster spp based on the Geographic locations in locations the spp occurrs
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

#select only the variables used in the model, calculate weighted means of each variable by species, filter for the species that the proportional abunance model converged for.
test <- test %>% 
  select(common_name, 
         propabun, 
         lat, 
         long, 
         ElevCat,
         logWsAreaSqKm) %>% 
  mutate(lat_2 = lat*propabun, 
         long_2 = long*propabun, 
         ElevCat_2 = ElevCat*propabun,
         logWsAreaSqKm_2 = logWsAreaSqKm*propabun) %>% 
  group_by(common_name) %>% 
  summarise(sum_propabun = sum(propabun),
            sum_lat_2 = sum(lat_2), 
            sum_long_2 = sum(long_2), 
            sum_ElevCat_2 = sum(ElevCat_2),
            sum_logWsAreaSqKm_2 = sum(logWsAreaSqKm_2)) %>% 
  ungroup() %>% 
  mutate(lat_wm = sum_lat_2/sum_propabun, 
         long_wm = sum_long_2/sum_propabun, 
         ElevCat_wm = sum_ElevCat_2/sum_propabun,
         logWsAreaSqKm_wm = sum_logWsAreaSqKm_2/sum_propabun) %>% 
  filter(common_name %in% c("american eel", "white perch", "finescale dace", "american brook lamprey", 
                            "cutlips minnow", "green sunfish", "central mudminnow", "bridle shiner", 
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
test2 <- test2[,6:9]

#scale the variables
cluster2 <- scale(test2)

#make distance matrix using the factoexta package
#default distance computed is the Euclidean
distance <- get_dist(cluster2)

# visualize distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#compute k-means into 2-5 clusters
k2 <- kmeans(distance, centers = 2, nstart = 25)
k3 <- kmeans(distance, centers = 3, nstart = 25)
k4 <- kmeans(distance, centers = 4, nstart = 25)
k5 <- kmeans(distance, centers = 5, nstart = 25)
str(k2)
k2

#view clusters. Plotted along the two most influential compoents according to PCA
fviz_cluster(k5, data = distance)

#make plots of the clusters using 2-5 centroids
p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = distance) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


#determine optimal number of clusters
# function to compute total within-cluster sum of square 
# elbow method
# plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
set.seed(123)

wss <- function(k) {
  kmeans(distance, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



# Another method to determine best number of clusters (the highest point is the best)
# function to compute average silhouette for k clusters
library(cluster)
avg_sil <- function(k) {
  km.res <- kmeans(distance, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(distance))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#well use 5 as our final cluster number

set.seed(123)
final <- kmeans(distance, 5, nstart = 25)
print(final)
finalclust <- data.frame(final$cluster)
finalclust$common_name <- rownames(finalclust)
rownames(finalclust) <- NULL
finalclust <- finalclust %>% 
  arrange(final.cluster)
write.csv(finalclust, "tmpfigures/fishclusters_bygeography.csv")

png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_bygeography.png", units = "in", res = 150, type = "cairo")

fviz_cluster(final, data = distance,
             pointsize = 3,
             labelsize = 25,
             main = NULL)+
  theme(legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

dev.off()

stats <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("mean") %>% 
  pivot_longer(cols = 2:5, names_to = "variable") %>% 
  mutate(mean = round(value, 2)) %>% 
  select(cluster, variable, mean) %>% 
  pivot_wider(names_from = variable, values_from = mean)
write.csv(stats, "tmpfigures/fishclusters_stats_mean_bygeography.csv")
stats2 <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("sd") %>% 
  pivot_longer(cols = 2:5, names_to = "variable") %>% 
  mutate(sd = round(value, 2)) %>% 
  select(cluster, variable, sd)%>% 
  pivot_wider(names_from = variable, values_from = sd)
write.csv(stats2, "tmpfigures/fishclusters_stats_sd_bygeography.csv")

stats <- test2 %>% 
  mutate(cluster = final$cluster)
names(stats) <- gsub(pattern = "_wm", replacement = "", x = names(stats))

for (i in 1:4) {
  ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
    geom_boxplot()+
    labs(y = names(stats)[i],
         x = "cluster")+
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  ggsave(
    filename = paste("tmpfigures/fish_cluster_stats/", "bygeography", names(stats)[i], ".png", sep = ""),
    plot = last_plot(),
    width = 11,
    height = 9,
    units = "cm",
    dpi = 150)
}













#####  cluster spp based on the variable values by climate change imapcted variables
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
         BFI_HIST, 
         LO7Q1DT_HIST,
         W95_HIST, 
         ElevCat,  
         logWsAreaSqKm, 
         logMJJA_HIST,
         CFM_HIST) %>% 
  mutate(annual_mean_summer_temp_2 = annual_mean_summer_temp*propabun, 
         BFI_HIST_2 = BFI_HIST*propabun, 
         LO7Q1DT_HIST_2 = LO7Q1DT_HIST*propabun,
         W95_HIST_2 = W95_HIST*propabun, 
         ElevCat_2 =  ElevCat*propabun,  
         logWsAreaSqKm_2 = logWsAreaSqKm*propabun, 
         CFM_HIST_2 = CFM_HIST*propabun, 
         logMJJA_HIST_2 = logMJJA_HIST*propabun) %>% 
  group_by(common_name) %>% 
  summarise(sum_propabun = sum(propabun),
            sum_annual_mean_summer_temp_2 = sum(annual_mean_summer_temp_2), 
            sum_BFI_HIST_2 = sum(BFI_HIST_2), 
            sum_LO7Q1DT_HIST_2 = sum(LO7Q1DT_HIST_2),
            sum_W95_HIST_2 = sum(W95_HIST_2), 
            sum_ElevCat_2 =  sum(ElevCat_2),  
            sum_logWsAreaSqKm_2 = sum(logWsAreaSqKm_2), 
            sum_CFM_HIST_2 = sum(CFM_HIST_2),
            sum_logMJJA_HIST_2 = sum(logMJJA_HIST_2)) %>% 
  ungroup() %>% 
  mutate(annual_mean_summer_temp_wm = sum_annual_mean_summer_temp_2/sum_propabun, 
         BFI_HIST_wm = sum_BFI_HIST_2/sum_propabun, 
         LO7Q1DT_HIST_wm = sum_LO7Q1DT_HIST_2/sum_propabun,
         W95_HIST_wm = sum_W95_HIST_2/sum_propabun, 
         ElevCat_wm =  sum_ElevCat_2/sum_propabun,  
         logWsAreaSqKm_wm = sum_logWsAreaSqKm_2/sum_propabun, 
         CFM_HIST_wm = sum_CFM_HIST_2/sum_propabun,
         logMJJA_HIST_wm = sum_logMJJA_HIST_2/sum_propabun) %>% 
  filter(common_name %in% c("american eel", "white perch", "finescale dace", "american brook lamprey", 
                            "cutlips minnow", "green sunfish", "central mudminnow", "bridle shiner", 
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
test2 <- test2[,10:17]

#scale the variables
cluster2 <- scale(test2)

#make distance matrix using the factoexta package
#default distance computed is the Euclidean
distance <- get_dist(cluster2)

# visualize distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#compute k-means into 2-5 clusters
k2 <- kmeans(distance, centers = 2, nstart = 25)
k3 <- kmeans(distance, centers = 3, nstart = 25)
k4 <- kmeans(distance, centers = 4, nstart = 25)
k5 <- kmeans(distance, centers = 5, nstart = 25)
k6 <- kmeans(distance, centers = 6, nstart = 25)
str(k2)
k2

#view clusters. Plotted along the two most influential compoents according to PCA
fviz_cluster(k5, data = distance)

#make plots of the clusters using 2-5 centroids
p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = distance) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


#determine optimal number of clusters
# function to compute total within-cluster sum of square 
# elbow method
# plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
set.seed(123)

wss <- function(k) {
  kmeans(distance, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



# Another method to determine best number of clusters (the highest point is the best)
# function to compute average silhouette for k clusters
library(cluster)
avg_sil <- function(k) {
  km.res <- kmeans(distance, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(distance))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#well use 6 as our final cluster number

set.seed(123)
final <- kmeans(distance, 6, nstart = 25)
print(final)
finalclust <- data.frame(final$cluster)
finalclust$common_name <- rownames(finalclust)
rownames(finalclust) <- NULL
finalclust <- finalclust %>% 
  arrange(final.cluster)
write.csv(finalclust, "tmpfigures/fishclusters_byclimate.csv")

png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_byclimate.png", units = "in", res = 150, type = "cairo")

fviz_cluster(final, data = distance,
             pointsize = 3,
             labelsize = 25,
             main = NULL)+
  theme(legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

dev.off()

stats <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("mean") %>% 
  pivot_longer(cols = 2:9, names_to = "variable") %>% 
  mutate(mean = round(value, 2)) %>% 
  select(cluster, variable, mean) %>% 
  pivot_wider(names_from = variable, values_from = mean)
write.csv(stats, "tmpfigures/fishclusters_stats_mean_byclimate.csv")
stats2 <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("sd") %>% 
  pivot_longer(cols = 2:9, names_to = "variable") %>% 
  mutate(sd = round(value, 2)) %>% 
  select(cluster, variable, sd)%>% 
  pivot_wider(names_from = variable, values_from = sd)
write.csv(stats2, "tmpfigures/fishclusters_stats_sd_byclimate.csv")

stats <- test2 %>% 
  mutate(cluster = final$cluster)
names(stats) <- gsub(pattern = "_wm", replacement = "", x = names(stats))

for (i in 1:8) {
  ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
    geom_boxplot()+
    labs(y = names(stats)[i],
         x = "cluster")+
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  ggsave(
    filename = paste("tmpfigures/fish_cluster_stats/", "byclimate", names(stats)[i], ".png", sep = ""),
    plot = last_plot(),
    width = 11,
    height = 9,
    units = "cm",
    dpi = 150)
}









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

#make distance matrix using the factoexta package
#default distance computed is the Euclidean
distance <- get_dist(cluster2)

# visualize distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#compute k-means into 2-5 clusters
k2 <- kmeans(distance, centers = 2, nstart = 25)
k3 <- kmeans(distance, centers = 3, nstart = 25)
k4 <- kmeans(distance, centers = 4, nstart = 25)
k5 <- kmeans(distance, centers = 5, nstart = 25)
k6 <- kmeans(distance, centers = 6, nstart = 25)
k7 <- kmeans(distance, centers = 7, nstart = 25)
k8 <- kmeans(distance, centers = 8, nstart = 25)
str(k2)
k2

#view clusters. Plotted along the two most influential compoents according to PCA
fviz_cluster(k6, data = distance)

#make plots of the clusters using 2-5 centroids
p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = distance) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = distance) + ggtitle("k = 6")


library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, nrow = 2)


#determine optimal number of clusters
# function to compute total within-cluster sum of square 
# elbow method
# plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
set.seed(123)

wss <- function(k) {
  kmeans(distance, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



# Another method to determine best number of clusters (the highest point is the best)
# function to compute average silhouette for k clusters
library(cluster)
avg_sil <- function(k) {
  km.res <- kmeans(distance, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(distance))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#well use 6 as our final cluster number

set.seed(123)
final <- kmeans(distance, 6, nstart = 25)
print(final)
finalclust <- data.frame(final$cluster)
finalclust$common_name <- rownames(finalclust)
rownames(finalclust) <- NULL
finalclust <- finalclust %>% 
  arrange(final.cluster)
write.csv(finalclust, "tmpfigures/fishclusters_bystreamflow.csv")

png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_bystreamflow.png", units = "in", res = 150, type = "cairo")

fviz_cluster(final, data = distance,
             pointsize = 3,
             labelsize = 25,
             main = NULL)+
  theme(legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

dev.off()

stats <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("mean") %>% 
  pivot_longer(cols = 2:8, names_to = "variable") %>% 
  mutate(mean = round(value, 2)) %>% 
  select(cluster, variable, mean) %>% 
  pivot_wider(names_from = variable, values_from = mean)
write.csv(stats, "tmpfigures/fishclusters_stats_mean_bystreamflow.csv")
stats2 <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("sd") %>% 
  pivot_longer(cols = 2:8, names_to = "variable") %>% 
  mutate(sd = round(value, 2)) %>% 
  select(cluster, variable, sd)%>% 
  pivot_wider(names_from = variable, values_from = sd)
write.csv(stats2, "tmpfigures/fishclusters_stats_sd_bystreamflow.csv")

stats <- test2 %>% 
  mutate(cluster = final$cluster)
names(stats) <- gsub(pattern = "_wm", replacement = "", x = names(stats))

for (i in 1:7) {
  ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
    geom_boxplot()+
    labs(y = names(stats)[i],
         x = "cluster")+
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  ggsave(
    filename = paste("tmpfigures/fish_cluster_stats/", "bystreamflow", names(stats)[i], ".png", sep = ""),
    plot = last_plot(),
    width = 11,
    height = 9,
    units = "cm",
    dpi = 150)
}









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

#make distance matrix using the factoexta package
#default distance computed is the Euclidean
distance <- get_dist(cluster2)

# visualize distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#compute k-means into 2-5 clusters
k2 <- kmeans(distance, centers = 2, nstart = 25)
k3 <- kmeans(distance, centers = 3, nstart = 25)
k4 <- kmeans(distance, centers = 4, nstart = 25)
k5 <- kmeans(distance, centers = 5, nstart = 25)
k6 <- kmeans(distance, centers = 6, nstart = 25)
str(k2)
k2

#view clusters. Plotted along the two most influential compoents according to PCA
fviz_cluster(k6, data = distance)

#make plots of the clusters using 2-5 centroids
p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = distance) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


#determine optimal number of clusters
# function to compute total within-cluster sum of square 
# elbow method
# plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
set.seed(123)

wss <- function(k) {
  kmeans(distance, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



# Another method to determine best number of clusters (the highest point is the best)
# function to compute average silhouette for k clusters
library(cluster)
avg_sil <- function(k) {
  km.res <- kmeans(distance, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(distance))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#well use 6 as our final cluster number

set.seed(123)
final <- kmeans(distance, 6, nstart = 25)
print(final)
finalclust <- data.frame(final$cluster)
finalclust$common_name <- rownames(finalclust)
rownames(finalclust) <- NULL
finalclust <- finalclust %>% 
  arrange(final.cluster)
write.csv(finalclust, "tmpfigures/fishclusters_bytemperature.csv")

png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_bytemperature.png", units = "in", res = 150, type = "cairo")

fviz_cluster(final, data = distance,
             pointsize = 3,
             labelsize = 25,
             main = NULL)+
  theme(legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

dev.off()

stats <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("mean") %>% 
  pivot_longer(cols = 2:4, names_to = "variable") %>% 
  mutate(mean = round(value, 2)) %>% 
  select(cluster, variable, mean) %>% 
  pivot_wider(names_from = variable, values_from = mean)
write.csv(stats, "tmpfigures/fishclusters_stats_mean_bytemperature.csv")
stats2 <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("sd") %>% 
  pivot_longer(cols = 2:4, names_to = "variable") %>% 
  mutate(sd = round(value, 2)) %>% 
  select(cluster, variable, sd)%>% 
  pivot_wider(names_from = variable, values_from = sd)
write.csv(stats2, "tmpfigures/fishclusters_stats_sd_bytemperature.csv")

stats <- test2 %>% 
  mutate(cluster = final$cluster)
names(stats) <- gsub(pattern = "_wm", replacement = "", x = names(stats))

for (i in 1:3) {
  ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
    geom_boxplot()+
    labs(y = names(stats)[i],
         x = "cluster")+
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  ggsave(
    filename = paste("tmpfigures/fish_cluster_stats/", "bytemperature", names(stats)[i], ".png", sep = ""),
    plot = last_plot(),
    width = 11,
    height = 9,
    units = "cm",
    dpi = 150)
}












#####  cluster spp based on the storm flow variable values 
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
         W95_HIST) %>% 
  mutate(W95_HIST_2 = W95_HIST*propabun) %>% 
  group_by(common_name) %>% 
  summarise(sum_propabun = sum(propabun),
            sum_W95_HIST_2 = sum(W95_HIST_2)) %>% 
  ungroup() %>% 
  mutate(W95_HIST_wm = sum_W95_HIST_2/sum_propabun) %>% 
  filter(common_name %in% c("american eel", "white perch", "finescale dace", "american brook lamprey", 
                            "cutlips minnow", "green sunfish", "central mudminnow", "bridle shiner", 
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
test2 <- test2 %>% select(W95_HIST_wm)

#scale the variables
cluster2 <- scale(test2)

#make distance matrix using the factoexta package
#default distance computed is the Euclidean
distance <- get_dist(cluster2)

# visualize distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#compute k-means into 2-5 clusters
k2 <- kmeans(distance, centers = 2, nstart = 25)
k3 <- kmeans(distance, centers = 3, nstart = 25)
k4 <- kmeans(distance, centers = 4, nstart = 25)
k5 <- kmeans(distance, centers = 5, nstart = 25)
str(k2)
k2

#view clusters. Plotted along the two most influential compoents according to PCA
fviz_cluster(k4, data = distance)

#make plots of the clusters using 2-5 centroids
p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = distance) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


#determine optimal number of clusters
#(the highest point is the best)
# function to compute average silhouette for k clusters
library(cluster)
avg_sil <- function(k) {
  km.res <- kmeans(distance, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(distance))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#well use 3 as our final cluster number

set.seed(123)
final <- kmeans(distance, 3, nstart = 25)
print(final)
finalclust <- data.frame(final$cluster)
finalclust$common_name <- rownames(finalclust)
rownames(finalclust) <- NULL
finalclust <- finalclust %>% 
  arrange(final.cluster)
write.csv(finalclust, "tmpfigures/fishclusters_stormflow.csv")

png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_stormflow.png", units = "in", res = 150, type = "cairo")

fviz_cluster(final, data = distance,
             pointsize = 3,
             labelsize = 25,
             main = NULL)+
  theme(legend.key.height= unit(2, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

dev.off()

stats <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("mean") 
write.csv(stats, "tmpfigures/fishclusters_stats_mean_stormflow.csv")
stats2 <- test2 %>% 
  mutate(cluster = final$cluster) %>% 
  group_by(cluster) %>% 
  summarise_all("sd") 
write.csv(stats2, "tmpfigures/fishclusters_stats_sd_stormflow.csv")

stats <- test2 %>% 
  mutate(cluster = final$cluster)

  ggplot(data = stats, aes(x = as.factor(cluster), y = W95_HIST_wm ))+
    geom_boxplot()+
    labs(y = "Storm Flows (#)",
         x = "cluster")+
    theme(panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  ggsave(
    filename = "tmpfigures/fish_cluster_stats/bystormflow.png",
    plot = last_plot(),
    width = 11,
    height = 9,
    units = "cm",
    dpi = 150)


  
  
 
  
  
  #####  cluster spp based on the COEFFCIENT values by climate change imapcted variables from the proportional abundance model
  ##### then we are using K-means clusting, following a tutorial from this website https://uc-r.github.io/kmeans_clustering
  
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/model_covariates.RData")
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/fish_count_with_zeros.RData")
  prop_abun_results <- read.csv("tmpfigures/propabun_results.csv")
  
  
  #we are filtering out the really high odds ratios and we are filtering out brook stickleback because its so unique that it makes everything else cluster together
  cluster <- results2 %>% 
    select(variable, common_name, OR) %>% 
    pivot_wider(names_from = variable, values_from = OR) %>% 
    data.frame() %>% 
    select(common_name, "mean.summer.temp", "baseflow.index", "low.flow.date", "number.of.winter.floods", "summer.flow",
           "elevation", "watershed.area") %>% 
    filter(`mean.summer.temp` < 100, 
           `baseflow.index` < 100, 
           `low.flow.date` < 100, 
           `number.of.winter.floods` < 100, 
           `summer.flow` < 100,
           `elevation` < 100, 
           `watershed.area` < 100,
           common_name != "brook stickleback")
  
  cluster2 <- cluster[,-1]
  rownames(cluster2) <- cluster[,1]
  cluster2 <- cluster2[complete.cases(cluster2),]
  
  
  cluster2 <- scale(cluster2)
  
  #make distance matrix using the factoexta package
  #default distance computed is the Euclidean
  distance <- get_dist(cluster2)
  
  # visualize distance matrix
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
  
  #compute k-means into 2-5 clusters
  k2 <- kmeans(distance, centers = 2, nstart = 25)
  k3 <- kmeans(distance, centers = 3, nstart = 25)
  k4 <- kmeans(distance, centers = 4, nstart = 25)
  k5 <- kmeans(distance, centers = 5, nstart = 25)
  str(k2)
  k2
  
  #view clusters. Plotted along the two most influential compoents according to PCA
  fviz_cluster(k4, data = distance)
  
  #make plots of the clusters using 2-5 centroids
  p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
  p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
  p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
  p4 <- fviz_cluster(k5, geom = "point",  data = distance) + ggtitle("k = 5")
  
  library(gridExtra)
  grid.arrange(p1, p2, p3, p4, nrow = 2)
  
  
  #determine optimal number of clusters
  # function to compute total within-cluster sum of square 
  # elbow method
  # plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
  set.seed(123)
  
  wss <- function(k) {
    kmeans(distance, k, nstart = 10 )$tot.withinss
  }
  
  # Compute and plot wss for k = 1 to k = 15
  k.values <- 1:15
  
  # extract wss for 2-15 clusters
  wss_values <- map_dbl(k.values, wss)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  
  
  
  # Another method to determine best number of clusters (the highest point is the best)
  # function to compute average silhouette for k clusters
  library(cluster)
  avg_sil <- function(k) {
    km.res <- kmeans(distance, centers = k, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(distance))
    mean(ss[, 3])
  }
  
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:15
  
  # extract avg silhouette for 2-15 clusters
  avg_sil_values <- map_dbl(k.values, avg_sil)
  
  plot(k.values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")
  
  
  #well use 3 as our final cluster number
  
  set.seed(123)
  final <- kmeans(distance, 3, nstart = 25)
  print(final)
  finalclust <- data.frame(final$cluster)
  finalclust$common_name <- rownames(finalclust)
  rownames(finalclust) <- NULL
  finalclust <- finalclust %>% 
    arrange(final.cluster)
  write.csv(finalclust, "tmpfigures/fishclusters_byPAcoef.csv")
  
  png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_byPAcoef.png", units = "in", res = 150, type = "cairo")
  
  fviz_cluster(final, data = distance,
               pointsize = 3,
               labelsize = 25,
               main = NULL)+
    theme(legend.key.height= unit(2, 'cm'),
          legend.key.width= unit(2, 'cm'),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          axis.text = element_text(size = 30),
          axis.title = element_text(size = 30),
          panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  dev.off()
  
  stats <- test2 %>% 
    mutate(cluster = final$cluster) %>% 
    group_by(cluster) %>% 
    summarise_all("mean") %>% 
    pivot_longer(cols = 2:9, names_to = "variable") %>% 
    mutate(mean = round(value, 2)) %>% 
    select(cluster, variable, mean) %>% 
    pivot_wider(names_from = variable, values_from = mean)
  write.csv(stats, "tmpfigures/fishclusters_stats_mean_byPAcoef.csv")
  stats2 <- test2 %>% 
    mutate(cluster = final$cluster) %>% 
    group_by(cluster) %>% 
    summarise_all("sd") %>% 
    pivot_longer(cols = 2:9, names_to = "variable") %>% 
    mutate(sd = round(value, 2)) %>% 
    select(cluster, variable, sd)%>% 
    pivot_wider(names_from = variable, values_from = sd)
  write.csv(stats2, "tmpfigures/fishclusters_stats_sd_byPAcoef.csv")
  
  stats <- test2 %>% 
    mutate(cluster = final$cluster)
  names(stats) <- gsub(pattern = "_wm", replacement = "", x = names(stats))
  
  for (i in 1:8) {
    ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
      geom_boxplot()+
      labs(y = names(stats)[i],
           x = "cluster")+
      theme(panel.border = element_rect(colour = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    
    ggsave(
      filename = paste("tmpfigures/fish_cluster_stats/", "byPAcoef", names(stats)[i], ".png", sep = ""),
      plot = last_plot(),
      width = 11,
      height = 9,
      units = "cm",
      dpi = 150)
  }
  

  
  
  
  
  
  ##############################################################################
  
  ##############################################################################
  
  ########################## Cluster Mussel Species ############################
  
  ##############################################################################
  
  ##############################################################################
  
  
  
  
  
  
  
  
  
  #####  cluster mussel spp based on the variable values by streamflow variables
  ##### in this code we are using weighted means for each species.
  ##### for each species, we calucated the weighted mean of each variable (weighted by the proportional abundance at a survey site)
  ##### then we are using K-means clusting, following a tutorial from this website https://uc-r.github.io/kmeans_clustering
  
  
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_occurrence.RData")
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_covariates_byhuc12.RData")
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_event_huc_join.RData")
  
  #join mussel_occurrence data to the mussel event huc join so that we can get presence and absence by huc10
  occ <- left_join(mussel_occurrence, mussel_event_huc_join, by = "UID")
  #there are tons of rows with NA for all the HUC information - this is because we removed all the lentic surveys from the mussel_event_huc_join data file, but those are still
  #present in the mussel_occurrence data file.
  occ <- occ %>% 
    data.frame() %>% 
    filter(!is.na(huc12_name)) %>% 
    group_by(huc12_name, huc12_tnmid, common_name) %>% 
    summarise(live_occurrence = max(live_occurrence, na.rm = T),
              shell_occurrence = max(shell_occurrence, na.rm = T))
  
   #join occurrence data by huc12 to the covariate dataset for the model
  #first need to remove all repeated rows of huc12 covariates
  mussel_covariates <- mussel_covariates_huc12 %>% 
    data.frame() %>% 
    select(-c(UID, state, date, project, source, waterbody, waterbody2,
              huc8_tnmid, huc8_areasqkm,
              huc10_tnmid, huc10_areasqkm,huc10_name,
              geometry)) %>% 
    unique()
  
  
  
  test <- left_join(occ, mussel_covariates, by = c("huc12_name", "huc12_tnmid"))
  
  #remove rows with incomplete data
  test <- test[complete.cases(test),]
  
  #select only the variables used in the model, calcuate weighted means of each varaible by species, filter for the species that the proportional abunance model converged for.
  test <- test %>% 
    ungroup() %>% 
    select(common_name, 
           live_occurrence, 
           BFI_HIST, 
           LO7Q1DT_HIST,
           W95_HIST, 
           ElevCat,  
           logWsAreaSqKm,) %>%
    filter(common_name %in% c("alewife floater", "brook floater", "creeper",  "dwarf wedgemussel" ,  
                              "eastern elliptio", "eastern floater", "eastern lampmussel", "eastern pearlshell",    
                              "eastern pondmussel", "tidewater mucket", "triangle floater", "yellow lampmussel"),
           live_occurrence == 1) %>% 
    select(-live_occurrence) %>% 
    group_by(common_name) %>% 
    summarise(BFI_HIST = mean(BFI_HIST), 
              LO7Q1DT_HIST = mean(LO7Q1DT_HIST),
              W95_HIST = mean(W95_HIST), 
              ElevCat = mean(ElevCat),  
              logWsAreaSqKm = mean(logWsAreaSqKm)) %>% 
    data.frame()
  
  #make the common names column into row names so we can cluster and keep only the final weighted mean values
  test2 <- test[,-1]
  rownames(test2) <- test[,1]
 
  
  #scale the variables
  cluster2 <- scale(test2)
  
  #make distance matrix using the factoexta package
  #default distance computed is the Euclidean
  distance <- get_dist(cluster2)
  
  # visualize distance matrix
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
  
  #compute k-means into 2-5 clusters
  k2 <- kmeans(distance, centers = 2, nstart = 25)
  k3 <- kmeans(distance, centers = 3, nstart = 25)
  k4 <- kmeans(distance, centers = 4, nstart = 25)
  str(k2)
  k2
  
  #view clusters. Plotted along the two most influential compoents according to PCA
  fviz_cluster(k4, data = distance)
  
  #make plots of the clusters using 2-5 centroids
  p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
  p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
  p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
  
  
  library(gridExtra)
  grid.arrange(p1, p2, p3, nrow = 2)
  
  
  #determine optimal number of clusters
  # function to compute total within-cluster sum of square 
  # elbow method
  # plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
  set.seed(123)
  
  wss <- function(k) {
    kmeans(distance, k, nstart = 10 )$tot.withinss
  }
  
  # Compute and plot wss for k = 1 to k = 8
  k.values <- 1:8
  
  # extract wss for 2-8 clusters
  wss_values <- map_dbl(k.values, wss)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  
  
  
  # Another method to determine best number of clusters (the highest point is the best)
  # function to compute average silhouette for k clusters
  library(cluster)
  avg_sil <- function(k) {
    km.res <- kmeans(distance, centers = k, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(distance))
    mean(ss[, 3])
  }
  
  # Compute and plot wss for k = 2 to k = 8
  k.values <- 2:8
  
  # extract avg silhouette for 2-8 clusters
  avg_sil_values <- map_dbl(k.values, avg_sil)
  
  plot(k.values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")
  
  
  #well use 3 as our final cluster number
  
  set.seed(123)
  final <- kmeans(distance, 3, nstart = 25)
  print(final)
  finalclust <- data.frame(final$cluster)
  finalclust$common_name <- rownames(finalclust)
  rownames(finalclust) <- NULL
  finalclust <- finalclust %>% 
    arrange(final.cluster)
  write.csv(finalclust, "tmpfigures/musselclusters_bystreamflow.csv")
  
  png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_bystreamflow_mussel.png", units = "in", res = 150, type = "cairo")
  
  fviz_cluster(final, data = distance,
               pointsize = 3,
               labelsize = 25,
               main = NULL)+
    theme(legend.key.height= unit(2, 'cm'),
          legend.key.width= unit(2, 'cm'),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          axis.text = element_text(size = 30),
          axis.title = element_text(size = 30),
          panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  dev.off()
  
  stats <- test2 %>% 
    mutate(cluster = final$cluster) %>% 
    group_by(cluster) %>% 
    summarise_all("mean") %>% 
    pivot_longer(cols = 2:6, names_to = "variable") %>% 
    mutate(mean = round(value, 2)) %>% 
    select(cluster, variable, mean) %>% 
    pivot_wider(names_from = variable, values_from = mean)
  write.csv(stats, "tmpfigures/musselclusters_stats_mean_bystreamflow.csv")
  stats2 <- test2 %>% 
    mutate(cluster = final$cluster) %>% 
    group_by(cluster) %>% 
    summarise_all("sd") %>% 
    pivot_longer(cols = 2:6, names_to = "variable") %>% 
    mutate(sd = round(value, 2)) %>% 
    select(cluster, variable, sd)%>% 
    pivot_wider(names_from = variable, values_from = sd)
  write.csv(stats2, "tmpfigures/musselclusters_stats_sd_bystreamflow.csv")
  
  stats <- test2 %>% 
    mutate(cluster = final$cluster)

  
  for (i in 1:5) {
    ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
      geom_boxplot()+
      geom_jitter(color = "red", width = .2)+
      labs(y = names(stats)[i],
           x = "cluster")+
      theme(panel.border = element_rect(colour = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    
    ggsave(
      filename = paste("tmpfigures/mussel_cluster_stats/", "bystreamflow", names(stats)[i], ".png", sep = ""),
      plot = last_plot(),
      width = 11,
      height = 9,
      units = "cm",
      dpi = 150)
  }
  
  
  
  
  
  #####  cluster mussel spp based on the variable values by stream temperature
  ##### in this code we are using weighted means for each species.
  ##### for each species, we calucated the weighted mean of each variable (weighted by the proportional abundance at a survey site)
  ##### then we are using K-means clusting, following a tutorial from this website https://uc-r.github.io/kmeans_clustering
  
  
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_occurrence.RData")
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_covariates_byhuc12.RData")
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_event_huc_join.RData")
  
  #join mussel_occurrence data to the mussel event huc join so that we can get presence and absence by huc10
  occ <- left_join(mussel_occurrence, mussel_event_huc_join, by = "UID")
  #there are tons of rows with NA for all the HUC information - this is because we removed all the lentic surveys from the mussel_event_huc_join data file, but those are still
  #present in the mussel_occurrence data file.
  occ <- occ %>% 
    data.frame() %>% 
    filter(!is.na(huc12_name)) %>% 
    group_by(huc12_name, huc12_tnmid, common_name) %>% 
    summarise(live_occurrence = max(live_occurrence, na.rm = T),
              shell_occurrence = max(shell_occurrence, na.rm = T))
  
  #join occurrence data by huc12 to the covariate dataset for the model
  #first need to remove all repeated rows of huc12 covariates
  mussel_covariates <- mussel_covariates_huc12 %>% 
    data.frame() %>% 
    select(-c(UID, state, date, project, source, waterbody, waterbody2,
              huc8_tnmid, huc8_areasqkm,
              huc10_tnmid, huc10_areasqkm,huc10_name,
              geometry)) %>% 
    unique()
  
  
  
  test <- left_join(occ, mussel_covariates, by = c("huc12_name", "huc12_tnmid"))
  
  #remove rows with incomplete data
  test <- test[complete.cases(test),]
  
  #select only the variables used in the model, calcuate weighted means of each varaible by species, filter for the species that the proportional abunance model converged for.
  test <- test %>% 
    ungroup() %>% 
    select(common_name, 
           live_occurrence, 
           annual_mean_summer_temp, 
           ElevCat,  
           logWsAreaSqKm) %>%
    filter(common_name %in% c("alewife floater", "brook floater", "creeper",  "dwarf wedgemussel" ,  
                              "eastern elliptio", "eastern floater", "eastern lampmussel", "eastern pearlshell",    
                              "eastern pondmussel", "tidewater mucket", "triangle floater", "yellow lampmussel"),
           live_occurrence == 1) %>% 
    select(-live_occurrence) %>% 
    group_by(common_name) %>% 
    summarise(annual_mean_summer_temp = median(annual_mean_summer_temp), 
              ElevCat = median(ElevCat),  
              logWsAreaSqKm = median(logWsAreaSqKm)) %>% 
    data.frame()
  
  #make the common names column into row names so we can cluster and keep only the final weighted mean values
  test2 <- test[,-1]
  rownames(test2) <- test[,1]
  
  
  #scale the variables
  cluster2 <- scale(test2)
  
  #make distance matrix using the factoexta package
  #default distance computed is the Euclidean
  distance <- get_dist(cluster2)
  
  # visualize distance matrix
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
  
  #compute k-means into 2-5 clusters
  k2 <- kmeans(distance, centers = 2, nstart = 25)
  k3 <- kmeans(distance, centers = 3, nstart = 25)
  k4 <- kmeans(distance, centers = 4, nstart = 25)
  str(k2)
  k2
  
  #view clusters. Plotted along the two most influential compoents according to PCA
  fviz_cluster(k4, data = distance)
  
  #make plots of the clusters using 2-5 centroids
  p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
  p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
  p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
  
  
  library(gridExtra)
  grid.arrange(p1, p2, p3, nrow = 2)
  
  
  #determine optimal number of clusters
  # function to compute total within-cluster sum of square 
  # elbow method
  # plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
  set.seed(123)
  
  wss <- function(k) {
    kmeans(distance, k, nstart = 10 )$tot.withinss
  }
  
  # Compute and plot wss for k = 1 to k = 8
  k.values <- 1:8
  
  # extract wss for 2-8 clusters
  wss_values <- map_dbl(k.values, wss)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  
  
  
  # Another method to determine best number of clusters (the highest point is the best)
  # function to compute average silhouette for k clusters
  library(cluster)
  avg_sil <- function(k) {
    km.res <- kmeans(distance, centers = k, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(distance))
    mean(ss[, 3])
  }
  
  # Compute and plot wss for k = 2 to k = 8
  k.values <- 2:8
  
  # extract avg silhouette for 2-8 clusters
  avg_sil_values <- map_dbl(k.values, avg_sil)
  
  plot(k.values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")
  
  
  #well use 3 as our final cluster number
  
  set.seed(123)
  final <- kmeans(distance, 3, nstart = 25)
  print(final)
  finalclust <- data.frame(final$cluster)
  finalclust$common_name <- rownames(finalclust)
  rownames(finalclust) <- NULL
  finalclust <- finalclust %>% 
    arrange(final.cluster)
  write.csv(finalclust, "tmpfigures/musselclusters_bytemperature.csv")
  
  png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_bytemperature_mussel.png", units = "in", res = 150, type = "cairo")
  
  fviz_cluster(final, data = distance,
               pointsize = 3,
               labelsize = 25,
               main = NULL)+
    theme(legend.key.height= unit(2, 'cm'),
          legend.key.width= unit(2, 'cm'),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          axis.text = element_text(size = 30),
          axis.title = element_text(size = 30),
          panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  dev.off()
  
  stats <- test2 %>% 
    mutate(cluster = final$cluster) %>% 
    group_by(cluster) %>% 
    summarise_all("mean") %>% 
    pivot_longer(cols = 2:4, names_to = "variable") %>% 
    mutate(mean = round(value, 2)) %>% 
    select(cluster, variable, mean) %>% 
    pivot_wider(names_from = variable, values_from = mean)
  write.csv(stats, "tmpfigures/musselclusters_stats_mean_bytemperature.csv")
  stats2 <- test2 %>% 
    mutate(cluster = final$cluster) %>% 
    group_by(cluster) %>% 
    summarise_all("sd") %>% 
    pivot_longer(cols = 2:4, names_to = "variable") %>% 
    mutate(sd = round(value, 2)) %>% 
    select(cluster, variable, sd)%>% 
    pivot_wider(names_from = variable, values_from = sd)
  write.csv(stats2, "tmpfigures/musselclusters_stats_sd_bytemperature.csv")
  
  stats <- test2 %>% 
    mutate(cluster = final$cluster)
  
  
  for (i in 1:3) {
    ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
      geom_boxplot()+
      labs(y = names(stats)[i],
           x = "cluster")+
      theme(panel.border = element_rect(colour = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    
    ggsave(
      filename = paste("tmpfigures/mussel_cluster_stats/", "bytemperature", names(stats)[i], ".png", sep = ""),
      plot = last_plot(),
      width = 11,
      height = 9,
      units = "cm",
      dpi = 150)
  }
  
  
  
  
  
  #####  cluster mussel spp based on the variable values by streamflow and stream temperature variables
  ##### in this code we are using weighted means for each species.
  ##### for each species, we calucated the weighted mean of each variable (weighted by the proportional abundance at a survey site)
  ##### then we are using K-means clusting, following a tutorial from this website https://uc-r.github.io/kmeans_clustering
  
  
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_occurrence.RData")
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_covariates_byhuc12.RData")
  load("C:/Users/jenrogers/Documents/necascFreshwaterBio/model_datafiles/mussel_event_huc_join.RData")
  
  #join mussel_occurrence data to the mussel event huc join so that we can get presence and absence by huc10
  occ <- left_join(mussel_occurrence, mussel_event_huc_join, by = "UID")
  #there are tons of rows with NA for all the HUC information - this is because we removed all the lentic surveys from the mussel_event_huc_join data file, but those are still
  #present in the mussel_occurrence data file.
  occ <- occ %>% 
    data.frame() %>% 
    filter(!is.na(huc12_name)) %>% 
    group_by(huc12_name, huc12_tnmid, common_name) %>% 
    summarise(live_occurrence = max(live_occurrence, na.rm = T),
              shell_occurrence = max(shell_occurrence, na.rm = T))
  
  #join occurrence data by huc12 to the covariate dataset for the model
  #first need to remove all repeated rows of huc12 covariates
  mussel_covariates <- mussel_covariates_huc12 %>% 
    data.frame() %>% 
    select(-c(UID, state, date, project, source, waterbody, waterbody2,
              huc8_tnmid, huc8_areasqkm,
              huc10_tnmid, huc10_areasqkm,huc10_name,
              geometry)) %>% 
    unique()
  
  
  
  test <- left_join(occ, mussel_covariates, by = c("huc12_name", "huc12_tnmid"))
  
  #remove rows with incomplete data
  test <- test[complete.cases(test),]
  
  #select only the variables used in the model, calcuate weighted means of each varaible by species, filter for the species that the proportional abunance model converged for.
  test <- test %>% 
    ungroup() %>% 
    select(common_name, 
           live_occurrence, 
           BFI_HIST, 
           LO7Q1DT_HIST,
           W95_HIST, 
           ElevCat,
           annual_mean_summer_temp,
           logWsAreaSqKm,) %>%
    filter(common_name %in% c("alewife floater", "brook floater", "creeper",  "dwarf wedgemussel" ,  
                              "eastern elliptio", "eastern floater", "eastern lampmussel", "eastern pearlshell",    
                              "eastern pondmussel", "tidewater mucket", "triangle floater", "yellow lampmussel"),
           live_occurrence == 1) %>% 
    select(-live_occurrence) %>% 
    group_by(common_name) %>% 
    summarise(BFI_HIST = mean(BFI_HIST), 
              LO7Q1DT_HIST = mean(LO7Q1DT_HIST),
              W95_HIST = mean(W95_HIST), 
              ElevCat = mean(ElevCat),
              annual_mean_summer_temp = mean(annual_mean_summer_temp),
              logWsAreaSqKm = mean(logWsAreaSqKm)) %>% 
    data.frame()
  
  #make the common names column into row names so we can cluster and keep only the final weighted mean values
  test2 <- test[,-1]
  rownames(test2) <- test[,1]
  
  
  #scale the variables
  cluster2 <- scale(test2)
  
  #make distance matrix using the factoexta package
  #default distance computed is the Euclidean
  distance <- get_dist(cluster2)
  
  # visualize distance matrix
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
  
  #compute k-means into 2-5 clusters
  k2 <- kmeans(distance, centers = 2, nstart = 25)
  k3 <- kmeans(distance, centers = 3, nstart = 25)
  k4 <- kmeans(distance, centers = 4, nstart = 25)
  str(k2)
  k2
  
  #view clusters. Plotted along the two most influential compoents according to PCA
  fviz_cluster(k4, data = distance)
  
  #make plots of the clusters using 2-5 centroids
  p1 <- fviz_cluster(k2, geom = "point", data = distance) + ggtitle("k = 2")
  p2 <- fviz_cluster(k3, geom = "point",  data = distance) + ggtitle("k = 3")
  p3 <- fviz_cluster(k4, geom = "point",  data = distance) + ggtitle("k = 4")
  
  
  library(gridExtra)
  grid.arrange(p1, p2, p3, nrow = 2)
  
  
  #determine optimal number of clusters
  # function to compute total within-cluster sum of square 
  # elbow method
  # plot the within cluster variation, when it stops dropping much (the elbow) after adding an adidtional cluster than you can use that numbmer
  set.seed(123)
  
  wss <- function(k) {
    kmeans(distance, k, nstart = 10 )$tot.withinss
  }
  
  # Compute and plot wss for k = 1 to k = 8
  k.values <- 1:8
  
  # extract wss for 2-8 clusters
  wss_values <- map_dbl(k.values, wss)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  
  
  
  # Another method to determine best number of clusters (the highest point is the best)
  # function to compute average silhouette for k clusters
  library(cluster)
  avg_sil <- function(k) {
    km.res <- kmeans(distance, centers = k, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(distance))
    mean(ss[, 3])
  }
  
  # Compute and plot wss for k = 2 to k = 8
  k.values <- 2:8
  
  # extract avg silhouette for 2-8 clusters
  avg_sil_values <- map_dbl(k.values, avg_sil)
  
  plot(k.values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")
  
  
  #well use 3 as our final cluster number
  
  set.seed(123)
  final <- kmeans(distance, 3, nstart = 25)
  print(final)
  finalclust <- data.frame(final$cluster)
  finalclust$common_name <- rownames(finalclust)
  rownames(finalclust) <- NULL
  finalclust <- finalclust %>% 
    arrange(final.cluster)
  write.csv(finalclust, "tmpfigures/musselclusters_bystreamflowAndTemp.csv")
  
  png(height = 15, width = 15, file = "tmpfigures/kmeanscluster_bystreamflowAndTemp_mussel.png", units = "in", res = 150, type = "cairo")
  
  fviz_cluster(final, data = distance,
               pointsize = 3,
               labelsize = 25,
               main = NULL)+
    theme(legend.key.height= unit(2, 'cm'),
          legend.key.width= unit(2, 'cm'),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          axis.text = element_text(size = 30),
          axis.title = element_text(size = 30),
          panel.border = element_rect(colour = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  dev.off()
  
  stats <- test2 %>% 
    mutate(cluster = final$cluster) %>% 
    group_by(cluster) %>% 
    summarise_all("mean") %>% 
    pivot_longer(cols = 2:7, names_to = "variable") %>% 
    mutate(mean = round(value, 2)) %>% 
    select(cluster, variable, mean) %>% 
    pivot_wider(names_from = variable, values_from = mean)
  write.csv(stats, "tmpfigures/musselclusters_stats_mean_bystreamflowAndTemp.csv")
  stats2 <- test2 %>% 
    mutate(cluster = final$cluster) %>% 
    group_by(cluster) %>% 
    summarise_all("sd") %>% 
    pivot_longer(cols = 2:7, names_to = "variable") %>% 
    mutate(sd = round(value, 2)) %>% 
    select(cluster, variable, sd)%>% 
    pivot_wider(names_from = variable, values_from = sd)
  write.csv(stats2, "tmpfigures/musselclusters_stats_sd_bystreamflowAndTemp.csv")
  
  stats <- test2 %>% 
    mutate(cluster = final$cluster)
  
  
  for (i in 1:6) {
    ggplot(data = stats, aes(x = cluster, y = stats[,i], group = cluster))+
      geom_boxplot()+
      geom_jitter(color = "red", width = .2)+
      labs(y = names(stats)[i],
           x = "cluster")+
      theme(panel.border = element_rect(colour = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank())
    
    
    ggsave(
      filename = paste("tmpfigures/mussel_cluster_stats/", "bystreamflowAndTemp", names(stats)[i], ".png", sep = ""),
      plot = last_plot(),
      width = 11,
      height = 9,
      units = "cm",
      dpi = 150)
  }
  
  
  
  
  
  
  
  
