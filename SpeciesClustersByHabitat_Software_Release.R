#load libraries
library(tidyverse)
library(sf)
library(gridExtra)
library(factoextra)







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








