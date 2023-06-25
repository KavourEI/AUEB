#### Libraries ####

library(pgmm)    # For data
library(tidyverse)    # For manipulation and ggplot
library(corrplot)    # For correlation plot
library(stats)    # hclust - clustering function
library(cluster)
library(mclust)
library(factoextra)
library(cowplot)

set.seed(3622114)

#### Preprocessing and EDA ####
data(coffee)
x <- coffee[,-c(1,2)]
x <- scale(x)

head(x)    # Check the values

apply(x,2 ,function(n) sum(is.na(n)))    # Check for na values

x %>%
  as.data.frame() %>% 
  pivot_longer(colnames(x)) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = value)) +    # Draw each column as histogram
  geom_histogram(aes(y = ..density..)) + 
  geom_density(col = "#1b98e0", size = 2) + 
  facet_wrap(~ name, scales = "free")

par(mfrow = c(1,1))
corrmatrix <- cor(x)
corrplot(corrmatrix, method = 'pie')

#### Euclidean Distance based clustering ####

dist_euc <- dist(x, method = "euclidian") 

# Hierarchical clustering by using different linkage function
cl_simple <- hclust(dist_euc, method = "single")
cl_compl <- hclust(dist_euc, method = "complete")
cl_avg <- hclust(dist_euc, method = "average")
cl_wardD <- hclust(dist_euc,method="ward.D")
cl_wardD2 <- hclust(dist_euc,method="ward.D2")
cl_mcq <- hclust(dist_euc,method="mcquitty")
cl_median <- hclust(dist_euc,method="median")
cl_centroid <- hclust(dist_euc,method="centroid")  

par(mfrow = c(2,4))
plot(cl_simple, main = "Single Linkage", sub = "2-clusters", xlab = "", cex = .7)
rect.hclust(cl_simple, 2) 
plot(cl_compl, main = "Complete Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_compl, 3) 
plot(cl_avg, main = "Average Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_avg, 3) 
plot(cl_wardD, main = "Ward.D Linkage", sub = "3,4-clusters", xlab = "", cex = .7)
rect.hclust(cl_wardD, 4,border =4)
rect.hclust(cl_wardD, 3)
plot(cl_wardD2, main = "Ward.D2 Linkage", sub = "3,4-clusters", xlab = "", cex = .7)
rect.hclust(cl_wardD2, 4,border =4)
rect.hclust(cl_wardD2, 3)
plot(cl_mcq, main = "McQuitty Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_mcq, 3)
plot(cl_median, main = "Median Linkage", sub = "4-clusters", xlab = "", cex = .7)
rect.hclust(cl_median, 4)
plot(cl_centroid, main = "Centroid Linkage", sub = "4-clusters", xlab = "", cex = .7)
rect.hclust(cl_centroid, 4)

#### Manhattan Distance based clustering ####

dist_manh <- dist(x, method = "manhattan") 

# Hierarchical clustering by using different linkage function
cl_simple_manh <- hclust(dist_manh, method = "single")
cl_compl_manh <- hclust(dist_manh, method = "complete")
cl_avg_manh <- hclust(dist_manh, method = "average")
cl_wardD_manh <- hclust(dist_manh,method="ward.D")
cl_wardD2_manh <- hclust(dist_manh,method="ward.D2")
cl_mcq_manh <- hclust(dist_manh,method="mcquitty")
cl_median_manh <- hclust(dist_manh,method="median")
cl_centroid_manh <- hclust(dist_manh,method="centroid")  

par(mfrow = c(2,4))
plot(cl_simple_manh, main = "Single Linkage", sub = "2-clusters", xlab = "", cex = .7)
rect.hclust(cl_simple_manh, 2) 
plot(cl_compl_manh, main = "Complete Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_compl_manh, 3) 
plot(cl_avg_manh, main = "Average Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_avg_manh, 3) 
plot(cl_wardD_manh, main = "Ward.D Linkage", sub = "3,4-clusters", xlab = "", cex = .7)
rect.hclust(cl_wardD_manh, 4,border =4)
rect.hclust(cl_wardD_manh, 3)
plot(cl_wardD2_manh, main = "Ward.D2 Linkage", sub = "3,4-clusters", xlab = "", cex = .7)
rect.hclust(cl_wardD2_manh, 4,border =4)
rect.hclust(cl_wardD2_manh, 3)
plot(cl_mcq_manh, main = "McQuitty Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_mcq_manh, 3)
plot(cl_median_manh, main = "Median Linkage", sub = "4-clusters", xlab = "", cex = .7)
rect.hclust(cl_median_manh, 4)
plot(cl_centroid_manh, main = "Centroid Linkage", sub = "4-clusters", xlab = "", cex = .7)
rect.hclust(cl_centroid_manh, 4)


#### Minkowski Distance based clustering ####

dist_mink <- dist(x, method = "minkowski") 

# Hierarchical clustering by using different linkage function
cl_simple_mink <- hclust(dist_mink, method = "single")
cl_compl_mink <- hclust(dist_mink, method = "complete")
cl_avg_mink <- hclust(dist_mink, method = "average")
cl_wardD_mink <- hclust(dist_mink,method="ward.D")
cl_wardD2_mink <- hclust(dist_mink,method="ward.D2")
cl_mcq_mink <- hclust(dist_mink,method="mcquitty")
cl_median_mink <- hclust(dist_mink,method="median")
cl_centroid_mink <- hclust(dist_mink,method="centroid")  

par(mfrow = c(2,4))
plot(cl_simple_mink, main = "Single Linkage", sub = "2-clusters", xlab = "", cex = .7)
rect.hclust(cl_simple_mink, 2)
plot(cl_compl_mink, main = "Complete Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_compl_mink, 3) 
plot(cl_avg_mink, main = "Average Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_avg_mink, 3) 
plot(cl_wardD_mink, main = "Ward.D Linkage", sub = "3,4-clusters", xlab = "", cex = .7)
rect.hclust(cl_wardD_mink, 4,border =4)
rect.hclust(cl_wardD_mink, 3)
plot(cl_wardD2_mink, main = "Ward.D2 Linkage", sub = "3,4-clusters", xlab = "", cex = .7)
rect.hclust(cl_wardD2_mink, 4,border =4)
rect.hclust(cl_wardD2_mink, 3)
plot(cl_mcq_mink, main = "McQuitty Linkage", sub = "3-clusters", xlab = "", cex = .7)
rect.hclust(cl_mcq_mink, 3)
plot(cl_median_mink, main = "Median Linkage", sub = "4-clusters", xlab = "", cex = .7)
rect.hclust(cl_median_mink, 4)
plot(cl_centroid_mink, main = "Centroid Linkage", sub = "4-clusters", xlab = "", cex = .7)
rect.hclust(cl_centroid_mink, 4)

euc_clust_simple <- cutree(cl_simple,2)
euc_clust_wd <- cutree(cl_wardD,3)
euc_clust_wd2 <- cutree(cl_wardD2,3)

manh_clust_simple <- cutree(cl_simple_manh,2)
manh_clust_compl <- cutree(cl_compl_manh,3)
manh_clust_wd <- cutree(cl_wardD_manh,3)
manh_clust_wd2 <- cutree(cl_wardD2_manh,3)

mink_clust_simple <- cutree(cl_simple_mink,2)
mink_clust_wd <- cutree(cl_wardD_mink,3)
mink_clust_wd2 <- cutree(cl_wardD2_mink,3)

par(mfrow = c(1,1))

# Contingency table - Euclidean distance
table(euc_clust_simple, coffee[,1])
table(euc_clust_wd, coffee[,1])
table(euc_clust_wd2, coffee[,1])

# Contingency table - Manhattan distance
table(manh_clust_simple, coffee[,1])
table(manh_clust_compl, coffee[,1])
table(manh_clust_wd, coffee[,1])
table(manh_clust_wd2, coffee[,1])

# Contingency table - Minkowski distance
table(mink_clust_simple, coffee[,1])
table(mink_clust_wd, coffee[,1])
table(mink_clust_wd2, coffee[,1])

#### Adjusted Rand Index ####

# Euclidean Distance

adjustedRandIndex(cutree(cl_simple, k = 2),coffee[,1])
adjustedRandIndex(cutree(cl_wardD, k = 3),coffee[,1])
adjustedRandIndex(cutree(cl_wardD2, k = 3),coffee[,1])

# Manhattan Distance

adjustedRandIndex(cutree(cl_simple_manh, k = 2),coffee[,1])
adjustedRandIndex(cutree(cl_compl_manh, k = 3),coffee[,1])
adjustedRandIndex(cutree(cl_wardD_manh, k = 3),coffee[,1])
adjustedRandIndex(cutree(cl_wardD2_manh, k = 3),coffee[,1])

# Minkowski Distance

adjustedRandIndex(cutree(cl_simple_mink, k = 2),coffee[,1])
adjustedRandIndex(cutree(cl_wardD_mink, k = 3),coffee[,1])
adjustedRandIndex(cutree(cl_wardD2_mink, k = 3),coffee[,1])

#### Principal Component ####

clst_res_e2 <- as.factor(euc_clust_simple)
clst_res_e3 <- as.factor(euc_clust_wd2)

pr_comp <- princomp(x) 
summary(pr_comp)

fviz_screeplot(pr_comp, addlabels = TRUE, title = "Principal Component Analysis \n Scree Plot") +
  ggeasy::easy_center_title()

fviz_pca_var(pr_comp, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)+
  ggeasy::easy_center_title()

# 1st PCA 
par(mfrow = c(1,2))
boxplot(pr_comp$scores[,1]~clst_res_e2, col=rainbow(2), main = '2-Clusters generated \n by PC1 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')
boxplot(pr_comp$scores[,1]~clst_res_e3, col=rainbow(3), main = '3-Clusters generated \n by PC1 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')

# 2nd PCA 
boxplot(pr_comp$scores[,2]~clst_res_e2, col=rainbow(2), main = '2-Clusters generated \n by PC2 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')
boxplot(pr_comp$scores[,2]~clst_res_e3, col=rainbow(3), main = '3-Clusters generated \n by PC2 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')

# 3rd PCA
boxplot(pr_comp$scores[,3]~clst_res_e2, col=rainbow(2), main = '2-Clusters generated \n by PC3 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')
boxplot(pr_comp$scores[,3]~clst_res_e3, col=rainbow(3), main = '3-Clusters generated \n by PC3 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')

# 4th PCA
boxplot(pr_comp$scores[,4]~clst_res_e2, col=rainbow(2), main = '2-Clusters generated \n by PC4 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')
boxplot(pr_comp$scores[,4]~clst_res_e3, col=rainbow(3), main = '3-Clusters generated \n by PC4 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')

# 5th PCA
boxplot(pr_comp$scores[,5]~clst_res_e2, col=rainbow(2), main = '2-Clusters generated \n by PC5 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')
boxplot(pr_comp$scores[,5]~clst_res_e3, col=rainbow(3), main = '3-Clusters generated \n by PC5 scores', xlab = 'Cluster Groups', ylab = 'PC1 Scores')

pr_selected <- data.frame(PC1 = pr_comp$scores[,1], 
                         PC2 = pr_comp$scores[,2])

pr_selected_Clust <- pr_selected %>% 
  mutate(eu_cl_simple = euc_clust_simple,
         eu_cl_wd2 = euc_clust_wd2)

plot1 <- ggplot(pr_selected_Clust, aes(x = PC1, y = PC2, color = as.factor(euc_clust_simple))) +
  geom_point(size = 2) + 
  theme(legend.position="bottom") +
  labs(color = "Clusters") +
  ggtitle("Single Link Method") +
  ggeasy::easy_center_title()
plot2 <- ggplot(pr_selected_Clust, aes(x = PC1, y = PC2, color = as.factor(euc_clust_wd2))) +
  geom_point(size = 2) + 
  theme(legend.position="bottom") +
  labs(color = "Clusters") +
  ggtitle("Ward.D2 Link Method") +  
  ggeasy::easy_center_title()
plot_row <- plot_grid(plot1, plot2)
title <- ggdraw() + 
  draw_label(
    "Hierarchical Clustering Results on Principal Components",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 2)
  )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1,1)
)

# K-Means Clustering

kmeans_2C <- kmeans(x, centers = 2, iter.max = 25, trace = TRUE)
kmeans_3C <- kmeans(x, centers = 3, iter.max = 25, trace = TRUE)

table(kmeans_2C$cluster)
table(kmeans_3C$cluster)

kmeans_2C$centers
kmeans_3C$centers

kmeans_2C$withinss
kmeans_3C$withinss

kmeans_2C$betweenss
kmeans_3C$betweenss

pr_selected_KM <- pr_selected %>% 
  mutate(kmeans_2C = kmeans_2C$cluster,
         kmeans_3C = kmeans_3C$cluster)

plot1 <- ggplot(pr_selected_KM, aes(x = PC1, y = PC2, color = as.factor(kmeans_2C))) +
  geom_point(size = 2) + 
  theme(legend.position="bottom") +
  labs(color = "Clusters") +
  ggtitle("KMeans wt 2 Centers") +
  ggeasy::easy_center_title()
plot2 <- ggplot(pr_selected_KM, aes(x = PC1, y = PC2, color = as.factor(kmeans_3C))) +
  geom_point(size = 2) + 
  theme(legend.position="bottom") +
  labs(color = "Clusters") +
  ggtitle("KMeans wt 3 Centers") +  
  ggeasy::easy_center_title()
plot_row <- plot_grid(plot1, plot2)
title <- ggdraw() + 
  draw_label(
    "KMeans Clustering on Principal Components",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 2)
  )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1,1)
)

adjustedRandIndex(coffee[,1], kmeans_2C$cluster)
adjustedRandIndex(coffee[,1], kmeans_3C$cluster)
par(mfrow=c(1,2))
plot(silhouette(kmeans_2C$cluster, dist(x, method = 'euclidean')), col=2:3, main ='Single')
plot(silhouette(kmeans_3C$cluster, dist(x, method = 'euclidean')), col=2:3, main ='Ward.D2')
par(mfrow=c(1,1))


mdl_cl <- Mclust(x)
names(mdl_cl)    # Elements of mdl_cl object generated earlier.
plot(mdl_cl, x, what = 'BIC')
title('Model Based Clustering')
print(mdl_cl)
table(mdl_cl$classification)

pr_selected_MB <- pr_selected %>% 
  mutate(G_Truth = coffee[,1],
         mb_cl = mdl_cl$classification)

plot1 <- ggplot(pr_selected_MB, aes(x = PC1, y = PC2, color = as.factor(G_Truth))) +
  geom_point(size = 2) + 
  theme(legend.position="bottom") +
  labs(color = "Clusters") +
  ggtitle("Ground Truth") +
  ggeasy::easy_center_title()
plot2 <- ggplot(pr_selected_MB, aes(x = PC1, y = PC2, color = as.factor(mb_cl))) +
  geom_point(size = 2) + 
  theme(legend.position="bottom") +
  labs(color = "Clusters") +
  ggtitle("Model Based Clustering") +  
  ggeasy::easy_center_title()
plot_row <- plot_grid(plot1, plot2)
title <- ggdraw() + 
  draw_label(
    "Model Based Clustering on Principal Components",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 2)
  )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1,1)
)
