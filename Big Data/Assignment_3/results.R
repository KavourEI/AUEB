#### Exercise 1  #####

###### Loading Libraries & Data ######
library(tidyverse)
library(e1071)
library(class)
library(tree)
library(randomForest)
library(HDclassif)

load("/Users/themiskavour/Documents/AUEB/Big Data/Assignment_3/Ramaswamy.RData")

###### EDA & Preprocessing  ######

set.seed(3622114)

str(Ramaswamy)    # List with two sublists (Train and Test)

df_train <- Ramaswamy$Train
df_test <- Ramaswamy$Test

str(df_train)
str(df_test)

length(df_train$y)    # 144 elements
dim(df_train$x)    # 144 rows and 16063 columns
y_train <- as.factor(df_train$y)

length(df_test$y)    # 54 elements
dim(df_test$x)    # 54 rows and 16063 columns
y_test <- as.factor(df_test$y)

which(is.na(df_train$x))    # 0 Null values
which(is.na(df_test$x))    # 0 Null Values

df_train_sc <- scale(as.data.frame(df_train$x))    # Scale Train Data
df_test_sc <- scale(as.data.frame(df_test$x))    # Scale Test Date
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)
trainset <- as.data.frame(cbind(y_train, df_train_sc))

###### Naive Bayes  ######


nbm <- naiveBayes(y = y_train,x = trainset[,-1])
nb_pred <- predict(nbm, newdata = df_test_sc, type = 'class')
table(y_test, nb_pred)
table(y_test)
table(nb_pred)
paste0(round(100*sum(diag(table(y_test, nb_pred)))/dim(df_test_sc)[1],2),'%')    # "42.59%"

###### k-nearest neighbors  ######

set.seed(3622114)
km1 <- knn(train = trainset[,-1],
           test = df_test_sc,
           cl = trainset[,1],
           k = 14)
table(y_test,km1)
paste0(round(100*sum(diag(table(y_test, km1)))/dim(df_test_sc)[1],2),'%')    # "42.59%"

###### Decision tree  ######


set.seed(3622114)
fit_tree <- tree(as.factor(y_train) ~ ., data = trainset)
tr <- predict(fit_tree, newdata = as.data.frame(df_test_sc), type = 'class')
table(y_test,tr)

paste0(round(100*sum(diag(table(y_test, tr)))/dim(df_test_sc)[1],2),'%')    # "38.89%"

###### Support Vector Machine  ######

set.seed(3622114)
svm_model <- svm(as.factor(y_train) ~ . , data = trainset)
svm.pred <- predict(svm_model, newdata = df_test_sc)
table(y_test, round(svm.pred))

paste0(round(100*sum(diag(table(y_test, svm.pred)))/dim(df_test_sc)[1],2),'%')    # "48.15%"


###### Random Forrest  ######


set.seed(3622114)
classifier_RF = randomForest(x = trainset[,-1],
                             y = as.factor(trainset$y_train),
                             ntree = 500)
tree_pred = predict(classifier_RF, newdata = df_test_sc)
table(y_test, tree_pred)

paste0(round(100*sum(diag(table(y_test, tree_pred)))/dim(df_test_sc)[1],2),'%')    # "61.11%"

###### HDClassif  ######

prms <- hdda(trainset[,-1], trainset[,1], scaling = FALSE, model ="all", graph = TRUE)
predict(prms, df_test_sc, y_test)    # "61.11"


#### Exercise 2  ####

###### Libraries  ######

library(pgmm)    # For data
library(tidyverse)    # For manipulation and ggplot
library(corrplot)    # For correlation plot
library(stats)    # hclust - clustering function
library(cluster)
library(mclust)
library(factoextra)
library(cowplot)

set.seed(3622114)

###### Preprocessing and EDA  ######
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

###### Euclidean Distance based clustering  ######

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

###### Manhattan Distance based clustering  ######

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


###### Minkowski Distance based clustering  ######

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

###### Adjusted Rand Index  ######

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

###### Principal Component  ######

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

#### Exercise 3  ####

###### Libraries  ######

library(igraph)
library(network)
library(intergraph)
library(ergm)
library(latentnet)

###### Building the Network  ######

m1 <- graph( ~'Themis'-'ChristosR'-'Themis'-'Thodoris'-'Themis'-'Thrasivoulos'-'Themis'-'Zisis'-'Themis'-'NikosD'-'Themis'-'DimitrisK'-'Themis'-'Athanasia'-'Themis'-'Nikos'-'Themis'-'Theodosia'-'Themis'-'Elina'-'Themis'-'NikosDaoul'-'Themis'-'Tasos'-'ChristosR'-'Thodoris'-'ChristosR'-'Thrasivoulos'-'ChristosR'-'Zisis'-'ChristosR'-'NikosD'-'ChristosR'-'DimitrisK'-'ChristosR'-'Theodosia'-'ChristosR'-'Elina'-'ChristosR'-'NikosDaoul'-'ChristosR'-'Tasos'-'Thodoris'-'Thrasivoulos'-'Thodoris'-'Zisis'-'Thodoris'-'NikosD'-'Thodoris'-'DimitrisK'-'Thodoris'-'Nikos'-'Thodoris'-'Theodosia'-'Thodoris'-'Elina'-'Thodoris'-'NikosDaoul'-'Thodoris'-'Tasos'-'Thrasivoulos'-'Zisis'-'Thrasivoulos'-'DimitrisK'-'Thrasivoulos'-'Nikos'-'Thrasivoulos'-'Theodosia'-'Thrasivoulos'-'Elina'-'Thrasivoulos'-'Tasos'-'Zisis'-'Tasos'-'Zisis'-'Athanasia'-'Zisis'-'Theodosia'-'Zisis'-'NikosDaoul'-'NikosD'-'Theodosia'-'NikosD'-'Elina'-'NikosD'-'Tasos'-'NikosD'-'Tasos'-'DimitrisK'-'Nikos'-'DimitrisK'-'Theodosia'-'DimitrisK'-'Elina'-'DimitrisK'-'Tasos'-'Athanasia'-'Theodosia'-'Nikos'-'Theodosia'-'Nikos'-'Elina'-'Nikos'-'Tasos'-'Theodosia'-'Elina'-'Theodosia'-'NikosDaoul'-'NikosDaoul'-'Tasos',
             'Themis'-'Nektaria'-'Themis'-'Nikos'-'Themis'-'Evangelia'-'Themis'-'Thanasis'-'Themis'-'Kaliopi'-'Themis'-'Machi'-'Themis'-'Theodosia'-'Nektaria'-'Nikos'-'Nektaria'-'Evangelia'-'Nektaria'-'Thanasis'-'Nektaria'-'Kaliopi'-'Nektaria'-'Machi'-'Nektaria'-'Theodosia'-'Nikos'-'Evangelia'-'Nikos'-'Thanasis'-'Nikos'-'Kaliopi'-'Nikos'-'Machi'-'Nikos'-'Theodosia'-'Evangelia'-'Thanasis'-'Evangelia'-'Kaliopi'-'Evangelia'-'Machi'-'Evangelia'-'Theodosia'-'Thanasis'-'Machi'-'Thanasis'-'Theodosia'-'Kaliopi'-'Machi'-'Kaliopi'-'Theodosia'-'Machi'-'Theodosia',
             'Themis'-'Vivian'-'Themis'-'Anna'-'Themis'-'Kostas'-'Themis'-'AkisM'-'Themis'-'AkisK'-'Themis'-'Myrto'-'Themis'-'Theodosia'-'Themis'-'Evangelia'-'Vivian'-'Anna'-'Vivian'-'AkisM'-'Vivian'-'AkisK'-'Vivian'-'Theodosia'-'Anna'-'Kostas'-'Anna'-'AkisK'-'Anna'-'Myrto'-'Anna'-'Theodosia'-'Anna'-'Evangelia'-'Kostas'-'Myrto'-'Kostas'-'Theodosia'-'Kostas'-'Evangelia'-'AkisM'-'AkisK'-'AkisM'-'Theodosia'-'AkisK'-'Myrto'-'AkisK'-'Theodosia'-'AkisK'-'Evangelia'-'Myrto'-'Theodosia'-'Myrto'-'Evangelia'-'Theodosia'-'Evangelia', 
             'Themis'-'Theodosia'-'Themis'-'Stavros'-'Themis'-'Tina'-'Themis'-'Erina'-'Themis'-'Despoina'-'Themis'-'Artemis'-'Themis'-'AnnaMaria'-'Themis'-'Thanos'-'Theodosia'-'Stavros'-'Theodosia'-'Tina'-'Theodosia'-'Erina'-'Theodosia'-'Despoina'-'Theodosia'-'Artemis'-'Theodosia'-'AnnaMaria'-'Theodosia'-'Thanos'-'Stavros'-'Tina'-'Stavros'-'Erina'-'Tina'-'Erina'-'Tina'-'Artemis'-'Tina'-'Thanos'-'Despoina'-'Artemis'-'Despoina'-'AnnaMaria'-'Despoina'-'Thanos'-'Artemis'-'AnnaMaria'-'Artemis'-'Thanos'-'AnnaMaria'-'Thanos'-'Artemis'-'AkisK'-'Artemis'-'Thodoris'-'Despoina'-'Anna'-'Despoina'-'Myrto'-'Despoina'-'Evangelia'-'Despoina'-'Kostas')

###### Visualizing the Network  ######

plot(m1,edge.arrow.size=2, main = "Social Networks of Themis Kavour")    # Plot the network 

m1[]    # Show the adj.matrix

E(m1)    # Show the edges of the object 

V(m1)    #Show the vertices of the object

V(m1)$name
V(m1)$gender <- c(rep("male",7),"female","male",rep("female",2),rep("male",2),rep("female",2),"male",rep("female",4),rep("male",3),"female","male",rep("female",3),"male","female","male")    #Genders 

# You may erase this part!!
V(m1)$Introduction <-  c(rep("Alexandroupolis",9),"Thessaloniki",rep("Alexandroupolis",3),'Kozani',rep('Thessaloniki',3), "Kozani",rep('Thessaloniki',6),rep("Athens",7))   # Where introduction took place

vertex_attr(m1)

plot(m1, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5, vertex.color=c( "pink", "skyblue")[1+(V(m1)$gender=="male")] )
legend(x=1.5, y=1, c('Female','Male'), pch=21, col="#777777", pt.bg=c( "pink", "skyblue"), pt.cex=2, cex=.8, bty="n", ncol=1, title = 'Gender')

vcount(m1)    # Number of nodes
ecount(m1)    # Number of edges
edge_density(m1)
reciprocity(m1)    # Expected as it is a non directed social network graph

transitivity(m1, type = 'global')    # ratio of triangles to connected triples

diameter(m1)    # The longest possible route one could take

s_degree <- degree(m1, mode = 'all')
plot(m1, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5, vertex.color=c( "pink", "skyblue")[1+(V(m1)$gender=="male")], vertex.size = s_degree)
legend(x=1.5, y=1, c('Female','Male'), pch=21, col="#777777", pt.bg=c( "pink", "skyblue"), pt.cex=2, cex=.8, bty="n", ncol=1, title = 'Gender')

# Community Detection
m <- make_graph(V(m1)$name)
fc <- cluster_fast_greedy(as.undirected(m))
plot(fc,m1)

d = get.diameter(m1)
E(m1)$color = "grey"
E(m1)$width = 2
E(m1, path=d)$color = "red"
E(m1, path=d)$width = 3
V(m1)$color  = "SkyBlue2"
V(m1)[d]$color = "red"
coords = layout.fruchterman.reingold(m1)
plot(m1, layout=coords, vertex.size=3)

hist(s_degree, breaks=1:vcount(m1)-1, main="Node degree of Social Network",xlab = 'Social Connections Degree', labels = TRUE)
lines(density(s_degree))
polygon(density(s_degree),col=rgb(1,0,1,.2))


net.m1 <- asNetwork(m1)
model1 <- ergm(net.m1 ~ edges)
model1
summary(model1)

control.ergm(seed = 3622114)
m1gof <- gof(model1, GOF = ~ distance + espartners + triadcensus,
             verbose = TRUE, interval = 5e+4)
par(mfrow = c(2,2))
plot(m1gof, cex.lab=1.6, cex.axis=1.6, plotlogodds = TRUE)

model.fit <- ergmm(net.m1 ~ euclidean(d = 2, G = 2), verbose = TRUE)
summary(model.fit)
attr(model.fit$sample, "Q")
par(mfrow = c(1,2))
plot(model.fit)
plot(model.fit, pie = TRUE, vertex.cex = 2.5)

model.fit.2 <- ergmm(net.m1 ~ euclidean(d = 2, G = 3), verbose = TRUE)
summary(model.fit.2)
attr(model.fit.2$sample, "Q")
par(mfrow = c(1,2))
plot(model.fit.2)
plot(model.fit.2, pie = TRUE, vertex.cex = 2.5)

model.fit.3 <- ergmm(net.m1 ~ euclidean(d = 2, G = 4), verbose = TRUE)
summary(model.fit.3)
attr(model.fit.3$sample, "Q")
par(mfrow = c(1,2))
plot(model.fit.3)
plot(model.fit.3, pie = TRUE, vertex.cex = 2.5)

model.fit.4 <- ergmm(net.m1 ~ euclidean(d = 2, G = 5), verbose = TRUE)
summary(model.fit.4)
attr(model.fit.4$sample, "Q")
par(mfrow = c(1,2))
plot(model.fit.4)
plot(model.fit.4, pie = TRUE, vertex.cex = 2.5)
