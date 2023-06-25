library(tidyverse)
library(nnet)
library(corrplot)
library(factoextra)
library(mclust)
library(fpc)
library(randomForest)
library(dbscan)


load("/Users/themiskavour/Documents/AUEB/Big Data/Assignment_3/Ramaswamy.RData")

#### EDA & Preprocessing ####

set.seed(999)

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
df_test_sc <- scale(as.data.frame(df_test$x))    

start <- Sys.time()
rf_model <- randomForest(df_train_sc, proximity = TRUE)
end <- Sys.time()
end - start
proximity_matrix <- rf_model$proximity
