#### Loading Libraries & Data####
library(tidyverse)
library(e1071)
library(class)
library(tree)
library(randomForest)
library(HDclassif)

load("/Users/themiskavour/Documents/AUEB/Big Data/Assignment_3/Ramaswamy.RData")

#### EDA & Preprocessing ####

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

#### Naive Bayes ####


nbm <- naiveBayes(y = y_train,x = trainset[,-1])
nb_pred <- predict(nbm, newdata = df_test_sc, type = 'class')
table(y_test, nb_pred)
table(y_test)
table(nb_pred)
paste0(round(100*sum(diag(table(y_test, nb_pred)))/dim(df_test_sc)[1],2),'%')    # "42.59%"

#### k-nearest neighbors ####

set.seed(3622114)
km1 <- knn(train = trainset[,-1],
           test = df_test_sc,
           cl = trainset[,1],
           k = 14)
table(y_test,km1)
paste0(round(100*sum(diag(table(y_test, km1)))/dim(df_test_sc)[1],2),'%')    # "42.59%"

#### Decision tree ####


set.seed(3622114)
fit_tree <- tree(as.factor(y_train) ~ ., data = trainset)
tr <- predict(fit_tree, newdata = as.data.frame(df_test_sc), type = 'class')
table(y_test,tr)

paste0(round(100*sum(diag(table(y_test, tr)))/dim(df_test_sc)[1],2),'%')    # "38.89%"

#### Support Vector Machine ####

set.seed(3622114)
svm_model <- svm(as.factor(y_train) ~ . , data = trainset)
svm.pred <- predict(svm_model, newdata = df_test_sc)
table(y_test, round(svm.pred))

paste0(round(100*sum(diag(table(y_test, svm.pred)))/dim(df_test_sc)[1],2),'%')    # "48.15%"


#### Random Forrest ####


set.seed(3622114)
classifier_RF = randomForest(x = trainset[,-1],
                             y = as.factor(trainset$y_train),
                             ntree = 500)
tree_pred = predict(classifier_RF, newdata = df_test_sc)
table(y_test, tree_pred)

paste0(round(100*sum(diag(table(y_test, tree_pred)))/dim(df_test_sc)[1],2),'%')    # "61.11%"

#### HDClassif ####

prms <- hdda(trainset[,-1], trainset[,1], scaling = FALSE, model ="all", graph = TRUE)
predict(prms, df_test_sc, y_test)    # "61.11"

