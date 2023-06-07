##### Exercise 1 #####

#### Required Libraries ####

library(biglm)
library(data.table)
library(beepr)
library(dplyr)
library(boot)

#### Data Generation ####

getwd()
setwd('/Users/themiskavour/Documents/AUEB/Big_Data/Assignment_2')

set.seed(3622114)
p <- rpois(1, lambda = 120)
n <- 2000000
b <- rt(p, df = 5)
outFile <- "big_data_regression.csv"
zz <- file(outFile , "w")
colNames <- c("y", paste0("x", 1:(p-1))) 
colNames <- paste0(colNames , collapse=",") 
cat(colNames , "\n", file = zz)

for (i in 1:n){
  x <- matrix(rnorm(p-1), nrow = 1)
  y <- b[1] + x %*% b[-1] + rnorm(1)
  xy <- cbind(y, x) 
  cat(paste0(xy, collapse = ","), file = zz, append=TRUE,"\n") 
  if( i %% 100000 == 0){
    cat(paste0("write to file ",outFile, " line: ", i), "\n")
  }
}
close(zz)
beep(sound=3)


#### Incremental Loading of HUGE csv ####

csv_file <- "/Users/themiskavour/Documents/AUEB/Big_Data/Assignment_2/big_data_regression.csv"

all <- fread(csv_file)
beep(sound=3)
head(all)

dim(all)    # Dimentions of Data
head(colnames(all))    # First five column names
tail(colnames(all))    # Last five column names

myFormula <- paste0('x',1:123, collapse=' + ')
myFormula <- paste0('y ~ ', myFormula, collapse = '')

fit <- as.formula(myFormula)

# Create a list where every element is a sub table containing all the data generated earlier (200 sub tables with dimentions 10000x124)
chunk_size <- 10000
num_chunks <- ceiling(nrow(all) / chunk_size)
matrix_list <- split(all, rep(1:num_chunks, each = chunk_size, length.out = nrow(all)))

fit_incr <- bigglm(fit, matrix_list[[1]])    # Fit first model
for(i in 2:length(matrix_list)){
  fit_incr <- update(fit_incr,matrix_list[[i]])
}    # Update model with the rest of the data
beep(sound=4)

summary(fit_incr)

#### Question 2 #### 

Y <- all[,1]
Y_mat <- as.matrix(Y)

# Extract the X variables
X <- all[,2:dim(all)[2]]

# Add a column of ones to the X matrix
X <- cbind(1, X)
X_mat <- as.matrix(X)
beep(sound=4)

# Calculate the betas
betas <- solve(t(X_mat)%*%X_mat)%*%t(X_mat)%*%Y_mat
beep(sound=4)

#### Question 3 ####

num_subsamples <- 100
coefficient_estimates <- list()
standard_errors <- list()
data_all <- data.frame(all)
for (i in 1:num_subsamples) {
  subsample <- sample(50000, replace = TRUE)
  X_subs <- data_all[subsample, 2:124]    # Sub-samples for x
  y_subs <- data_all[subsample, 1]    # Sub-samples for y
  sub_data <- cbind(y_subs, X_subs)
  sub_model <- lm(y_subs ~ ., data = sub_data)    # Fit model with subsamples
  
  # Obtain coefficient estimates and their standard errors
  sum_sub <- summary(sub_model)
  coefficient_estimates[[i]] <- sum_sub$coefficients[,1]
  standard_errors[[i]] <- sum_sub$coefficients[,2]
}
beep(sound = 4)

mat_se <- matrix(unlist(standard_errors), ncol = 124, byrow = T)
mat_coef <- matrix(unlist(coefficient_estimates), ncol = 124, byrow = T)
west <- numeric(dim(all)[2]-1)

for(i in 1:124){
  inverse_variances <- sum(1 / mat_se[,i]^2)
  west[i]<- sum(mat_coef[,i] * (1/mat_se[,i]^2))/sum(1/mat_se[,i]^2)
}
west
summary(fit_incr)    # Comparison with prev results
tail(west, n =5)


##### Exercise 2 #####

#### Required Libraries ####

library(R.utils)
library(car)
library(ggplot2)
library(tidyr)
library(biglm)
library(data.table)
library(dplyr)
library(beepr)
#### Importing data ####

ex2_data <- fread(file = '/Users/themiskavour/Documents/AUEB/Big_Data/Assignment_2/Packages/2004.csv.bz2')
beep(sound=4)

dim(ex2_data)    # Dimensions 7129270x29
str(ex2_data)

ex2_dt_11 <- ex2_data %>% 
  select(ArrDelay, Month, DayOfWeek, Distance, DepDelay, DepTime)

ex2_dt_12 <- ex2_dt_11[ex2_dt_11$ArrDelay >0]
dim(ex2_dt_12)    #3.305.169x6

shapiro.test(ex2_dt_11$ArrDelay)    # Has limitations therefore we will not use this
ks.test(x=ex2_dt_12$ArrDelay,y='pnorm',alternative='two.sided')

# From the output we can see that the test statistic is 0.47811 and the corresponding p-value is 2.2e-16. Since the p-value is less than .05, we reject the null hypothesis. We have sufficient evidence to say that the sample data does not come from a normal distribution.

chunk_size <- 33000
num_chunks <- ceiling(nrow(ex2_dt_12) / chunk_size)
matrix_nb <- split(ex2_dt_12, rep(1:num_chunks, each = chunk_size, length.out = nrow(ex2_dt_12)))

library(MASS)
fit_g <- glm(ArrDelay~., matrix_nb[[1]], family = Gamma(link = 'log'))    # Fit first model
for(i in 2:length(matrix_nb)){
  fit_g <- update(fit_g,matrix_nb[[i]])
}    # Update model with the rest of the data
beep(sound=4)
detach("package:MASS", unload = TRUE)
summary(fit_g)

##### Exercise 3 #####

#### Required Libraries ####
library(biglm)
library(data.table)
library(beepr)
library(dplyr)
library(glmnet)

#### Importing data ####
csv_file <- "/Users/themiskavour/Documents/AUEB/Big_Data/Assignment_2/CommViolPredUnnormalizedData.txt"
dt3 <- fread(csv_file,na.strings = c("?",NA))
head(dt3)
dim(dt3)

dt3 %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

colns <- c("communityname","state","countyCode","communityCode","fold","population","householdsize","racepctblack","racePctWhite","racePctAsian","racePctHisp","agePct12t21","agePct12t29","agePct16t24","agePct65up","numbUrban","pctUrban","medIncome","pctWWage","pctWFarmSelf","pctWInvInc","pctWSocSec","pctWPubAsst","pctWRetire","medFamInc","perCapInc","whitePerCap","blackPerCap","indianPerCap","AsianPerCap","OtherPerCap","HispPerCap","NumUnderPov","PctPopUnderPov","PctLess9thGrade","PctNotHSGrad","PctBSorMore","PctUnemployed","PctEmploy","PctEmplManu","PctEmplProfServ","PctOccupManu","PctOccupMgmtProf","MalePctDivorce","MalePctNevMarr","FemalePctDiv","TotalPctDiv","PersPerFam","PctFam2Par","PctKids2Par","PctYoungKids2Par","PctTeen2Par","PctWorkMomYoungKids","PctWorkMom","NumKidsBornNeverMar","PctKidsBornNeverMar","NumImmig","PctImmigRecent","PctImmigRec5","PctImmigRec8","PctImmigRec10","PctRecentImmig","PctRecImmig5","PctRecImmig8","PctRecImmig10","PctSpeakEnglOnly","PctNotSpeakEnglWell","PctLargHouseFam","PctLargHouseOccup","PersPerOccupHous","PersPerOwnOccHous","PersPerRentOccHous","PctPersOwnOccup","PctPersDenseHous","PctHousLess3BR","MedNumBR","HousVacant","PctHousOccup","PctHousOwnOcc","PctVacantBoarded","PctVacMore6Mos","MedYrHousBuilt","PctHousNoPhone","PctWOFullPlumb","OwnOccLowQuart","OwnOccMedVal","OwnOccHiQuart","OwnOccQrange","RentLowQ","RentMedian","RentHighQ","RentQrange","MedRent","MedRentPctHousInc","MedOwnCostPctInc","MedOwnCostPctIncNoMtg","NumInShelters","NumStreet","PctForeignBorn","PctBornSameState","PctSameHouse85","PctSameCity85","PctSameState85","LemasSwornFT","LemasSwFTPerPop","LemasSwFTFieldOps","LemasSwFTFieldPerPop","LemasTotalReq","LemasTotReqPerPop","PolicReqPerOffic","PolicPerPop","RacialMatchCommPol","PctPolicWhite","PctPolicBlack","PctPolicHisp","PctPolicAsian","PctPolicMinor","OfficAssgnDrugUnits","NumKindsDrugsSeiz","PolicAveOTWorked","LandArea","PopDens","PctUsePubTrans","PolicCars","PolicOperBudg","LemasPctPolicOnPatr","LemasGangUnitDeploy","LemasPctOfficDrugUn","PolicBudgPerPop","murders","murdPerPop","rapes","rapesPerPop","robberies","robbbPerPop","assaults","assaultPerPop","burglaries","burglPerPop","larcenies","larcPerPop","autoTheft","autoTheftPerPop","arsons","arsonsPerPop","ViolentCrimesPerPop","nonViolPerPop")

colnames(dt3) <- colns
head(dt3)

dt4 <- dt3 %>% 
  select(everything()) %>% 
  na.omit(.)

d4_mur <- dt4 %>% 
  select(murders)

d4_nmur <- dt4 %>% 
  select(-murders)


lasso_fit <- glmnet(x = as.matrix(d4_nmur), y = d4_mur$murders, alpha = 1)

#lambda = 0.05
coef(lasso_fit,s=0.05)
length(coef(lasso_fit,s=0.05)[,1][coef(lasso_fit,s=0.05)[,1] >0])
l0.05_variables <- names(coef(lasso_fit,s=0.05)[,1][coef(lasso_fit,s=0.05)[,1] >0])

glm(data = dt3, murders ~ communityCode + blackPerCap + OtherPerCap + HispPerCap + PctNotSpeakEnglWell + NumInShelters + LemasSwornFT + RacialMatchCommPol + PolicAveOTWorked + murdPerPop + robberies + assaults, family = poisson(link = 'log'))


#lambda = 0.1
coef(lasso_fit,s=0.1)
length(coef(lasso_fit,s=0.1)[,1][coef(lasso_fit,s=0.1)[,1] >0])
l0.1_variables <- names(coef(lasso_fit,s=0.1)[,1][coef(lasso_fit,s=0.1)[,1] >0])

summary(glm(data = dt3, murders ~ communityCode + blackPerCap + OtherPerCap + HispPerCap + PctNotSpeakEnglWell + NumInShelters + LemasSwornFT + RacialMatchCommPol + PolicAveOTWorked + murdPerPop + robberies + assaults, family = poisson(link = 'log')))

#lambda = 0.2
coef(lasso_fit,s=0.2)
length(coef(lasso_fit,s=0.2)[,1][coef(lasso_fit,s=0.2)[,1] >0])
l0.2_variables <- names(coef(lasso_fit,s=0.2)[,1][coef(lasso_fit,s=0.2)[,1] >0])

glm(data = dt3, murders ~ communityCode + blackPerCap + OtherPerCap + HispPerCap + PctNotSpeakEnglWell + NumInShelters + LemasSwornFT + RacialMatchCommPol + PolicAveOTWorked + murdPerPop + robberies + assaults, family = poisson(link = 'log'))

#lambda = 0.3
coef(lasso_fit,s=0.3)
length(coef(lasso_fit,s=0.3)[,1][coef(lasso_fit,s=0.3)[,1] >0])
l0.3_variables <- names(coef(lasso_fit,s=0.3)[,1][coef(lasso_fit,s=0.3)[,1] >0])

glm(data = dt3, murders ~ communityCode + blackPerCap + OtherPerCap + HispPerCap + PctNotSpeakEnglWell + NumInShelters + LemasSwornFT + RacialMatchCommPol + PolicAveOTWorked + murdPerPop + robberies + assaults, family = poisson(link = 'log'))


#lambda = 0.4
coef(lasso_fit,s=0.4)
length(coef(lasso_fit,s=0.4)[,1][coef(lasso_fit,s=0.4)[,1] >0])
l0.4_variables <- names(coef(lasso_fit,s=0.4)[,1][coef(lasso_fit,s=0.4)[,1] >0])

glm(data = dt3, murders ~ communityCode + blackPerCap + OtherPerCap + HispPerCap + PctNotSpeakEnglWell + NumInShelters + LemasSwornFT + RacialMatchCommPol + PolicAveOTWorked + murdPerPop + robberies + assaults, family = poisson(link = 'log'))

#lambda = 0.5
coef(lasso_fit,s=0.5)
length(coef(lasso_fit,s=0.5)[,1][coef(lasso_fit,s=0.5)[,1] >0])
l0.5_variables <- names(coef(lasso_fit,s=0.5)[,1][coef(lasso_fit,s=0.5)[,1] >0])

glm(data = dt3, murders ~ communityCode + blackPerCap + OtherPerCap + HispPerCap + PctNotSpeakEnglWell + NumInShelters + LemasSwornFT + RacialMatchCommPol + PolicAveOTWorked + murdPerPop + robberies + assaults, family = poisson(link = 'log'))

#lambda = 1
coef(lasso_fit,s=1)
l1.0_variables <- names(coef(lasso_fit,s=1)[,1][coef(lasso_fit,s=1)[,1] >0])

glm(data = dt3, murders ~ communityCode + OtherPerCap + HispPerCap + PctNotSpeakEnglWell + LemasSwornFT + RacialMatchCommPol + PolicAveOTWorked + murdPerPop + robberies + assaults, family = poisson(link = 'log'))

#lambda = 1.5
coef(lasso_fit,s=1.5)
l1.5_variables <- names(coef(lasso_fit,s=1.5)[,1][coef(lasso_fit,s=1.5)[,1] >0])

glm(data = dt3, murders ~ OtherPerCap +  PctHousOwnOcc + LemasSwornFT + RacialMatchCommPol + murdPerPop + robberies + assaults + larcenies, family = poisson(link = 'log'))

#lambda = 2
coef(lasso_fit,s=2)
l2.0_variables <- names(coef(lasso_fit,s=2)[,1][coef(lasso_fit,s=2)[,1] >0])

glm(data = dt3, murders ~ LemasSwornFT + murdPerPop + robberies + assaults + larcenies, family = poisson(link = 'log'))

#lambda = 2.5
coef(lasso_fit,s=2.5)
l2.5_variables <- names(coef(lasso_fit,s=2.5)[,1][coef(lasso_fit,s=2.5)[,1] >0])

glm(data = dt3, murders ~ PctHousOwnOcc + LemasSwornFT + RacialMatchCommPol + murdPerPop + robberies + assaults + larcenies, family = poisson(link = 'log'))


