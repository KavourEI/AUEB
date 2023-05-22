##### Exercise 1 #####

# Libraries Installations
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install(version = "3.17")
# 
# BiocManager::install("leukemiasEset", force=T)
# BiocManager::install("qvalue", force = T)

# Importing required library
library(leukemiasEset)
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(qvalue)

# Importing Data
data("leukemiasEset")
x_all <- exprs(leukemiasEset)
table(leukemiasEset$LeukemiaType)    # Check the Leukemia Types

x <- x_all[,leukemiasEset$LeukemiaType == "AML" | leukemiasEset$LeukemiaType == "NoL"]
View(x)

#### First Question ####
 
# EDA
dim(x) # Data dimentions

averages <- x %>%
  as.data.frame %>% 
  summarise(across(1:24, mean)) %>%
  pivot_longer(cols = everything(),
               names_to = "gene",
               values_to = "average_expression") # Data needed

Groups <- c(rep('AML',12), rep('NoL',12)) # Groups Needed
ggplot(averages, aes(x = gene, y=average_expression)) +
  geom_point(aes(color = Groups), cex = 2.5) + 
  geom_segment(aes(x = 1, y = mean(averages$average_expression[1:12]), xend = 12, yend = mean(averages$average_expression[1:12])),linetype="dashed", color = "red") +
  geom_segment(aes(x = 13, y = mean(averages$average_expression[13:24]), xend = 24, yend = mean(averages$average_expression[13:24])),linetype="dashed", color = "blue") +
  ggdark::dark_theme_gray() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Acute Myeloid \n Leukemia") +
  xlab("Subject") + ylab("Mean")+
  ggeasy::easy_center_title() 

box_colours <- c(rep('red',12), rep('green',12))
boxplot(x, col=box_colours,las=3,cex.axis=0.5, main = "Acute Myeloid \n Leukemia")

plot(density(x[,1:12]),main="Acute Myeloid \n Leukemia",ylim=c(0,0.3), col = 'red') +
lines(density(x[,13:24]),main="LeuData",ylim=c(0,0.3), col = 'darkgreen')

summary(x[,1:12])
summary(x[,13:24])
#### Second Question ####
#### Summary

r_pca <- prcomp(x, scale = TRUE)
summary(r_pca)
View(get_eig(r_pca))

fviz_screeplot(r_pca, addlabels = TRUE, title = "PCA \n Scree Plot") +
  ggeasy::easy_center_title()

fviz_pca_ind(r_pca,geom = "point",
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE    
)

get_pca_var(r_pca)
fviz_pca_var(r_pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)+
  ggeasy::easy_center_title()

# Contributions of variables to PC1
fviz_contrib(r_pca, choice = "var", axes = 1)
# Contributions of variables to PC2
fviz_contrib(r_pca, choice = "var", axes = 2)
# Contributions of variables to PC3
fviz_contrib(r_pca, choice = "var", axes = 3)

fviz_pca_ind(r_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.ind = list(cos2 = 20))

fviz_pca_ind(r_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.ind = list(contrib = 20))

###### Data Transposed ######

x_t <- t(x)

r_pca_t <- prcomp(x_t, scale = TRUE)
View(get_eig(r_pca_t))

fviz_screeplot(r_pca_t, addlabels = TRUE, title = "PCA \n Scree Plot") +
  ggeasy::easy_center_title()

get_pca_var(r_pca_t)
print("The next plot may take a while so grab a cup of tea!")
fviz_pca_var(r_pca_t, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.ind = list(contrib = 3)
)+
  ggeasy::easy_center_title()

# Contributions of variables to PC1
fviz_contrib(r_pca_t, choice = "var", axes = 1)
# Contributions of variables to PC2
fviz_contrib(r_pca_t, choice = "var", axes = 2)
# Contributions of variables to PC3
fviz_contrib(r_pca_t, choice = "var", axes = 3)

fviz_pca_ind(r_pca_t,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.ind = list(cos2 = 20)
)

fviz_pca_ind(r_pca_t,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             select.ind = list(contrib = 20)
)

fviz_pca_var(r_pca_t,geom = "point",
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

fviz_pca_ind(r_pca_t,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

#### Third Question ####

# Empty vector to store p-values

p_values <- vector("numeric", nrow(x))

# Perform t-tests for each gene
for (i in 1:nrow(x)) {
  AML <- x[i, 1:12]
  NoL <- x[i, 13:24]
  results <- t.test(AML, NoL, var.equal = TRUE)
  p_values[i] <- results$p.value
}

# Plot histogram of p-values
hist(p_values, breaks = 30, freq = FALSE, main = "Histogram of p-values", xlab = "p-value")
abline(h=pi0est(p_values)$pi0, col="red", lty = 3)

#### Fourth Question ####

pi0est(p_values)$pi0

#### Fifth Question ####

m <- length(p_values)
qStar <- 0.01
# return a list of significant genes, while controlling FWER at q = 0.05
fwer_genes <- m * p_values < qStar
sum(fwer_genes)
# return a list of significant genes, while controlling FDR at q = 0.05
fdr_genes <- qvalue(p = p_values, lambda = 0, fdr.level = qStar)$significant
sum(fdr_genes)
# return a list of significant genes, while controlling pFDR at q = 0.05
qvalue_genes <- qvalue(p = p_values, fdr.level = qStar, pfdr = TRUE)$significant
sum(qvalue_genes)

#### Sixth Question ####

fdr_genes    # Contains which genes are rejected or accepted.

false_ <- table(fdr_genes)[[1]]
true_ <- table(fdr_genes)[[2]]

test_res <- data.frame(Count = c(false_, true_))
rownames(test_res) <- c("False", "True")

'FDR Result' <- c(c('False', 'True'))
ggplot(data=test_res, aes(x=rownames(test_res), y=Count, fill = `FDR Result`)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = Count), vjust = -0.1) +
  ggdark::dark_theme_gray() +
  ggtitle("Count of FDR Results") +
  xlab("Result") + ylab("Count")+
  ggeasy::easy_center_title()

dim(x)

leuk <- x[,1:12]
nol <- x[,13:24]

leuk_means <- apply(leuk, MARGIN = 1, mean)
nol_means <- apply(nol, MARGIN = 1, mean)

head(leuk_means)
head(nol_means)

diff_means <- data.frame(leuk_means - nol_means)
colnames(diff_means) <- c('Mean_Diffs')
rownames(diff_means) <- rownames(x)
diff_means$`H0 Rejection` <- fdr_genes

ggplot(diff_means, aes(x = Mean_Diffs, y = rownames(diff_means), fill = `H0 Rejection`)) + 
  geom_bar(stat="identity") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab("Mean Differences") + ylab("Genes") +
  ggtitle("Mean Diffences of Genes \n between Leukemia and Healthy Subjects") + 
  ggeasy::easy_center_title()
  
ggplot(diff_means, aes(y = Mean_Diffs, x = rownames(diff_means), colour = `H0 Rejection`)) + 
  geom_point() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  xlab("Genes") + ylab("Differences of Means") +
  ggtitle("Mean Diffences of Genes \n between Leukemia and Healthy Subjects") + 
  ggeasy::easy_center_title()

leuk_sd <- apply(leuk, MARGIN = 1, sd)
nol_sd <- apply(nol, MARGIN = 1, sd)
diff_sd <- data.frame(leuk_sd - nol_sd)
colnames(diff_sd) <- c('SD_Diffs')
rownames(diff_sd) <- rownames(x)
diff_sd$`H0 Rejection` <- fdr_genes


ggplot(diff_sd, aes(x = SD_Diffs, y = rownames(diff_sd), fill = `H0 Rejection`)) + 
  geom_bar(stat="identity") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab("SD Differences") + ylab("Genes") +
  ggtitle("SD Diffences of Genes \n between Leukemia and Healthy Subjects") + 
  ggeasy::easy_center_title()

ggplot(diff_sd, aes(y = SD_Diffs, x = rownames(diff_sd), colour = `H0 Rejection`)) + 
  geom_point() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  xlab("Genes") + ylab("Differences of SD") +
  ggtitle("SD Diffences of Genes \n between Leukemia and Healthy Subjects") + 
  ggeasy::easy_center_title()

cor(diff_means$Mean_Diffs, diff_means$`H0 Rejection`, method = c("pearson"))    # Moderate Negative Correlation
cor(diff_sd$SD_Diffs, diff_sd$`H0 Rejection`, method = c("pearson"))    # Weak Positive Correlation


r_pca_t    # PCA results of genes
contrib_pca <- get_pca_var(r_pca_t)$contrib
PCA1 <- data.frame(contrib_pca[,1])
colnames(PCA1) <- c('Contribution')
PCA1$`H0 Rejection` <- fdr_genes
head(PCA1)

A <- tapply(PCA1$Contribution, PCA1$`H0 Rejection`, summary)
a <- data.frame(Value =NA)
A

for(i in 1:2){
  for(j in 1:6){
    if(i == 1){
      a[j,1] <- A[[i]][j]
    } else {
      a[j+6,1] <- A[[i]][j]
    }
  }
}
rownames(a) <- c("Min.F","1st Qu.F","MedianF","MeanF","3rd Qu.F","Max.F","Min.T","1st Qu.T","MedianT","MeanT","3rd Qu.T","Max.T")
a$'H0 Rej' <- c(rep('False',6),rep('True',6))
ggplot(data=a, aes(x=rownames(a), y=Value, fill=`H0 Rej`)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label = ifelse(Value < 0.005, '<5e-3', round(Value,2))), vjust = -0.1, colour = 'black') +
  xlab("Summaries") + ylab("Values") +
  ggtitle("Summaries of Contribution per Gene for H0 Rejection") + 
  ggeasy::easy_center_title()

cor(PCA1$Contribution, PCA1$`H0 Rejection`, method = c("pearson"))    #Weak Negative Correlation


##### Exercise 2 #####

#### Data Creation ####

n<-500
p<-100
m <- 10000
p_val<-numeric(m)
gtruth<-numeric(m)
set.seed(123)
for (i in 1:m) {
  x <- matrix(rnorm(n*p),nrow = n, ncol = p)
  b <- numeric(p)
  if( runif(1) < 0.3){ b[1] <- rnorm(1) }
  y <- x%*%b + rnorm(n)
  f<-summary(lm(y~x))$fstatistic
  p_val[i]<-pf(f[1],f[2],f[3],lower.tail = FALSE)
  gtruth[i]<-b[1]
}

#### All Methods' Test Statistics ####
qStar <- 0.05
#bonferoni
adjustedP_bonf <- p.adjust(p = p_val, method = 'bonf')
bonfSelected2 <- adjustedP_bonf < qStar
tt2 <- table(as.logical(gtruth), bonfSelected2)
tt2
tt2[1,2]/sum(tt2[1,])

#BH
adjustedP_bh <- p.adjust(p = p_val, method = 'BH')
bhSelected <- adjustedP_bh < qStar
tt2<- table(as.logical(gtruth), bhSelected)
tt2
tt2[1,2]/sum(tt2[1,])

#holm
adjustedPholm <- p.adjust(p = p_val, method = 'holm')
bholmSelected <- adjustedPholm < qStar
tt2<- table(as.logical(gtruth), bholmSelected)
tt2
tt2[1,2]/sum(tt2[1,])

#hochberg
adjustedPhoch <- p.adjust(p = p_val, method = 'hochberg')
bSelectedhoch <- adjustedPhoch < qStar
tt2<- table(as.logical(gtruth), bSelectedhoch)
tt2
tt2[1,2]/sum(tt2[1,])

# hommel
adjustedPhomel <- p.adjust(p = p_val, method = 'hommel')
bhSelectedhomel <- adjustedPhomel < qStar
tt2<- table(as.logical(gtruth), bhSelectedhomel)
tt2
tt2[1,2]/sum(tt2[1,])

#BY
adjustedPby <- p.adjust(p = p_val, method = 'BY')
bhSelectedby <- adjustedPby < qStar
tt2<- table(as.logical(gtruth), bhSelectedby)
tt2
tt2[1,2]/sum(tt2[1,])

#qvalue
summary(qvalue(p_val)) 
bhSelectedq <- qvalue(p_val)$qvalues < qStar
tt2<- table(as.logical(gtruth), bhSelectedq)
tt2
tt2[1,2]/sum(tt2[1,])

#### Confution Matricies ####

alpha <- seq(0,1,length.out=m)
powerq<-rep(NA,m)
powerb<-rep(NA,m)
powerbh<-rep(NA,m)
powerbholm<-rep(NA,m)
powerbhochbe<-rep(NA,m)
powerbhomel<-rep(NA,m)
powerby<-rep(NA,m)

for (i in 1:m) {
  #qvalue
  bhSelectedq<- qvalue(p_val)$qvalues <alpha[i]
  powerq[i]<-(sum(bhSelectedq==TRUE & as.logical(gtruth) == TRUE))/(sum(bhSelectedq==TRUE&as.logical(gtruth)==TRUE)+sum(bhSelectedq!=TRUE&as.logical(gtruth)==TRUE))
  
  #bonferoni
  bonfSelected2 <- adjustedP_bonf < alpha[i]
  powerb[i]<-(sum(bonfSelected2==TRUE&as.logical(gtruth)==TRUE))/(sum(bonfSelected2==TRUE&as.logical(gtruth)==TRUE)+sum(bonfSelected2!=TRUE&as.logical(gtruth)==TRUE))
  
  #BH
  bhSelected<- adjustedP_bh < alpha[i]
  powerbh[i]<-(sum(bhSelected==TRUE&as.logical(gtruth)==TRUE))/(sum(bhSelected==TRUE&as.logical(gtruth)==TRUE)+sum(bhSelected!=TRUE&as.logical(gtruth)==TRUE))
  
  #holm
  bholmSelected <- adjustedPholm < alpha[i]
  powerbholm[i]<-(sum(bholmSelected==TRUE&as.logical(gtruth)==TRUE))/(sum(bholmSelected==TRUE&as.logical(gtruth)==TRUE)+sum(bholmSelected!=TRUE&as.logical(gtruth)==TRUE))
  
  #hochberg
  bSelectedhoch <- adjustedPhoch < alpha[i]
  powerbhochbe[i]<-(sum(bSelectedhoch==TRUE&as.logical(gtruth)==TRUE))/(sum(bSelectedhoch==TRUE&as.logical(gtruth)==TRUE)+sum(bSelectedhoch!=TRUE&as.logical(gtruth)==TRUE))
  
  #homel
  bhSelectedhomel <- adjustedPhomel < alpha[i]
  powerbhomel[i]<-(sum(bhSelectedhomel==TRUE&as.logical(gtruth)==TRUE))/(sum(bhSelectedhomel==TRUE&as.logical(gtruth)==TRUE)+sum(bhSelectedhomel!=TRUE&as.logical(gtruth)==TRUE))
  
  #BY
  bhSelectedby <- adjustedPby < alpha[i]
  powerby[i]<-(sum(bhSelectedby==TRUE&as.logical(gtruth)==TRUE))/(sum(bhSelectedby==TRUE&as.logical(gtruth)==TRUE)+sum(bhSelectedby!=TRUE&as.logical(gtruth)==TRUE))
}

#### Methods' Plot ####
plot(alpha, powerq, type = "l", col="darkred", ylab = "power", xlab = "type I error control value", main="Power per Method") 
lines(alpha,powerb,type = "l",col="blue")
lines(alpha,powerbh,type = "l",col="darkgreen")
lines(alpha,powerbholm,type = "l",col="darkorange")
lines(alpha,powerbhochbe,type = "l",col="black")
lines(alpha,powerbhomel,type = "l",col="darkviolet")
lines(alpha,powerby,type = "l",col="grey")
legend("bottomright",title = "Methods", legend = c("q-value","Bonferroni","BH","holm","hochberg","hommel","BY", title = c("Methods")),col = c("darkred", "blue","darkgreen","darkorange","black","darkviolet","grey"), lty = 1)