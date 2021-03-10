

###########################################################################################################################
####################################################### MODELS ############################################################
###########################################################################################################################

library(readr)
library(car)
require(lmtest)
require(plm)
require(visreg)
require(dplyr)
require(psych)
require(ggplot2)
require(ggpubr)
require(methods)
require(caTools)
require(splines)
library(boot)
library(stargazer)
library(MASS)
library(klaR)
library(gbm)
library(caret)
library(gbm)
library(randomForest)
require(caret)
library(caret)
require(foreign)
require(nnet)
require(reshape2)
library(e1071)
library(factoextra)
library(NbClust)
library(ggfortify)
library(RColorBrewer)
library(gridExtra)
#install.packages('caret')
#install.packages('generics')



##########################################  CLASSIFICATION   #######################################

##### READ THE NEW DUMMIFIED DATAFRAME 
hospital_C <- read_csv("C:/Users/Sophie/Downloads/hospital_data_factors.csv")

#set target levels as factor 
hospital_C$LOS_days = as.factor(hospital_C$LOS_days)

#remove outliers for age
hospital_C = hospital_C[-c(20),]

#Remove index column and unary variable 
drop <- c("X1", 'insurance_Self Pay') #insurance self_pay is almost unary thus should be removed
hospital = hospital_C[,!(names(hospital_C) %in% drop)]
attach(hospital)



## For performance evaluation: Train and test data
require(caTools)
set.seed(101) 
sample = sample.split(hospital$los_days, SplitRatio = .70)
train_T = subset(hospital, sample == TRUE)
test_T  = subset(hospital, sample == FALSE)
write.csv(train_T, 'train_data.csv')
write.csv(test_T, 'test_data.csv')

train_dataset = read_csv("C:/Users/Sophie/Documents/train_data.csv")
test_dataset = read_csv("C:/Users/Sophie/Documents/train_data.csv")


####################################################################################################
####################################### Model 1: Logistic regression ###############################

## Logistic regression
multiLR = multinom(LOS_days ~ . , data = hospital)
summary(multiLR)

#Variable importance
var_MLR <- varImp(multiLR, scale = FALSE)


#summary of model in stargazer
stargazer(multiLR, type = "html", out = "multiLR.htm")
z <- summary(multiLR)$coefficients/summary(multiLR)$standard.errors
z



## EVALUATION OF THE MODEL 
#model
multinom_train = multinom(los_days~., data = train_dataset)

#predict new observations
pred = predict(object = multinom_train, newdata = test_dataset, type = "probs")

#Labels
labels = colnames(pred)[apply(pred, 1, which.max)]
result = data.frame(labels, test_dataset$los_days)

result$labels = as.factor(result$labels)
result$test_dataset.los_days = as.factor(result$test_dataset.los_days)

#Compare predicted values with the actual values in the test dataset
matrix = confusionMatrix(result$labels,result$test_dataset.los_days)
matrix


#calculate error
count = 0 
L = length(result$labels)
for (i in 1:L){
  if (result$labels[i] == result$test_dataset.los_days[i]){
    count = count + 0
  }
  else{
    count = count + 1
  }
}

error_rate = count/L
count
error_rate


#Feature importance 


##############################################################################################
####################################### Model 2: Random Forest ###############################

#RANDOM FOREST 
attach(hospital)
forest = randomForest(LOS_days~., data = hospital, ntree = 200)
forest
varImpPlot(forest, n.var = 5)


## EVALUATION OF THE MODEL 
set.seed(1234)
# Run the model
train_dataset$los_days = as.factor(train_dataset$los_days)

rf_default <- train(los_days~.,
                    data = train_dataset,
                    method = "rf",
                    metric = "Accuracy",
                    ntree = 200)
# Print the results
print(rf_default)
varImpPlot(rf_default, n.var =4)

#Evaluate the model
predicted_values = predict(rf_default, newdata = test_dataset)


#Labels
results_RF = data.frame(predicted_values, test_dataset$los_days)

results_RF$predicted_values = as.factor(results_RF$predicted_values)
results_RF$test_dataset.los_days = as.factor(results_RF$test_dataset.los_days)

#Confusion Matrix
Matrix_RF = confusionMatrix(results_RF$predicted_values, results_RF$test_dataset.los_days)
Matrix_RF


#calculate error
count = 0 
L = length(results_RF$predicted_values)
for (i in 1:L){
  if (results_RF$predicted_values[i] == results_RF$test_dataset.los_days[i]){
    count = count + 0
  }
  else{
    count = count + 1
  }
}

error_rate = count/L
count
error_rate



###############################################################################################
####################################### Model 3: Gradient Boost ###############################

#GRADIENT BOOST
#build Gradient Boost classification model
set.seed(1234)
boosted = gbm(los_days~., data = train_dataset, 
              distribution = "multinomial", 
              n.trees = 200, 
              interaction.depth = 7,
              shrinkage = 0.01,
              n.minobsinnode = 10, 
              cv.folds = 5)

summary(boosted)

#predict new values 
pred = predict.gbm(object = boosted, newdata = test_dataset, n.trees = 200, type = 'response')

# assign the label with the highest probability to the predicted value 
labels_GBM = colnames(pred)[apply(pred, 1, which.max)]

#create a new data frame to compare actual labels with test result
result_GBM = data.frame(labels_GBM, test_dataset$los_days)

result_GBM$labels = as.factor(result_GBM$labels)
result_GBM$test_dataset.los_days = as.factor(result_GBM$test_dataset.los_days)

#compare predicted values with actual values in the test dataset
matrix_GBM = confusionMatrix(result_GBM$labels, result_GBM$test_dataset.los_days)
matrix_GBM


#calculate error
count = 0 
L = length(result_GBM$labels)
for (i in 1:L){
  if (result_GBM$labels[i] == result_GBM$test_dataset.los_days[i]){
    count = count + 0
  }
  else{
    count = count + 1
  }
}

error_rate_GBM = count/L
count
error_rate_GBM



############## Hyperparameter tuning #############

#see what hyperparameters can be tuned
modelLookup('gbm')

#specify hyperparameter grid 
gb_grid = expand.grid(n.trees = c(100, 200, 500), #number of boosting iterations to build
                      interaction.depth = c(1,3,5,7), #max tree depth
                      shrinkage = c(0.1, 0.01, 0.001), #shrinkage parameter, smaller shrinkage requires more trees 
                      n.minobsinnode = c(10))  #use 10 as the default for most models  


#use 5 fold cross validation repeated 5 times
fitControl <- trainControl(method = 'repeatedcv', number = 5, repeats = 5) 

#train model using hyperparameter grid and 5-fold x5 cross validation to find optimal hyperparamters
tuned_gbm <-train(train[,c(1:48)], train_dataset$los_days,
                  method='gbm',
                  trControl=fitControl,
                  tuneGrid=gb_grid) 


###########################################################################################################
############################################################################################################
##################CLUSTERING########################## 

library(ggplot2)
library(factoextra)
library(NbClust)
library(ggpubr)
library(ggfortify)
library(RColorBrewer)
library(dplyr)
library(readr)
library(gridExtra)

hospital_data <- read_csv("C:/Users/Sophie/Downloads/hospital_data_factors.csv")
attach(hospital_data)

#remove outliers first 
hospital_data <- hospital_data[-c(3,20,32,34,1138,1690,837,301,65,29583, 31512,34533, 19673, 45458, 16593, 2986, 29432, 42772,  6734, 15871, 35476, 15871, 46920, 19673, 42772, 2986, 53905,28998,19673, 51941, 19673, 46766, 1266, 11440,  1266,  19673, 5581, 45744, 14880, 38305, 30427, 31484, 42560,19673, 20642),]

#filter data to top 5 emergency admissions first 
emergency_admissions <- hospital_data%>%
  filter(admit_type == 'EMERGENCY', admit_diagnosis == c('GASTROINTESTINAL BLEED','CORONARY ARTERY DISEASE','PNEUMONIA','SEPSIS','CONGESTIVE HEART FAILURE'))

#define variables used for clustering 
cluster_vars <- as.data.frame(emergency_admissions[,c(11:23)])
#don't include notes, chart events, and transfers 
cluster_vars <- cluster_vars[,c(2, 3, 5, 6, 7, 9, 10, 11)]

#normalize data
normalize <-function(x){
  return((x-min(x))/max((x)-min(x)))
}
cluster_vars <- normalize(cluster_vars)


#######################CLUSTER SELECTION#################################


#elbow method suggests 3 clusters is optial 
fviz_nbclust(cluster_vars, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") 

#silhouette method suggests 2 clusters is optimal 
fviz_nbclust(cluster_vars, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#NB clust compares 30 clustering indices and takes their frequency --  3 clusters is found to be optimal 
nbclust_out <- NbClust(
  data = cluster_vars,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "kmeans" 
)
# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 5 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 5)

# create plot of optimal number of clusters 
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") 
theme_minimal()

#######################K MEANS #################################

#create 3 clusters using K-Means
km.3 = kmeans(cluster_vars, centers = 3, nstart = 10)

cluster_vars$cluster_labels = as.factor(km.3$cluster)
attach(cluster_vars)

#######################PLOT CLSUTERS #################################

#add variables to plot by to cluster dataframe 
cluster_vars$LOS_days <- emergency_admissions$LOS_days
cluster_vars$admit_diagnosis <- factor(emergency_admissions$admit_diagnosis)
cluster_vars$expired <- factor(emergency_admissions$expired_hospital)
cluster_vars$admit_location <- emergency_admissions$admit_location
cluster_vars$age <- emergency_admissions$age

#rename variables for clustering 
cluster_vars$LOS_days[cluster_vars$LOS_days == 0] <- '0-5'
cluster_vars$LOS_days[cluster_vars$LOS_days == 1] <- '6-10'
cluster_vars$LOS_days[cluster_vars$LOS_days == 2] <- '10+'
cluster_vars$admit_location[cluster_vars$admit_location == 'CLINIC REFERRAL/PREMATURE'] <- 'CLINICAL REFERRAL'
cluster_vars$admit_location[cluster_vars$admit_location == 'PHYS REFERRAL/NORMAL DELI'] <- 'PHYSICIAN REFERRAL'
cluster_vars$admit_location[cluster_vars$admit_location == 'TRANSFER FROM HOSP/EXTRAM'] <- 'TRANSFER FRM HOSP'
cluster_vars$admit_location[cluster_vars$admit_location == 'TRANSFER FROM SKILLED NUR'] <- 'TRANSFER FRM NURS HOME'


#relative counts per cluster, important to note that the total number of observations in each cluster is quite uneven, 19, 1202, and 129
p1 <- ggplot(cluster_vars, aes(cluster_labels))+geom_bar(aes(fill = admit_diagnosis), position = 'fill')+labs(x = 'Cluster', y ='Relative Frequency', fill = 'Admit Diagnosis                           ')+ scale_fill_brewer(palette = "YlGnBu")
p2 <- ggplot(cluster_vars, aes(cluster_labels))+geom_bar(aes(fill = LOS_days), position = 'fill')+labs(x = 'Cluster', y ='Relative Frequency', fill = 'Length of Stay (Days)    ')+ scale_fill_brewer(palette = "YlGnBu")
p3 <- ggplot(cluster_vars, aes(cluster_labels))+geom_bar(aes(fill = expired), position = 'fill')+labs(x = 'Cluster', y ='Relative Frequency', fill = 'Deceased                     ')+ scale_fill_brewer(palette = "YlGnBu")
p4 <- ggplot(cluster_vars, aes(cluster_labels))+geom_bar(aes(fill = admit_location), position = 'fill')+labs(x = 'Cluster', y ='Relative Frequency', fill = 'Admit Location                            ')+ scale_fill_brewer(palette = "YlGnBu")

ggarrange(p2, p1, p3, p4, ncol =2, nrow = 2)

#######################PLOT PCA CLUSTERS#################################

#perform dimension reduction, and add individual coords
res.pca <- prcomp(cluster_vars[,c(1:8)], scale = TRUE)
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)

#add clusters, LOS, etc. to PCA
ind.coord$cluster <- factor(km.3$cluster)
ind.coord$LOS_days <- factor(cluster_vars$LOS_days)
ind.coord$admit_diagnosis <- factor(emergency_admissions$admit_diagnosis)
ind.coord$ethnicity <- factor(emergency_admissions$ethnicity)
ind.coord$insurance <- factor(emergency_admissions$insurance)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

#Plot clusters
ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster",   palette = c("#253494","#41B6C4" ,"#7FCDBB"), ellipse = TRUE, ellipse.type = "convex",
  size = 'LOS_days', legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dimension 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dimension 2 (", variance.percent[2], "% )" )
)+labs(size = 'Length of Stay (Days)')
