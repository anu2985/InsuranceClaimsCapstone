setwd("/Users/anupama/Documents/Anupama Data Science/GreatLakes/Capstone")
getwd()

toload_libraries <- c("reshape2", "rpsychi", "car", "psych", "corrplot", "forecast", "GPArotation", "psy", "MVN", "DataExplorer", "ppcor", "Metrics", "foreign", "MASS", "lattice", "nortest", "Hmisc","factoextra", "nFactors")
#new.packages <- toload_libraries[!(toload_libraries %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
lapply(toload_libraries, require, character.only= TRUE)
library(funModeling)
library(Hmisc)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(PerformanceAnalytics)
library(nFactors)
library(psych)
library(flextable)
library(officer)
library(GGally)
library(caTools)
library(data.table)
library(ROCR)
library(class)
library(gmodels)
library(naivebayes)
library(gridExtra)
library(caret)
library(dplyr)
library(class)
library(gmodels)
#library(datasets)

load("groupdf.RData")

str(df)

#KNN data creation
#Setting the numeric variables to scales
#Dummy code Factor w 2 levels
#Dummy Code Factor w more than 3 levels

### KNN Model

#The K nearest Neighbours Model(KNN) measures the Euclidean distance between points close and far in the neighborhood, Hence two things are required before running the model : Scaling or Standardizing the variables to mean of zero and variance of 1, secondly, dummy coding all factor levels as separate columns 
#*Scaling* is an important aspect for running a KNN model else the model would bias itself to higher numeric values. As part of the data preparation for the model, we will do the following:
#  1. Scale the numeric variables - Vehicle Age, Sum Insured (Num_IDV), Claim Amount(DRV_CLAIM_AMT), OD Premium (Net_Num_OD_Premium)
#  2. Dummy code all the variables that are factor to two or more levels in different columns setting as each factor as 1 and 0- Policy Code, Zone Code, Class Code, Permit Code, NAture of Good Code, Road Type,  Vehicle Driven, Driver Experience, Claim History and Boo_Antitheft, Claim and Policy year etc.


#Scaling is an important aspect for running a KNN model else the model would bias itself to higher numeric values.

knn.df= df


#Scaling of Numeric Variables
knn.df[,c("Num_Vehicle_Age", "Num_IDV", "DRV_CLAIM_AMT", "Num_Net_OD_Premium",
          "PaymentDays", "IntimationDays")] = scale(knn.df[,c("Num_Vehicle_Age", "Num_IDV", "DRV_CLAIM_AMT", "Num_Net_OD_Premium", "PaymentDays", "IntimationDays")])

str(knn.df)

#Factor variables - Dummy code - More than 2 levels
#install.packages("fastDummies")
results <- fastDummies::dummy_cols(knn.df)


#removee knn.df and df
rm(df,knn.df)


colnames(results)
results = results[,-c(1:5,8:17,19:20,23,25:32,197:198)] #removed all original variables and kept the dummy coded variables for all factors

results= results[,c(1:6,8:170,7)] #readjusting the status variable as the last!



nearZeroVar(results1) #removing the zero variance columns


results1= results[,-nearZeroVar(results)]



#Training and Testing of the data
#Spliting data as training and test set. Using createDataPartition() function from caret

indx<- createDataPartition(y = results1$status,p = 0.70,list = FALSE)

trainingKNN <- results1[indx,]
testingKNN<- results1[-indx,]

#Checking distribution in original data and partitioned data
prop.table(table(trainingKNN$status)) * 100

prop.table(table(testingKNN$status)) * 100

prop.table(table(results1$status)) * 100

trainingKNN.num= trainingKNN[,c(1:6,95)]

library(DMwR)
library(grid)
## SMOTE technique
train.KNN.smote<-SMOTE(status~., trainingKNN.num, perc.over = 250,perc.under = 150) #with this, there will be an equal split of 0.5 for both classes - 0 and 1
prop.table(table(train.KNN.smote$status))



set.seed(400)
KNNctrl <- trainControl(method="repeatedcv",repeats = 3)#classProbs=TRUE,summaryFunction = twoClassSummary)

knnFit <- train(status ~ ., data = gd.train.smote, 
                method = "knn", 
                trControl = KNNctrl, 
                preProcess = c("center","scale"), 
                tuneLength = 20)
#Output of kNN fit
knnFit

plot(knnFit, main= "Accuracy with balanced sample")
plot(knnFit, print.thres = 0.5, type="S")


#As per the model; accuracy of the model is highest when k= 5.

knnPredict <- data.frame( actual= testingKNN$status,
                         predict(knnFit, newdata = testingKNN[c(1:6,95)], type="prob"))
head(knnPredict,10)



knnPredict$pred= ifelse(knnPredict$X1> 0.5, 1, 0)


knnPredict.train <- data.frame( actual= trainingKNN$status,
                          predict(knnFit, newdata = trainingKNN[c(1:6,95)], type="prob"))

knnPredict.train$pred= ifelse(knnPredict.train$X1> 0.5, 1, 0)
#Get the confusion matrix to see accuracy value and other parameter values

confusionMatrix(table(knnPredict.train$pred, trainingKNN$status), positive = "1")
confusionMatrix(table(knnPredict$pred, testingKNN$status), positive = "1")



knnROC <- roc(testingKNN$status,knnPredict[,"Down"], levels = rev(testingKNN$status))
knnROC

Knn.ROC <- roc(predictor=knnPredict$X1, 
                    response=testingKNN$status,
                    levels=rev(levels(testingKNN$status)))

plot(Knn.ROC)


#Running the other KNN Models using e1071 package

#Random splitting of iris data as 70% train and 30%test datasets
#set.seed(123)
#ind <- sample(2, nrow(celldataKNN), replace=TRUE, prob=c(0.7, 0.3))
#trainKNN <- celldataKNN[ind==1,]
#testKNN <- celldataKNN[ind==2,]
#checking the dimensions of train and test datasets
#dim(trainKNN)
#dim(testKNN)

#removing Target variable from training and test datasets
trainKNN1 <- trainingKNN[,-95]
testKNN1<- testingKNN[,-95]


#storing target variable for testing and training data as factor
cell_train_labels <- as.factor(trainingKNN$status) 
dim(cell_train_labels)
str(cell_train_labels)
cell_test_labels <- as.factor(testingKNN$status)
dim(cell_train_labels)

#KNN Model building
KNN_test_pred <- knn(train = trainKNN1, 
                     test = testKNN1, 
                     cl= cell_train_labels,
                     k = 3,
                     prob=TRUE)


# library(gmodels)

CrossTable(x = cell_test_labels, y = KNN_test_pred,prop.chisq=FALSE, 
           prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)
knn_tab <- table(KNN_test_pred,cell_test_labels)
knn_tab
1 - sum(diag(knn_tab)) / sum(knn_tab)   ## Error
# Error when k=3 = 0.0400 : 0.9599 #sensitivity 0.7193 #specificity 0.9663 Pos Pred Value : 0.36403 
# Error when k=5 = 0.0385: 0.9614  sensitivity 0.79435 specificity 0.9651 Pos Pred Value : 0.33907
# Error when k=7 = 9.4% : 90.5%  #sensitivity 0.39 specificity 0.992
# Error when k=9 = 10.1%% : 89.8% #sensitivity 0.35 specificity 0.989
# Error when k=21 = 10.7%% :89.2% #sensitivity 0.28 specificity 0.990
#install.packages("e1071")
library(e1071)

#confusionMatrix(table(cell_test_labels,KNN_test_pred), positive = "1")


#rm(results, results1,testingKNN, trainingKNN)


#KNN Model Performance

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}


calc_class_err(actual    = cell_test_labels,
               predicted = KNN_test_pred) #10.98 Error rate

set.seed(42)
k_to_try = 1:11
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = knn(train = trainKNN1, 
             test = testKNN1, 
             cl= cell_train_labels, 
             k     = k_to_try[i])
  err_k[i] = calc_class_err(cell_test_labels, pred)
}

# plot error vs choice of k
plot(err_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number of neighbors", ylab = "classification error",
     main = "(Test) Error Rate vs Neighbors")
# add line for min error seen
abline(h = min(err_k), col = "darkorange", lty = 3)
# add line for minority prevalence in test set
abline(h = round(mean(cell_test_labels == "1"),digits = 2), col = "black", lty = 3)
#the orange dotted line is the minimum error line which represent the smallest observed test classification error rate. As the number of K increases the error rate increases to such the the error rate approaches the minority class prevalence in actual.
min(err_k)




seed=1000
set.seed(seed) #since kmeans uses a randomized starting point for cluster centroids

clust2 = kmeans(x=custSpendData.Scaled, centers = 2, nstart = 5)
print(clust2)
