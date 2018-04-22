#Import packages
library(randomForest)  
library(e1071)  
library(caret)  
library(ggplot2) 
library(gridExtra)
library(caret)
library(rpart.plot)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(doParallel)
library(xtable)

#helper function
source('helper_functions.r')

#Import datasets using helper function

mushroom <- fetchAndCleanData()

#Data exploration


#Number of rows in the dataset
nrow(mushroom)
#Number of columns in the dataset
ncol(mushroom)

#Summary of the dataset
summary(mushroom)

str(mushroom)

#Inspecting the dataset


#Class Distribution
barplot(table(mushroom$Edible))


#Comparisons of CapShape and CapSurface with Edible or Poisionous
ggplot(mushroom,aes(x=CapShape, y=CapSurface, color=Edible)) + 
                       geom_jitter(alpha=0.3) +
                       scale_color_manual(breaks = c('Edible','Poisonous'), values=c('darkgreen','red'))
                                                                                  


#Comparisons of StalkSurfaceAboveRing and StalkSurfaceBelowRing with Edible or Poisionous
ggplot(mushroom,aes(x=StalkSurfaceAboveRing, y=StalkSurfaceBelowRing, color=Edible)) + 
  geom_jitter(alpha=0.3) +
  scale_color_manual(breaks = c('Edible','Poisonous'), values=c('darkgreen','red'))




##Preprocess dataset

#Remove missing values
table(complete.cases (mushroom))


#Modeling and Classification


#Divide the datset into 70% training and 30% testing.
inTrain <- createDataPartition(y=mushroom$Edible, p=0.6, list=FALSE)

#Assign indexes to split the Mushroom dataset into training and testing
training <- mushroom[inTrain,]
testing <- mushroom[-inTrain,]



#Classification Models using caret

#Setup Parallel processing to speed up classification modelling 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

#set train control to cross-validation with 5 folds
train_control<- trainControl(method="cv", number=10,verboseIter=FALSE)

#First set the seed for reproducibility
set.seed(1)

#train model using kNN
kNNModel <- train(Edible ~ ., data = training, 
                  trControl = train_control, 
                  tuneLength =10,
                  method = "knn",
                  metric = 'Accuracy'
)

#Show the kNN model results
kNNModel


#Predict the accuracy of the kNN Model against the testing set
predictkNN <- predict(kNNModel,testing)
confusionMatrix(predictkNN, testing$Edible)



#First set the seed for reproducibility
set.seed(1)

# train the model using random forest
RFModel<- train(Edible~., data=training,
                trControl=train_control,
                method="rf",
                tuneLength =10,
                metric = 'Accuracy'
)
RFModel

predictRF <- predict(RFModel,testing)
confusionMatrix(predictRF, testing$Edible)


#First set the seed for reproducibility
set.seed(1)

#train the model using c5.0
c50Model<- train(Edible~., data=training,
                 trControl=train_control,
                 tuneLength = 10,
                 method="C5.0",
                 metric = 'Accuracy'
)

c50Model
predictC50 <- predict(c50Model, testing)
confusionMatrix(predictC50,testing$Edible)



# make predictions
predictions<- predict(mymodel$finalModel,testing[,-ncol(testing)])
# append predictions (just for manual analysis)
test<- cbind(testing,predictions)
# summarize results
results<- confusionMatrix(test$predictions,test$Edible)


results

varImpPlot(RFModel$finalModel,main = 'Variable Importance')


actual <- clusteredDF$cluster




# turn parallel processing off and run sequentially again:
registerDoSEQ()


rs <- resamples(list(kNN = kNNModel, 
                     c50 = c50Model, 
                     rf = RFModel))
summary(rs)
bwplot(rs, layout = c(4, 1))


# Clustering dataset


# #Copy dataset
# noClass <-mushroom
# #Remove class as it is not being transformed to binary
# noClass$Edible <- NULL
# 
# binaryVars <- caret::dummyVars(~ ., data = noClass)
# newMushroom <- predict(binaryVars, newdata = noClass)
# 
# #add class to binarised dataset
# binMushroom <-cbind(newMushroom, mushroom[1])
# str(binMushroom)
# 
# summary(binMushroom)


# df <- mushroom
# 
# dfN <- as.data.frame(lapply(newMushroom[,], normalizeData) )
# # add the label
# dfN$Species <- df$Species
# clusteredDF <- clustData(dfN,ncol(df), c(2,2,2))
# 
# clusteredDF <- clustData(mushroom ,ncol(mushroom)-1, c(2,2,2))
# 
# str(df)



normalizeData <- function (x ) {
return ( (x-min(x) ) / ( max(x)- min(x) ))
}

str(mushroom)


dfNew <- mushroom
dfNew$VeilType <- NULL

dfNew[,2:22] = lapply(dfNew[,2:22], as.numeric)


dfN <- as.data.frame(lapply(dfNew[,-1], normalizeData))


str(dfN)
dfN$Edible <- dfNew$Edible

table(complete.cases (dfN))


clusteredDF <- clustData(dfN,ncol(dfNew), c(2,2))

summary(clusteredDF$cluster)

autoplot(clusteredDF)

head(clusteredDF,20)
table(clusteredDF$cluster)




#Adapting the model

#Divide the datset into 60% training and 40% testing. To avoid overfitting
inTrainCluster <- createDataPartition(y=clusteredDF$cluster, p=0.6, list=FALSE)


#Assign indexes to split the Mushroom dataset into training and testing
trainingCluster <- clusteredDF[inTrainCluster,]
testingCluster <- clusteredDF[-inTrainCluster,]

#set train control to cross-validation with 5 folds
train_controlCluster<- trainControl(method="cv", number=10,verboseIter=FALSE)


#First set the seed for reproducibility
set.seed(1)

# train the model using random forest
RFModelCluster<- train(cluster~., data=trainingCluster,
                trControl=train_controlCluster,
                method="rf",
                tuneLength =10,
                metric = 'Accuracy'
)

#Show Random forest model
RFModelCluster

#Predict the accuracy and display using a confusion matrix
predictRFCluster <- predict(RFModelCluster,testingCluster)
confusionMatrix(predictRFCluster, testingCluster$cluster)


#Results


#Split the actual results
actual <- testingCluster$cluster

str(actual)

#Split the predicted Results
pred <- predictRFCluster
str(pred)


#Combine the actual and predicted results into a dataframe.
cols = data.frame("Actual" = actual, "Predicted" = pred)


#Convert the both the actual and predict results to characters
cols$Actual <- as.character(cols$Actual)
cols$Predicted <- as.character(cols$Predicted)

str(cols)

#Loop for all the rows in cols dataframe
for(row in 1:nrow(cols)){
  
  #split actual/pred again within for loop
  actualRow <- cols$Actual[row]
  predRow <- cols$Predicted[row]
  
  #Remove the last three characters in the actual/predicted results 
  # then check if they are the same
  results[row] <- substr(actualRow, 0,nchar(actualRow)-3)== substr(predRow,0, nchar(predRow)-3)
  
  #If results are the same(TRUE) then set 'Yes' otherwise then 'No'
  if(results[row] == TRUE){
    results[row] <- 'Yes'
  } else{
    results[row] <- 'No'
  }
}

#Bind the actual, predicted and correct results together.
finalResults = cbind(cols, "Correct" =results)

#Head of the final results
xtable(head(finalResults,10))

#Tail of the final results
xtable(tail(finalResults,10))




#Clustering Function
clustData <- function (df,ClassIndex,kmeansClasses = rep(0,unique(df[,ClassIndex]))) {
  # use split function to split the dataset according to the class label
  # a set of dataframes each representing a class label will be stored
  # in dfs list()
  dfs <- split (df, df[,ClassIndex])
  # create empty list
  clustList <- list()
  n <- length(dfs)
  for (i in 1:length(kmeansClasses)){
    # Cluster according to all features excluding the label
    if (kmeansClasses[i]>1 & kmeansClasses[i]< nrow(dfs[[i]])){
      clustList[[i]] <- kmeans(dfs[[i]][,-ClassIndex],kmeansClasses[i])
      #plotcluster(clustList[[i]], clustList[[i]]$cluster)
      dfs[[i]]$cluster <- paste0((dfs[[i]][,ClassIndex]),
                                 "_","c",clustList[[i]]$cluster)
    }
    else {
      dfs[[i]]$cluster = paste0((dfs[[i]][,ClassIndex]),
                                "_c0")
    }
  }
  # put all list elements in a dataframe and return it
  # note that ldply() require the library plyr
  allClusteredElements <- ldply (dfs, data.frame)
  # drop the first column 'id' resulting from ldply
  allClusteredElements <- allClusteredElements[,-1]
  allClusteredElements <- allClusteredElements[,-ClassIndex]
  return(allClusteredElements)
}





