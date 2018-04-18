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
                                                                                  



# ggplot(mushroom,aes(x=StalkSurfaceAboveRing, y=CapSurface, color=Edible)) + 
#   geom_jitter(alpha=0.3) +
#   scale_color_manual(breaks = c('Edible','Poisonous'), values=c('darkgreen','red'))


#Comparisons of CapShape and CapSurface with Edible or Poisionous
ggplot(mushroom,aes(x=CapShape, y=CapSurface, color=Edible)) + 
  geom_jitter(alpha=0.3) +
  scale_color_manual(breaks = c('Edible','Poisonous'), values=c('darkgreen','red'))






##Preprocess dataset

#Remove missing values
table(complete.cases (mushroom))


#Modeling and Classification

#Divide dataset into training and testing 
mushroom$Edible <- as.factor(mushroom$Edible)

mushroomCopy <- mushroom



#Divide the datset into 70% training and 30% testing.
inTrain <- createDataPartition(y=mushroom$Edible, p=.7, list=FALSE)

#Assign indexes to split the Mushroom dataset into training and testing
training <- mushroom[inTrain,]
testing <- mushroom[-inTrain,]

ncol(training)

ncol(testing)

#Random forest classification 
library(randomForest)
library(doParallel) 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)


#Set initial trees value
ntrees <- 100
rf = randomForest(Edible ~ .,  
                  ntree = 100,
                  data = training)
plot(rf)  


set.seed(0)
df <- data.frame(ntrees =as.numeric(),
                 Accuracy=as.numeric())
system.time((for(i in 1:10){
  
RFModel = randomForest(Edible ~ .,  
                  ntree = ntrees,
                  data = training,
                  mtry = 10,
                  proximity = TRUE)

preds <- levels(training)[RFModel$test$predicted]

auc <- sum(preds == testing[,23]/nrow(testing))*100
df <- rbind(df, data.frame(NTrees = ntrees, Accuracy = auc))

ntrees <- ntrees + 100
}))
plot(rfModel)

varImpPlot(RFModel,main = 'Variable Importance')



  




for(i in 1:10){
  RFModel <- randomForest(
          training[,-23],
          testing[,-23],
          
          xtest = testing[,-23],
          ytest = testing[,23],
          ntree = ntrees,
          mtry = 10,
          proximity = TRUE
  )
  
  preds <- levels(training[,23])[RFModel$test$predicted]
  
  auc <- sum(preds == testing[,23]/nrow(testing))*100
  df <- rbind(df, data.frame(NTrees = ntrees, Accuracy = auc))
  
  ntrees <- ntrees + 100
}

print(RFModel)
# turn parallel processing off and run sequentially again:
registerDoSEQ()
# comparing the running time for each loop
system.time(x <- foreach(j=1:times ) %dopar% check(j))  #  2.56 seconds  (notice that the first run would be slower, because of R's lazy loading)
system.time(for(j in 1:times ) x <- check(j))  #  4.82 seconds

# stop workers
stopWorkers(workers)


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







