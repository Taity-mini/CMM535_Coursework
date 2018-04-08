#Import packages
library(randomForest)  
library(e1071)  
library(caret)  
library(ggplot2) 


#Import Dataset
#Feature names
colNames <- c("class", "cap-shape", "cap-surface",
                 "cap-color", "bruises?", 
                 "odor",
                 "grill-attachment",
                 "grill-spacing",
                 "grill-size",
                 "grill-color",
                 "stalk-shape",
                 "stalk-root",
                 "stalk-surface-above-ring",
                 "stalk-surface-below-ring",
                 "stalk-color-above-ring",
                 "stalk-color-below-ring",
                 "veil-type",
                 "veil-color",
                 "ring-number",
                 "ring-type",
                 "spore-print-color",
                 "population",
                 "habitat")
#Import datasets

mushroom <- read.table("data/agaricus-lepiota.data" ,header = FALSE, sep = ",",
                         strip.white = TRUE, col.names = colNames,
                         na.strings = "?")
summary(mushroom)
str(mushroom)


#Preprocess dataset
