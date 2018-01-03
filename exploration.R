## Coursera Data Science track, Johns Hopkins U
## Getting and Cleaning Data course project
## Author: Scott Loseke
## The purpose of this script is to explore the data, see what's there and attempt to piece it together. 

cat("\014")           ## Clear the Console
rm(list=ls())         ## Clear the data Environment
library(plyr)

## Explore the high level data

setwd('C:/Users/salosek/datasciencecoursera/course3project/UCI HAR Dataset')
getwd()
list.files(pattern = "*.txt")

features <- read.table("features.txt")
#head(features)
#tail(features)
str(features)

labels <- read.table("activity_labels.txt")
head(labels)
str(labels)

## Explore the test data

setwd('C:/Users/salosek/datasciencecoursera/course3project/UCI HAR Dataset/test')
list.files(pattern = "*.txt")

subject_test <- read.table("subject_test.txt")
head(subject_test)
str(subject_test)
count(subject_test$V1)

x_test <- read.table("X_test.txt")
str(x_test)

y_test <- read.table("y_test.txt")
head(y_test)
count(y_test$V1)

setwd('C:/Users/salosek/datasciencecoursera/course3project/UCI HAR Dataset/test/Inertial Signals')

testIS <- list.files(pattern = "*.txt")
tempfn <- testIS

for (i in 1:length(testIS)) { tempfn[i] <- sub(".txt","",tempfn[i])
assign(tempfn[i], read.table(testIS[i])) }
rm(testIS, tempfn, i)


## Explore the training data

setwd('C:/Users/salosek/datasciencecoursera/course3project/UCI HAR Dataset/train')
list.files(pattern = "*.txt")

subject_train <- read.table("subject_train.txt")
str(subject_train)
count(subject_train$V1)

x_train <- read.table("X_train.txt")
str(x_train)

y_train <- read.table("y_train.txt")
head(y_train)
count(y_train$V1)

setwd('C:/Users/salosek/datasciencecoursera/course3project/UCI HAR Dataset/train/Inertial Signals')

trainIS <- list.files(pattern = "*.txt")
tempfn <- trainIS

for (i in 1:length(trainIS)) { tempfn[i] <- sub(".txt","",tempfn[i])
                               assign(tempfn[i], read.table(trainIS[i])) }
rm(trainIS, tempfn, i)
     
     



