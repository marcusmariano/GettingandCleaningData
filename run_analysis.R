## Name: Marcus Ney Mariano
## Peer Assessments: Getting and Cleaning Data Course Project
## File: run_analysis.R

## ------------------------------------------------------------
##
## 
##
## ------------------------------------------------------------

## set the working directory

setwd("D:/Marcus/Projetos/R/03 - Getting and Cleaning Data/project")

## loading necessary library

library(tidyr)
library(dplyr)
library(data.table)
library(stringr)

## loading required dataset

namesAll <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE,
                                               header = FALSE)

label <- read.table("UCI HAR Dataset/activity_labels.txt", 
                       stringsAsFactors = FALSE, header = FALSE)

TrainX <- read.table("UCI HAR Dataset/train/X_train.txt", 
                       header = FALSE)

TrainY <- read.table("UCI HAR Dataset/train/y_train.txt", 
                     header = FALSE)

TestX <- read.table("UCI HAR Dataset/test/X_test.txt", 
                    header = FALSE)

TestY <- read.table("UCI HAR Dataset/test/y_test.txt", 
                    header = FALSE)

## Remove all special characters
namesAll$V2 <- str_replace_all(namesAll$V2, "[^[:alnum:]]", " ")

## remove all whitespace
namesAll$V2 <- str_replace_all(namesAll$V2, " ", "")

## add the names of columns TrainX
setnames(TrainX, old = names(TrainX), new= namesAll$V2)

## add the names of columns TestX
setnames(TestX, old = names(TestX), new= namesAll$V2)

## exclude duplicate columns of TrainX
train_x <- TrainX[ ,-c(303:344, 382:423)]

## exclude duplicate columns of TestY
test_x <- TestX[ ,-c(303:344, 382:423)]


## format the TrainX tidy dataset
train_x_tiny <-  train_x %>%
    select(contains("std"), contains("mean")) %>%
    select(-contains("angle"), -contains("meanFreq")) %>%
    mutate(id = TrainY$V1) %>%
    gather( signals, count, -id) %>%
    mutate(status = "train")

## format the TrainY tidy dataset 
test_x_tiny <-  test_x %>%
    select(contains("std"), contains("mean")) %>%
    select(-contains("angle"), -contains("meanFreq")) %>%
    mutate(id = TestY$V1) %>%
    gather( signals, count, -id) %>%
    mutate(status = "test")


## delete dataset 
rm(namesAll, label, TrainX, TrainY, TestX, TestY, train_x, test_x)


## merge two dataset Train and Test
dat = bind_rows(train_x_tiny, test_x_tiny)

## delete dataset 
rm(train_x_tiny, test_x_tiny)

## locate the activity labels
translator_function = function(element) {
    switch(element,
           '1' = "WALKING",
           '2' = "WALKING_UPSTAIRS",
           '3' = "WALKING_DOWNSTAIRS",
           '4' = "SITTING",
           '5' = "STANDING",
           '6' = "LAYING")
}

## list activity labels
activity_sapply = sapply(dat$id, translator_function)

## add activity labels
dat <- dat %>%
    mutate(activity = activity_sapply)

## calculate the mean and standard deviation for each measurement
temp <- tapply(dat$count, dat[ , c("activity", "signals")], mean)

## creat a data frame
data <- data.frame(temp)

## format a final result
data <- data %>%
    mutate( activity = c("LAYING", "SITTING", "STANDING", "WALKING", 
    "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS"), 
    id = c("6", "4", "5", "1", "3", "2")) %>%
    arrange(id) %>%
    select(id, activity, tBodyAccstdX:fBodyBodyGyroJerkMagmean)

## save a table with a name "data.txt"
write.table(data, "data.txt", row.name = FALSE)
