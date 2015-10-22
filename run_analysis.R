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
    gather( activ, count, -id) %>%
    separate(col = activ, c("signals", "measure", "axial")) %>%
    mutate(status = "train")

## format the TrainY tidy dataset 
test_x_tiny <-  test_x %>%
    select(contains("std"), contains("mean")) %>%
    select(-contains("angle"), -contains("meanFreq")) %>%
    mutate(id = TestY$V1) %>%
    gather( activ, count, -id) %>%
    separate(col = activ, c("signals", "measure", "axial")) %>%
    mutate(status = "test")


## delete dataset 
rm(namesAll, label, TrainX, TrainY, TestX, TestY, train_x, test_x)
rm(train_x_tiny, test_x_tiny)

## merge two dataset Train and Test
dat = bind_rows(train_x_tiny, test_x_tiny)

## 
translator_function = function(element) {
    switch(element,
           '1' = "WALKING",
           '2' = "WALKING_UPSTAIRS",
           '3' = "WALKING_DOWNSTAIRS",
           '4' = "SITTING",
           '5' = "STANDING",
           '6' = "LAYING")
}

## 
activity_sapply = sapply(dat$id, translator_function)

## 
dat <- dat %>%
    mutate(activity = activity_sapply)

##
temp <- tapply(dat$count, dat[ , c("activity", "signals")], mean)

##
data <- data.frame(temp)

##
data <- data %>%
    mutate( activity = c("LAYING", "SITTING", "STANDING", "WALKING", 
    "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS"), 
    id = c("6", "4", "5", "1", "3", "2")) %>%
    arrange(id)
