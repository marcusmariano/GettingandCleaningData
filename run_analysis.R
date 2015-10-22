## Name: Marcus Ney Mariano
## Peer Assessments: Getting and Cleaning Data Course Project
## File: run_analysis.R

## ------------------------------------------------------------
##
## 
##
## ------------------------------------------------------------

setwd("D:/Marcus/Projetos/R/03 - Getting and Cleaning Data/project")

library(tidyr)
library(dplyr)

library(data.table)

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

train_x <- TrainX[ ,-c(303:344, 382:423)]

test_x <- TestX[ ,-c(303:344, 382:423)]



train_x_tiny <-  train_x %>%
    select(contains("std"), contains("mean")) %>%
    select(-contains("angle"), -contains("meanFreq")) %>%
    mutate(lable = TrainY$V1) %>%
    gather( activ, count, -lable) %>%
    separate(col = activ, c("signals", "measure", "axial")) %>%
    mutate(status = "train")


test_x_tiny <-  test_x %>%
    select(contains("std"), contains("mean")) %>%
    select(-contains("angle"), -contains("meanFreq")) %>%
    mutate(lable = TestY$V1) %>%
    gather( activ, count, -lable) %>%
    separate(col = activ, c("signals", "measure", "axial")) %>%
    mutate(status = "test")

rm(namesAll, label, TrainX, TrainY, TestX, TestY, train_x, test_x)

dat = bind_rows(train_x_tiny, test_x_tiny)

translator_function = function(element) {
    switch(element,
           '1' = "WALKING",
           '2' = "WALKING_UPSTAIRS",
           '3' = "WALKING_DOWNSTAIRS",
           '4' = "SITTING",
           '5' = "STANDING",
           '6' = "LAYING")
}

activity_sapply = sapply(dat$lable, translator_function)

dat <- dat %>%
    mutate(activity = activity_sapply)


