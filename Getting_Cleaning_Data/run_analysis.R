library(plyr)
library(reshape2)
library(gtools)

setwd("~/GitHub/Course-Projects/Getting_Cleaning_Data/UCI HAR Dataset")
### Read data into table objects
features <- read.table('./features.txt',header = FALSE)
activity <- read.table('./activity_labels.txt',header = FALSE)
subject_train <- read.table('./train/subject_train.txt', header = FALSE)
xTrain <- read.table('./train/x_train.txt', header = FALSE)
yTrain <- read.table('./train/y_train.txt', header = FALSE)
### Assign column names to the tables
colnames(activity) = c('activityID', 'activityType')
colnames(subject_train) = "subjectID"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityID"
### Merge the individual training table objects into training object
training_data <- cbind(yTrain, subject_train, xTrain)
### Read in Test data to table objects
subject_test <- read.table('./test/subject_test.txt', header = FALSE)
xTest<- read.table('./test/x_test.txt',header=FALSE)
yTest <- read.table('./test/y_test.txt', header = FALSE)
### Assign Column Names
colnames(subject_test) ="subjectID"
colnames(xTest) = features[,2]
colnames(yTest) = "activityID"
### Merge test data table objects
test_data <- cbind(yTest, subject_test, xTest)
### Merge training and test data table objects
uci_merge <- rbind(training_data, test_data)
### Create a vector of column names
col_names <- colnames(uci_merge)
### Assign a logical vector to a variable using the grepl function
logical_vector = (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) 
                  & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) 
                  & !grepl("-std()..-",col_names))
### Create a final data frame using desired values
uci_final = uci_merge[logical_vector == TRUE]
### Merge the descriptives to name activities
uci_final <- merge(uci_final,activity, by= 'activityID', all.x=TRUE)
