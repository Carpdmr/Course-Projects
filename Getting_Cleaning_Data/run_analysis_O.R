library(plyr)
library(reshape2)
library(gtools)

setwd("https://github.com/Carpdmr/Course-Projects/tree/master/Getting_Cleaning_Data/UCI%20HAR%20Dataset")
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
col_names <- colnames(uci_final)
### Assign more descriptive variable names
for (i in 1:length(col_names))
{
        col_names[i] = gsub("\\()","",col_names[i])
        col_names[i] = gsub("-std$", "StdDev", col_names[i])
        col_names[i] = gsub("-mean", "Mean", col_names[i])
        col_names[i] = gsub("^(t)", "Time", col_names[i])
        col_names[i] = gsub("^(f)", "Freq", col_names[i])
        col_names[i] = gsub("([Gg]ravity)","Gravity", col_names[i])
        col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", col_names[i])
        col_names[i] = gsub("[Gg]yro", "Gyro", col_names[i])
        col_names[i] = gsub("AccMag", "AccMagnitude", col_names[i])
        col_names[i] = gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", col_names[i])
        col_names[i] = gsub("JerkMag", "JerkMagnitude", col_names[i])
        col_names[i] = gsub("GyroMag", "GyroMagnitude", col_names[i])
}
### Assigning descriptive column names
colnames(uci_final) <- col_names
### Create a table with the average of each variable, activity & subject
uci_final_noact <- uci_final[,names(uci_final) !='activityType']
tidy_data <- aggregate(uci_final_noact[,names(uci_final_noact) != c('activityID','subjectID')]
                       ,by=list(activityID=uci_final_noact$activityID,
                                subjectID = uci_final_noact$subjectID),mean)
tidy_data <- merge(tidy_data,activity,by='activityID',all.x=TRUE)
# Prepare the final tidy data frame for distribution
write.table(tidy_data,'./tidy_data.txt', row.names=TRUE,sep='\t')

