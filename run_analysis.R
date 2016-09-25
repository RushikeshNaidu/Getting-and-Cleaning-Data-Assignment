# Coursera Getting and CLeaning Data Assignment 
# by Rushikesh Naidu

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Clear the workspace
rm(list=ls())
# Set the working directory to the location where UCI HAR Dataset was unizpped
setwd("F:/Samsung Data")
# 1. Merge the training and the test sets to create one data set.

# Read in the data from files test and train
x.train <- read.table("F:/Samsung Data/train/X_train.txt")
y.train <- read.table("F:/Samsung Data/train/y_train.txt")
subject.train <- read.table("F:/Samsung Data/train/subject_train.txt")
x.test <- read.table("F:/Samsung Data/test/X_test.txt")
y.test <- read.table("F:/Samsung Data/test/y_test.txt")
subject.test <- read.table("F:/Samsung Data/test/subject_test.txt")

# Read in the general data from main file
features <- read.table("F:/Samsung Data/features.txt")
activityL <- read.table("F:/Samsung Data/activity_labels.txt")

# Assign Column names to the data imported
colnames(x.train) <- features [,2]
colnames(y.train) <- "activityID"
colnames(subject.train) <- "SubjectID"
colnames(x.test) <- features[,2]
colnames(y.test) <- "activityID"
colnames(subject.test) <- "SubjectID"
colnames(activityL) <- c('activityID','activitytype')

# Creating the final test set by merging 
merge.test <- cbind(x.train,subject.train,y.train)
# creating the final train set by merging
merge.train <- cbind(x.test,subject.test,y.test)

# Final Result of merging test and train datasets into one data set
merge.test_and_train <- rbind(merge.train , merge.test)

# Creating a vector containing all the column names which will be used to select the desired mean and standard deviation
column.names=colnames(merge.test_and_train)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
mean.and.std <- (grepl("activityID",column.names)|grepl("SubjectID",column.names)|grepl("mean..",column.names)|grepl("std..",column.names))

#Final Result
mean.and.std.set <- merge.test_and_train[,mean.and.std == TRUE]

# 3. Use descriptive activity names to name the activities in the data set
# Final Result : Merging the obtaned data with activityL table to obtain the descriptive activity names
sorted.by.activity.names <- merge(mean.and.std.set,activityL, by= "activityID" , all.x = TRUE)

# 4. Appropriately label the data set with descriptive activity names. 
#Generate a vector containing all column names of the above table
colNames  = colnames(sorted.by.activity.names)
# Select appropriate variable names by using gsub and clean the data
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
# Reassign the columnnames back into the previous table 
colnames(sorted.by.activity.names)=colNames

5.# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#Generate a new table without the activitytype column
finalDataNoActivityType  <- sorted.by.activity.names[,names(sorted.by.activity.names) != 'activitytype'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityID','SubjectID')],by=list(activityID=finalDataNoActivityType$activityID,SubjectID = finalDataNoActivityType$SubjectID),mean)
# Merging the tidyData with activityL to include descriptive acitvity names
tidyData    = merge(tidyData,activityL,by='activityID',all.x=TRUE)
# Export the tidyData set 

write.table(tidyData, "tidyData.txt", row.name=FALSE)
