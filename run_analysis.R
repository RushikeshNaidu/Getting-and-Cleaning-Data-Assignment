x.train <- read.table("F:/Samsung Data/train/X_train.txt")
y.train <- read.table("F:/Samsung Data/train/y_train.txt")
subject.train <- read.table("F:/Samsung Data/train/subject_train.txt")
x.test <- read.table("F:/Samsung Data/test/X_test.txt")
y.test <- read.table("F:/Samsung Data/test/y_test.txt")
subject.test <- read.table("F:/Samsung Data/test/subject_test.txt")
features <- read.table("F:/Samsung Data/features.txt")
activityL <- read.table("F:/Samsung Data/activity_labels.txt")

colnames(x.train) <- features [,2]
colnames(y.train) <- "activityID"
colnames(subject.train) <- "SubjectID"
colnames(x.test) <- features[,2]
colnames(y.test) <- "activityID"
colnames(subject.test) <- "SubjectID"
colnames(activityL) <- c('activityID','activitytype') 
merge.test <- cbind(x.train,subject.train,y.train)
merge.train <- cbind(x.test,subject.test,y.test)
merge.test_and_train <- rbind(merge.train , merge.test)
column.names=colnames(merge.test_and_train)
mean.and.std <- (grepl("activityID",column.names)|grepl("SubjectID",column.names)|grepl("mean..",column.names)|grepl("std..",column.names))
mean.and.std.set <- merge.test_and_train[,mean.and.std == TRUE]
sorted.by.activity.names <- merge(mean.and.std.set,activityL, by= "activityID" , all.x = TRUE)
colNames  = colnames(sorted.by.activity.names)
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
colnames(sorted.by.activity.names)=colNames
finalDataNoActivityType  <- sorted.by.activity.names[,names(sorted.by.activity.names) != 'activitytype'];
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityID','SubjectID')],by=list(activityID=finalDataNoActivityType$activityID,SubjectID = finalDataNoActivityType$SubjectID),mean)
tidyData    = merge(tidyData,activityL,by='activityID',all.x=TRUE)
