# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#1. Load files
#features
features <- read.table("UCI HAR Dataset/features.txt", header=FALSE, colClasses = "character")
#features that contain mean or std
mean.std.vector<-grepl('mean\\(\\)|std\\(\\)', features[,"V2"])
#activities
activity <- read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE, colClasses = "character")


#test
test_X <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE)
test_Y <- read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE)
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE)

#train
train_X <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
train_Y <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE)
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)

# 2. Combine subject, activity and features data into a test and trainning data framecombine all cololumns in one file
test_data <- data.frame(test_X, test_subject, test_Y, stringsAsFactors=FALSE)
train_data <- data.frame(train_X, train_subject, train_Y, stringsAsFactors=FALSE)

# 3. Merges the training and the test sets to create one data set.
combined_data <- rbind(test_data, train_data)

# 4. remove unwnated columns keeping mean and stds ones

size <- ncol(combined_data)
tidyDataSet <- data.frame(subject= combined_data[,562], activity = combined_data[,563])

# 5. Prepere column names
vecColNames <- rep(NA,sum(mean.std.vector, na.rm=TRUE))
for(x in 1:(size-2))
{
  if(mean.std.vector[x])
  {
    # 6. Extracts only the measurements on the mean and standard deviation for each measurement.
    tidyDataSet <- cbind(tidyDataSet, name  = combined_data[,x])
    vecColNames[x] <- features[x,"V2"]
  }
}
vecColNames <- vecColNames[!is.na(vecColNames)]
# 7. Appropriately labels the data set with descriptive variable names.
# 8. Assign col names
colnames(tidyDataSet) <- c("subject", "activity", vecColNames)

# 9. Uses descriptive activity names to name the activities in the data set
for (x in 1:nrow(tidyDataSet))
{
  
  tidyDataSet[x,2] <- activity[tidyDataSet[x,2],2]
}

# 10. Exports first tidy data set to file system
write.table(tidyDataSet, "summTidyDataSet1.txt", sep="\t")

# 11. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
summTidyDataSet <- subset(tidyDataSet, FALSE)
size <- ncol(tidyDataSet)
rows <- 1
for (subject in unique(tidyDataSet[,"subject"]))
{  
  for (activity in unique(tidyDataSet[,"activity"]))
  {
    for (feature in 3:(size-2))
    {
      featureMean <- mean(tidyDataSet[,feature] [tidyDataSet[,2] == activity & tidyDataSet[1] == subject]) 
      summTidyDataSet[rows,"subject"] <- subject
      summTidyDataSet[rows,"activity"] <- activity
      summTidyDataSet[rows,feature] <- featureMean
    }
    rows <- rows + 1
  }  
}
#Exports second tidy data set to file system with the average of each variable for each activity and each subject.
write.table(summTidyDataSet, "summTidyDataSet2-SumAvg.txt", sep="\t")

