library(dplyr)
library(plyr)

# Read data
test = read.table("./UCI HAR Dataset/test/X_test.txt", colClasses="numeric")
train = read.table("./UCI HAR Dataset/train/X_train.txt", colClasses="numeric")
test_activityID = read.table("./UCI HAR Dataset/test/y_test.txt", colClasses="numeric")
train_activityID = read.table("./UCI HAR Dataset/train/y_train.txt", colClasses="numeric")
test_subjectID = read.table("./UCI HAR Dataset/test/subject_test.txt", colClasses="numeric")
train_subjectID = read.table("./UCI HAR Dataset/train/subject_train.txt", colClasses="numeric")
activityLabel = read.table("./UCI HAR Dataset/activity_labels.txt")
featuresLabel = read.table("./UCI HAR Dataset/features.txt")

#4----Label the variables with descriptive names
names(test) = as.vector(featuresLabel[,2])
names(train) = as.vector(featuresLabel[,2])

    # Add activityID and subjectID to both data sets.
test$activityID = as.vector(test_activityID[,1])
test$subjectID = as.vector(test_subjectID$V1)
train$activityID = as.vector(train_activityID[,1])
train$subjectID = as.vector(train_subjectID$V1)

#1----Merge train and test together -> data
data = rbind(train,test)

#2----Extract mean and standard deviation -> mean_sd
mean_sd_names = names(data)[grep("mean|std|subjectID|activityID", names(data))]
mean_sd = data[,mean_sd_names]

#3----Uses descriptive acticity names -> newData
newData = merge(mean_sd, activityLabel, by.x="activityID", by.y="V1", all.x=T)
names(newData)[82] = "activity"

#5----A tidy data set with only average of each activity and each subject -> new_data
    # Order the data by subjectID & activityID
pre_tidy_data = arrange(newData, subjectID, activityID)
pre_tidy_data$activity = NULL

    # Split by subjectID & activityID, calculate colMeans by lapply loop.
split = split(pre_tidy_data, list(pre_tidy_data$subjectID, pre_tidy_data$activityID))
newlist = lapply(split, colMeans)
    
    # Create a new data frame, by rbind each element in the newlist.
tidy_data = rbind.data.frame(newlist[[1]],newlist[[2]])
for (i in 3:180){
    tidy_data = rbind.data.frame(tidy_data, newlist[[i]])
}
names(tidy_data) = names(pre_tidy_data)

    # Change the position of subjectID to the second column.
new_data = as.data.frame(append(tidy_data, list(tidy_data$subjectID), after=1))
new_data$subjectID = NULL
names(new_data)[2] = "subjectID"

    # Change the activityID into descriptive activity name.
new_data = merge(new_data, activityLabel, by.x="activityID", by.y="V1", all.x=T)
new_data$activityID = new_data$V2
new_data$V2 = NULL
names(new_data)[1] = "activity"

write.table(new_data, file="data.txt", row.names=FALSE)

