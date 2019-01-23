# FILE - run_analysis.R
#

#Loading Packages
library(dplyr)


#Download Dataset
fname <- "Coursera_PG_Project.zip"

# Checking if zip file already exists.
if (!file.exists(fname)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, fname, method="curl")
}  

# Checking if extracted folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(fname) 
}


#Load Data

# read features and activity labels
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))


#Reading Test Data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

# Reading training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#--------------------------------------------------------------

#1.Merge the training and the test sets to create one data set.
x_all <- rbind(x_train, x_test)
y_all <- rbind(y_train, y_test)
subject_all <- rbind(subject_train, subject_test)
merged <- cbind(subject_all, y_all, x_all)

#2.Extract only the measurements on the mean and standard deviation for each measurement.
tidy_data <- merged %>% select(subject, code, contains("mean"), contains("std"))

#3.Uses descriptive activity names to name the activities in the data set
tidy_data$code <- activities[tidy_data$code, 2]

#4.Appropriately labels the data set with descriptive variable names.

#remove special characters
names(tidy_data) <- gsub("[\\(\\)-]", "", names(tidy_data))

names(tidy_data)[2] = "activity"
names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data)<-gsub("^t", "Time", names(tidy_data))
names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data)<-gsub("mean", "Mean", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("std", "STD", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("freq", "Frequency", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("angle", "Angle", names(tidy_data))
names(tidy_data)<-gsub("gravity", "Gravity", names(tidy_data))
names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))

#write.table(tidy_data, "Tidy_Data.txt", row.name=FALSE)

#From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

final_tidy_data <- tidy_data %>% group_by(subject, activity) %>% summarise_all(funs(mean))
write.table(final_tidy_data, "Grouped_Tidy_Data.txt", row.name=FALSE)
