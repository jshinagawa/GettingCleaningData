##This script will generate the "tidy data" where each measured variable is in one column and each different observation of that variable is in a different row.

setwd("U:/Profile/Documents/R/WORK/Getting and Cleaning Data/Project")

##1 and #4 Merge all data with descriptive variable names.
	library(dplyr)

	# read train as well as test data from the data folder.       
		train.data <- read.table("./UCI HAR Dataset/train/X_train.txt")
		test.data <- read.table("./UCI HAR Dataset/test/X_test.txt")	
      
	# Replace column name of train data with "features" labels ie) variable names
	# This is part of #4 to label the data set with descriptice variable names

		features.data <- read.table("./UCI HAR Dataset/features.txt")
		column_name <- c(features.data)
      	colnames(train.data) <- column_name$V2
      	colnames(test.data) <- column_name$V2

	# Append subject and activity data before merging the test&train data
      	train_activity.data <- read.table("./UCI HAR Dataset/train/Y_train.txt")
      	test_activity.data <- read.table("./UCI HAR Dataset/test/Y_test.txt")
      	train_subject.data <- read.table("./UCI HAR Dataset/train/subject_train.txt")
		test_subject.data <- read.table("./UCI HAR Dataset/test/subject_test.txt")

      	train.data[,"activity"] <- train_activity.data
      	train.data[,"subject"] <- train_subject.data
		test.data[,"activity"] <- test_activity.data
      	test.data[,"subject"] <- test_subject.data

	# Finally merging all data.    
 		all.data <- merge(train.data, test.data, all=TRUE)

##2 Extracts only the measurements on the mean and standard deviation for each measurements.
	meanindex <- grep ("mean", features.data[,2], ignore.case=TRUE)
	stdindex <- grep ("std", features.data[,2], ignore.case=TRUE)
	meanstdindex <- c(meanindex, stdindex)
      ncol <- length(meanstdindex)
      allmeanstd.data <- all.data[meanstdindex]
      allmeanstd2.data <- cbind(allmeanstd.data, activity = all.data$activity, subject = all.data$subject)

##3 Uses descriptive activity names to name the activityes in the data set.
	#imports activity labels
		activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
	
      #replace index with activity labels
		allmeanstd2.data$activity <- activity_labels$V2[match(allmeanstd2.data$activity, activity_labels$V1)]

##5 From the data set in step 4, create as second, independent tidy data set with the average of each variable for each activity and each subject
## merged train and test sets with "mean" and "std" variables with proper labels. In activity column, the activity numbers were converted to activity names.

	allmeanstd2.data %>% group_by(activity, subject) %>% summarise_each(funs(mean)) -> allmeanstd3.data
	
	write.table(allmeanstd3.data, file = "tidydata.txt", row.name=FALSE)
	

