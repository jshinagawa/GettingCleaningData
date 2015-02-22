##This script will generate the "tidy data" where each measured variable is in one column and each different observation of that variable is in a different row.

setwd("U:/Profile/Documents/R/WORK/Getting and Cleaning Data/Project")

##1 and #4 Merge all data with descriptive variable names.
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
	mean.data <- grep ("mean", features.data[,2], ignore.case=TRUE)
	std.data <- grep ("std", features.data[,2], ignore.case=TRUE)
	mean_std.data <- c(mean.data, std.data)
      ncol <- length(mean_std.data)
      all_mean_std.data <- all.data[mean_std.data]
      all_mean_std_2.data <- cbind(all_mean_std.data, activity = all.data$activity, subject = all.data$subject)

##3 Uses descriptive activity names to name the activityes in the data set.
	#imports activity labels
		activity_labels.data <- read.table("./UCI HAR Dataset/activity_labels.txt")
	
      #replace index with activity labels
		nloop=length(activity_labels.data$V1)
		for (i in 1:nloop)
		{	
			match.data = NULL
            	match.data = all_mean_std_2.data$activity == activity_labels.data$V1[i]
			all_mean_std_2.data$activity[match.data] = as.character(activity_labels.data$V2[i])
		}

##5 From the data set in step 4, create as second, independent tidy data set with the average of each variable for each activity and each subject.
	
	subject_mean_all.data = NULL
	
	#loop through each activity and calculate the mean for each subject. The resulted data.frame is concatenated.
		for (i in 1:nloop)
		{	
			subject_mean.data = NULL
                  subject_mean.data = NULL
			match.data2 = NULL
            	match.data2 = all_mean_std_2.data$activity == activity_labels.data$V2[i]
			subject_mean.data <- aggregate(all_mean_std_2.data[match.data2,1:ncol], by =list(all_mean_std_2.data$subject[match.data2]), mean)
			names(subject_mean.data)[1] <- "subject"
			subject_mean_2.data <- cbind("activity" = activity_labels.data$V2[i], subject_mean.data)
                  subject_mean_all.data <- rbind(subject_mean_all.data, subject_mean_2.data)
		}
