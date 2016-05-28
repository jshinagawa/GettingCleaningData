### run_analysi.R ###
## This script will generate the "tidy data" where each measured variable
## is in one column and each different observation of that variable is in a different row. 

## Data process flow
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Set user specific wd
setwd("C:/Users/161205/Documents")
# set data directory
dtdir <- paste0("~", "/R/CourseraWork/GettingAndCleaningData/UCI HAR Dataset/")
# #1 and #4 Merge all data with descriptive variable names. 
# dplyr functions are used 
library(dplyr) 
library(plyr)

# Import all the .txt files in directories ("train", "test")
# and create a list containing all the data.frame for each files
lstctgry <- list("train", "test")
lstfls <- lapply(
        lstctgry, function(x) {
                dr <- paste0(dtdir, x)
                lstflsdr <- list.files(dr)
                lstfls1 <- lstflsdr[grep("\\.txt", lstflsdr)]
                lstfls1nm <- gsub("\\.txt", "", lstfls1)
                lstfls2 <- lapply(
                        lstfls1, function(y) {
                                flnm <- paste0(dr, "/",  y)
                                read.table(flnm)
                        }
                )
                names(lstfls2) <- lstfls1nm
                lstfls2
        } 
)
names(lstfls) <- lstctgry

# Tried join_all but memory ran out
#a <- join_all(lstfls$train)
# First find which df within the list has maximum variable and use it as the base
# try merge one df by one.
nwlstfls <- lapply(
        #lstfls, function(x) {#
        lstctgry, function(x0) {
                x <- lstfls[[x0]]
                lstlngth <- summary(x)[, "Length"]
                #name of max dim df and use this as a base
                mxdmnm <- attr(lstlngth[lstlngth == max(lstlngth)], "names") 
                lstmrgnm <- attr(lstlngth[!lstlngth == max(lstlngth)], "names")
                
                #append the smaller dim df into the base df
                for (y in lstmrgnm) {
                        clnms <- colnames(x[[y]])
                        nwclnms0 <- gsub(paste0("_", x0), "", y)
                        nwclnms <- paste0(nwclnms0, "_", clnms)
                        x[[mxdmnm]][,nwclnms] <- x[[y]]
                }
                #print(colnames(x[[mxdmnm]]))
                x[[mxdmnm]]
        } 
)
names(nwlstfls) <- lstctgry
# Finally merging all data.     
all.data <- merge(nwlstfls$train, nwlstfls$test, all=TRUE) 

# Replace column name of train data with "features" labels ie) variable names 
# This is part of #4 to label the data set with descriptice variable names 
features.data <- read.table(paste0(dtdir, "features.txt")) 
clmnnm <- as.character(features.data$V2) 
colnames(all.data)[c(1:561)] <- clmnnm

##2 Extracts only the measurements on the mean and standard deviation for each measurements. 
meanindex <- grep ("mean", clmnnm, ignore.case=TRUE) 
stdindex <- grep ("std", clmnnm, ignore.case=TRUE) 
meanstdindex <- c(meanindex, stdindex) 
ncol <- length(meanstdindex) 
allmeanstd.data <- all.data[meanstdindex] 
allmeanstd2.data <- cbind(allmeanstd.data, activity = all.data$y, subject = all.data$subject) 


##3 Uses descriptive activity names to name the activityes in the data set. 
#imports activity labels 
actvtylbls <- read.table(paste0(dtdir, "activity_labels.txt")) 
#Replace index with activity labels 
allmeanstd2.data$activity <- actvtylbls$V2[match(allmeanstd2.data$activity, actvtylbls$V1)] 


##5 From the data set in step 4, create as second, independent tidy data set with the average 
# of each variable for each activity and each subject merged train and test sets with "mean" 
# and "std" variables with proper labels. In activity column, the activity numbers were converted 
# to activity names. 
allmeanstd2.data %>% group_by(activity, subject) %>% summarise_each(funs(mean)) -> allmeanstd3.data 
	 
write.table(allmeanstd3.data, file = "tidydata.txt", row.name=FALSE) 
