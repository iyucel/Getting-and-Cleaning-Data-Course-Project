#run_analysis.R
#Getting and Cleaning Data Course Project
#Student:  Ibrahim Yucel

#1. Merges the training and the test sets to create one data set.
setwd('//UCI HAR Dataset/')

#read in the ancillary and data files and assign relevant column names
     activitylabels <- read.table('./activity_labels.txt', col.names=c("activity", "activityName"))
     features <- read.table('./features.txt',header=FALSE)
     subjectTrain <- read.table('./train/subject_train.txt',header=FALSE)
     xTrain <- read.table('./train/x_train.txt',header=FALSE)
     yTrain <- read.table('./train/y_train.txt',header=FALSE)
     
     subjectTest <- read.table('./test/subject_test.txt',header=FALSE)
     xTest <- read.table('./test/x_test.txt',header=FALSE); 
     yTest <- read.table('./test/y_test.txt',header=FALSE); 
     
     names(activitylabels)
     names(features)
     names(subjectTrain); names(xTrain); names(yTrain)
     #note that only the v1 and v2 column headings seem to match across all data labels

     combinedData <- rbind(xTrain, xTest)
     combinedLabels <- rbind(yTrain, yTest)
     combinedSubjects <- rbind(subjectTrain, subjectTest)
     
#create variable names prior to performing final merging of the data
     
     names(combinedSubjects) <-c("subject")
     names(combinedLabels) <-c("activity")
     names(combinedData) <- features$V2
     
     
     finalLabels <- cbind(combinedSubjects, combinedLabels)
     mergedData <- cbind(combinedData, finalLabels)
     

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

     relevantLabels <-features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
     relevantVariables <-c(as.character(relevantLabels), "subject", "activity")
     finalData <- subset(mergedData, select=relevantVariables)
     
 #3.  Uses descriptive activity names to name the activities in the data set
     
     #first, name the activities
     finalData2 <- merge(finalData,activitylabels, by="activity", all.x=TRUE)
     finalData2$activityName <-as.character(finalData2$activityName)
     
#4. Appropriately labels the data set with descriptive variable names.
     #name the various device components and measurement methods
     
     names(finalData2) <- gsub("^t", "time", names(finalData2))
     names(finalData2) <- gsub("^f", "frequency", names(finalData2))
     names(finalData2) <- gsub("Freq", "frequency", names(finalData2))
     names(finalData2) <- gsub("Acc", "Accelerometer reading", names(finalData2))
     names(finalData2) <- gsub("Gyro", "Gyroscope Reading", names(finalData2))
     names(finalData2) <- gsub("Mag", "Magnitude", names(finalData2))
     names(finalData2) <- gsub("BodyBody", "Body", names(finalData2))
     
#5.     From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
     install.packages("plyr", "dplyr")
     library(dplyr); library(plyr)
     tidyData <- ddply(finalData2, c("subject", "activity"), numcolwise(mean))
     write.table(tidyData, file = "Average data by activity and subject.txt", row.name=FALSE)
     
     
     
     
     
     
     
     
     
     
