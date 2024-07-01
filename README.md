# Peer-graded-Assignment-Getting-and-Cleaning-Data-Course-Project
# Install the quantmod package if it's not already installed
if (!require(quantmod)) {
  install.packages("ye")
  # Load the quantmod package
  library(quantmod)
  # Download Amazon stock prices
  amzn <- getSymbols("AMZN", auto.assign = FALSE)
  # Extract the sample times (dates) from the data
  sampleTimes <- index(amzn)
  # Extract dates from 2012
  dates_2012 <- sampleTimes[format(sampleTimes, "%Y") == "2012"]
  # Count the number of values collected in 2012
  num_values_2012 <- length(dates_2012)
  print(num_values_2012)
  # Extract Mondays from the 2012 dates
  mondays_2012 <- dates_2012[weekdays(dates_2012) == "Monday"]
  # Count the number of values collected on Mondays in 2012
  num_mondays_2012 <- length(mondays_2012)
  print(num_mondays_2012)
###
  # Load necessary libraries
  library(dplyr)
  
  # Step 1: Download and unzip the dataset
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, destfile = "UCI_HAR_Dataset.zip")
  unzip("UCI_HAR_Dataset.zip")
  
  # Step 2: Read the data
  # Read features
  features <- read.table("UCI HAR Dataset/features.txt", col.names = c("index", "feature"))
  
  # Read activity labels
  activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
# Read training data
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
# Read test data
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
# Step 3: Merge the training and test datasets
  subject <- rbind(subject_train, subject_test)
  x <- rbind(x_train, x_test)
  y <- rbind(y_train, y_test)
  merged_data <- cbind(subject, y, x)
# Step 4: Extract only the measurements on the mean and standard deviation for each measurement
measurements <- grepl("mean\\(\\)|std\\(\\)", features$feature)
data_mean_std <- cbind(subject, y, x[, measurements])
# Step 5: Use descriptive activity names to name the activities in the dataset
data_mean_std$code <- activity_labels[data_mean_std$code, 2]
# Step 6: Label the dataset with descriptive variable names
names(data_mean_std)[2] <- "activity"
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Acc", "Accelerometer", names(data_mean_std))
names(data_mean_std) <- gsub("Gyro", "Gyroscope", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
# Step 7: Create a second, independent tidy dataset with the average of each variable for each activity and each subject
  tidy_data <- data_mean_std %>%
    group_by(subject, activity) %>%
    summarise_all(list(mean = mean))
  
# Write the tidy dataset to a file
  write.table(tidy_data, "tidy_data.txt", row.names = FALSE)
  
