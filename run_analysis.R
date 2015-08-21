# Getting and cleaning data assignment
# Data source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# Data description: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# Script created to
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
#    measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

run_analysis <- function() {
        #Bring data into R
        y_test <- read.table("./test/y_test.txt") 
        x_test <- read.table("./test/X_test.txt")
        subject_test <- read.table("./test/subject_test.txt")
        y_train <- read.table("./train/y_train.txt")
        x_train <- read.table("./train/X_train.txt")
        subject_train <- read.table("./train/subject_train.txt")
        features <- read.table("./features.txt")
        activity_labels <- read.table("./activity_labels.txt")
        
        # Appropriately labels the data set with descriptive variable names. 
        names(x_test) <- features[,2]
        names(x_train) <- features[,2]
        
        # Use descriptive activity names to name the activities in the data set
        names(y_test) <- c("activityid")
        names(y_train) <- c("activityid")
        names(activity_labels) <- c("activityid","activity")
        y_test2 <- merge(y_test, activity_labels)
        y_train2 <- merge(y_train, activity_labels)
        
        # Provide column name to subject
        names(subject_test) <- c("subject") 
        names(subject_train) <- c("subject")
        
        # Put all the data together
        test_data <- bind_cols(subject_test, y_test2, x_test) 
        train_data <- bind_cols(subject_train, y_train2, x_train)
        all_data <- bind_rows(train_data, test_data)
        
        # Extracts only the measurements on the mean and standard deviation for 
        # each measurement. 
        all_data2 <- select(all_data, subject, activityid, activity, 
                            contains("mean()"), contains("std()"))

        # Creates a second, independent tidy data set 
        # with the average of each variable for each activity and each subject.
        all_data3 <- group_by(all_data2, activity, subject)
        summary_data <- summarise_each(all_data3, funs(mean)) 
        summary_data
}
        