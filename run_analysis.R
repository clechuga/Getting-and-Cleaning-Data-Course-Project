#------------------------------------------------------------------------------------------------------------
# 1. Merges the training and the test sets to create one data set.
#------------------------------------------------------------------------------------------------------------

# Reading in the training data.        
subject_train <- read.table("train/subject_train.txt", header = FALSE, sep = "", col.names="subject")
x_train <- read.table("train/X_train.txt", header = FALSE, sep = "", comment.char = "")
y_train <- read.table("train/y_train.txt", header = FALSE, sep = "", col.names = "activity")

# Reading in the test data.      
subject_test <- read.table("test/subject_test.txt", header = FALSE, sep = "",col.names = "subject")
x_test <- read.table("test/X_test.txt", header = FALSE, sep = "", comment.char = "")
y_test <- read.table("test/y_test.txt", header = FALSE, sep = "", col.names = "activity")

# Combining the training set with the test set and giving header names to each column of the data frame 
# defined by 'features.txt': List of all features.
x_train_test <- rbind(x_train, x_test)
features <- read.table("features.txt", header = FALSE, sep = "")
names(x_train_test) <- make.unique(as.character(features[,2])) 
 
# Combining the subject identifiers from the training and test data.
# Combining the activity labels from the training and test data.
subject <- rbind(subject_train, subject_test)
activity <- rbind(y_train, y_test)

# Merging the training and test data by having the subject identifiers (id) as the first column by convention,
# followed by the feature variable data, and followed by the activity labels.
merged_data <- cbind(subject, x_train_test, activity)


#------------------------------------------------------------------------------------------------------------
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#------------------------------------------------------------------------------------------------------------
features[,2] <- as.character(features[,2])
grep_features <- grep("mean\\(\\)|std\\(\\)|subject|activity", c("subject", features[,2], "activity"))
extracted_data <- merged_data[, grep_features]

#------------------------------------------------------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set.
#------------------------------------------------------------------------------------------------------------
activity <- read.table("activity_labels.txt", header = FALSE, sep = "", col.names = c("activity_labels", "activity"))
        for (i in activity[,1]) { 
                extracted_data$activity[extracted_data$activity == i] <- as.character(activity[i,2])
        }

#------------------------------------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names.
#------------------------------------------------------------------------------------------------------------
names(extracted_data) <- gsub("std","standard deviation",names(extracted_data))
names(extracted_data) <- gsub("BodyBody","body ",names(extracted_data))
names(extracted_data) <- gsub("mean","mean value",names(extracted_data))
names(extracted_data) <- gsub("^f","frequency ",names(extracted_data))
names(extracted_data) <- gsub("^t","time ",names(extracted_data))
names(extracted_data) <- gsub("Body","body ",names(extracted_data))
names(extracted_data) <- gsub("Acc","acceleration ",names(extracted_data))
names(extracted_data) <- gsub("Gyro","gyroscope ",names(extracted_data))
names(extracted_data) <- gsub("Gravity", "gravity ", names(extracted_data))
names(extracted_data) <- gsub("-"," ",names(extracted_data))
names(extracted_data) <- gsub("Jerk","jerk ",names(extracted_data))
names(extracted_data) <- gsub("Mag","magnitude ",names(extracted_data))
names(extracted_data) <- gsub("\\()", "",names(extracted_data))
names(extracted_data) <- gsub("valueX", "value X",names(extracted_data))
names(extracted_data) <- gsub("valueY", "value Y",names(extracted_data))
names(extracted_data) <- gsub("valueZ", "value Z",names(extracted_data))
names(extracted_data) <- gsub("deviationX", "deviation X",names(extracted_data))
names(extracted_data) <- gsub("deviationY", "deviation Y",names(extracted_data))
names(extracted_data) <- gsub("deviationZ", "deviation Z",names(extracted_data))
names(extracted_data) <- gsub("  ", " ",names(extracted_data))

#------------------------------------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
#    variable for each activity and each subject.
#------------------------------------------------------------------------------------------------------------
library(dplyr)
cran <- tbl_df(extracted_data)
tidy_data <- cran %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))