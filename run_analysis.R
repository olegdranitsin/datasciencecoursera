#################################################################################################
# title: "Getting and Cleaning Data Course Project"
# author: "Oleg Dranitsin"
# date: "11/11/2017"
#################################################################################################

#################################################################################################
### Initial data preparation
#################################################################################################

# Check and install necessary packages
cat("\014")
rm(list = ls())
if (!"dplyr" %in% installed.packages()) {install.packages("dplyr")}; library("dplyr")
if (!"tidyr" %in% installed.packages()) {install.packages("tidyr")}; library("tidyr")
if (!"janitor" %in% installed.packages()) {install.packages("janitor")}; library("janitor")
if (!"knitr" %in% installed.packages()) {install.packages("knitr")}; library("knitr")

#################################################################################################
### Data download
#################################################################################################

# Import of required data
if(!file.exists("./data")){dir.create("./data")}
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "./dataset.zip")
unzip("./dataset.zip")

#################################################################################################
### 1. Merges the training and the test data sets to create one data set.
#################################################################################################

# Import test data
X_test <- as_tibble(read.table("UCI HAR Dataset/test/X_test.txt", sep = "", stringsAsFactors= F))
Y_test <- as_tibble(read.table("UCI HAR Dataset/test/y_test.txt", sep = "", stringsAsFactors= F))
subject_test <- as_tibble(read.table("UCI HAR Dataset/test/subject_test.txt", sep = "", stringsAsFactors= F))

# Import train data
X_train <- as_tibble(read.table("UCI HAR Dataset/train/X_train.txt", sep = "", stringsAsFactors= F))
Y_train <- as_tibble(read.table("UCI HAR Dataset/train/y_train.txt", sep = "", stringsAsFactors= F))
subject_train <- as_tibble(read.table("UCI HAR Dataset/train/subject_train.txt", sep = "", stringsAsFactors= F))

# Join test and train data
test <- bind_cols(subject_test, Y_test, X_test)
train <- bind_cols(subject_train, Y_train, X_train)
joined_data <- ungroup(rbind(test, train))

# Write task_1_dataset.csv as results in csv
write.csv(joined_data, "./data/task_1_dataset.csv")

#################################################################################################
### 4. Appropriately labels the data set with descriptive variable names.
#################################################################################################

# Import file with column names and transpose
features <-  as_tibble(read.table("UCI HAR Dataset/features.txt", sep = "", stringsAsFactors = F))
features_transposed <- as_tibble(t(c("subject_test", "exercise_mode", t(features[,2]))))

### Clean column names clean
colnames(joined_data) <- features_transposed
joined_data_cleaned <- clean_names(joined_data)

# Write task_4_dataset.csv as results in csv
write.csv(joined_data_cleaned, "./data/task_4_dataset.csv")

#################################################################################################
### 3. Uses descriptive activity names to label the activities in the data set
#################################################################################################

# Change numbers to activity names
activity_labels <- as_tibble(read.table("UCI HAR Dataset/activity_labels.txt", sep = "", stringsAsFactors = F))
i <- 0
while (i <= 6) {
  joined_data_cleaned$exercise_mode[joined_data_cleaned$exercise_mode == as.character(i)] <- c(activity_labels[i,2])
  i = i + 1
}
rm(i)
joined_data_cleaned$exercise_mode <- unlist(joined_data_cleaned$exercise_mode)
joined_data_cleaned <- as_tibble(joined_data_cleaned)

# Write task_3_dataset.csv as results in csv
write.csv(joined_data_cleaned, "./data/task_3_dataset.csv")

#################################################################################################
### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#################################################################################################

# Extract mean and std columns
joined_data_selected <- select(joined_data_cleaned, subject_test, exercise_mode, grep("mean", names(joined_data_cleaned)), grep("std", names(joined_data_cleaned)))
joined_data_selected$subject_test <- factor(joined_data_selected$subject_test)
joined_data_selected$exercise_mode <- factor(joined_data_selected$exercise_mode, levels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

# Write task_2_dataset.csv as results in csv
write.csv(joined_data_selected, "./data/task_2_dataset.csv")

#################################################################################################
### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#################################################################################################

# Group and summarize all with mean
joined_data_grouped <- group_by(joined_data_selected, subject_test, exercise_mode)
final_dataset <- summarize_all(joined_data_grouped, funs(mean))

# Write final dataset as results in csv
write.csv(final_dataset, "./data/task_5_final_dataset.csv")
write.table(final_dataset, "./task_5_final_dataset.txt")
            
