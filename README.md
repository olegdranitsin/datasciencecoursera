Introduction
------------

This is a Readme document in Markdown format which was created within final course project of Cousera "Getting and Cleaning Data Course".

### This final submission consists of the following files:

1.  run\_analysis.R as required by instructions and does all analyzes in changed order. The file also includes the self description of some parts of the process, so you can see most necessary description there. After you run this script, it should, also, save results of each 5 steps of the final task as csv format in ./data folder. Required final tidy dataset is stored as ./task\_5\_final\_dataset.txt

2.  Readme.Rmd documents which describe each step of analyzes with a bit more details and results displayed. Also, this file allows run analyzes as the separate function for each data step with explanation of each step.

3.  CodeBook.Rmd which contains codebook of initially used data and codebook for each step of analyzes.

The order of steps neede to complete final task was changed and that was reflected by step numbes.

### Initial data preparation

``` r
# Check and install necessary packages
cat("\014")
```



``` r
rm(list = ls())
if (!"dplyr" %in% installed.packages()) {install.packages("dplyr")}; library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
if (!"tidyr" %in% installed.packages()) {install.packages("tidyr")}; library("tidyr")
if (!"janitor" %in% installed.packages()) {install.packages("janitor")}; library("janitor")
if (!"knitr" %in% installed.packages()) {install.packages("knitr")}; library("knitr")
```

### Data download

``` r
# Import of required data
if(!file.exists("./data")){dir.create("./data")}
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "./dataset.zip")
unzip("./dataset.zip")
```

### 1. Merges the training and the test data sets to create one data set.

``` r
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
```

### 4. Appropriately labels the data set with descriptive variable names.

``` r
# Import file with column names and transpose
features <-  as_tibble(read.table("UCI HAR Dataset/features.txt", sep = "", stringsAsFactors = F))
features_transposed <- as_tibble(t(c("subject_test", "exercise_mode", t(features[,2]))))

### Clean column names clean
colnames(joined_data) <- features_transposed
joined_data_cleaned <- clean_names(joined_data)

# Write task_4_dataset.csv as results in csv
write.csv(joined_data_cleaned, "./data/task_4_dataset.csv")
```

### 3. Uses descriptive activity names to label the activities in the data set

``` r
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
```

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

``` r
# Extract mean and std columns
joined_data_selected <-  select(joined_data_cleaned, subject_test, exercise_mode, grep("mean", names(joined_data_cleaned)), grep("std", names(joined_data_cleaned)))
joined_data_selected$subject_test <- factor(joined_data_selected$subject_test)
joined_data_selected$exercise_mode <- factor(joined_data_selected$exercise_mode, levels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

# Write task_2_dataset.csv as results in csv
write.csv(joined_data_selected, "./data/task_2_dataset.csv")
```

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

``` r
# Group and summarize all with mean
joined_data_grouped <- group_by(joined_data_selected, subject_test, exercise_mode)
final_dataset <- summarize_all(joined_data_grouped, funs(mean))

# Write final dataset as results in csv
write.csv(final_dataset, "./data/task_5_final_dataset.csv")
write.table(final_dataset, "./task_5_final_dataset.txt")
```

### Final dataset check

``` r
str(final_dataset)
```

    ## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':  180 obs. of  88 variables:
    ##  $ subject_test                       : Factor w/ 30 levels "1","2","3","4",..: 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ exercise_mode                      : Factor w/ 6 levels "WALKING","WALKING_UPSTAIRS",..: 1 2 3 4 5 6 1 2 3 4 ...
    ##  $ tbodyacc_mean_x                    : num  0.277 0.255 0.289 0.261 0.279 ...
    ##  $ tbodyacc_mean_y                    : num  -0.01738 -0.02395 -0.00992 -0.00131 -0.01614 ...
    ##  $ tbodyacc_mean_z                    : num  -0.1111 -0.0973 -0.1076 -0.1045 -0.1106 ...
    ##  $ tgravityacc_mean_x                 : num  0.935 0.893 0.932 0.832 0.943 ...
    ##  $ tgravityacc_mean_y                 : num  -0.282 -0.362 -0.267 0.204 -0.273 ...
    ##  $ tgravityacc_mean_z                 : num  -0.0681 -0.0754 -0.0621 0.332 0.0135 ...
    ##  $ tbodyaccjerk_mean_x                : num  0.074 0.1014 0.0542 0.0775 0.0754 ...
    ##  $ tbodyaccjerk_mean_y                : num  0.028272 0.019486 0.02965 -0.000619 0.007976 ...
    ##  $ tbodyaccjerk_mean_z                : num  -0.00417 -0.04556 -0.01097 -0.00337 -0.00369 ...
    ##  $ tbodygyro_mean_x                   : num  -0.0418 0.0505 -0.0351 -0.0454 -0.024 ...
    ##  $ tbodygyro_mean_y                   : num  -0.0695 -0.1662 -0.0909 -0.0919 -0.0594 ...
    ##  $ tbodygyro_mean_z                   : num  0.0849 0.0584 0.0901 0.0629 0.0748 ...
    ##  $ tbodygyrojerk_mean_x               : num  -0.09 -0.1222 -0.074 -0.0937 -0.0996 ...
    ##  $ tbodygyrojerk_mean_y               : num  -0.0398 -0.0421 -0.044 -0.0402 -0.0441 ...
    ##  $ tbodygyrojerk_mean_z               : num  -0.0461 -0.0407 -0.027 -0.0467 -0.049 ...
    ##  $ tbodyaccmag_mean                   : num  -0.137 -0.1299 0.0272 -0.9485 -0.9843 ...
    ##  $ tgravityaccmag_mean                : num  -0.137 -0.1299 0.0272 -0.9485 -0.9843 ...
    ##  $ tbodyaccjerkmag_mean               : num  -0.1414 -0.4665 -0.0894 -0.9874 -0.9924 ...
    ##  $ tbodygyromag_mean                  : num  -0.161 -0.1267 -0.0757 -0.9309 -0.9765 ...
    ##  $ tbodygyrojerkmag_mean              : num  -0.299 -0.595 -0.295 -0.992 -0.995 ...
    ##  $ fbodyacc_mean_x                    : num  -0.2028 -0.4043 0.0382 -0.9796 -0.9952 ...
    ##  $ fbodyacc_mean_y                    : num  0.08971 -0.19098 0.00155 -0.94408 -0.97707 ...
    ##  $ fbodyacc_mean_z                    : num  -0.332 -0.433 -0.226 -0.959 -0.985 ...
    ##  $ fbodyacc_meanfreq_x                : num  -0.2075 -0.4187 -0.3074 -0.0495 0.0865 ...
    ##  $ fbodyacc_meanfreq_y                : num  0.1131 -0.1607 0.0632 0.0759 0.1175 ...
    ##  $ fbodyacc_meanfreq_z                : num  0.0497 -0.5201 0.2943 0.2388 0.2449 ...
    ##  $ fbodyaccjerk_mean_x                : num  -0.1705 -0.4799 -0.0277 -0.9866 -0.9946 ...
    ##  $ fbodyaccjerk_mean_y                : num  -0.0352 -0.4134 -0.1287 -0.9816 -0.9854 ...
    ##  $ fbodyaccjerk_mean_z                : num  -0.469 -0.685 -0.288 -0.986 -0.991 ...
    ##  $ fbodyaccjerk_meanfreq_x            : num  -0.209 -0.377 -0.253 0.257 0.314 ...
    ##  $ fbodyaccjerk_meanfreq_y            : num  -0.3862 -0.5095 -0.3376 0.0475 0.0392 ...
    ##  $ fbodyaccjerk_meanfreq_z            : num  -0.18553 -0.5511 0.00937 0.09239 0.13858 ...
    ##  $ fbodygyro_mean_x                   : num  -0.339 -0.493 -0.352 -0.976 -0.986 ...
    ##  $ fbodygyro_mean_y                   : num  -0.1031 -0.3195 -0.0557 -0.9758 -0.989 ...
    ##  $ fbodygyro_mean_z                   : num  -0.2559 -0.4536 -0.0319 -0.9513 -0.9808 ...
    ##  $ fbodygyro_meanfreq_x               : num  0.0148 -0.1875 -0.1005 0.1892 -0.1203 ...
    ##  $ fbodygyro_meanfreq_y               : num  -0.0658 -0.4736 0.0826 0.0631 -0.0447 ...
    ##  $ fbodygyro_meanfreq_z               : num  0.000773 -0.133374 -0.075676 -0.029784 0.100608 ...
    ##  $ fbodyaccmag_mean                   : num  -0.1286 -0.3524 0.0966 -0.9478 -0.9854 ...
    ##  $ fbodyaccmag_meanfreq               : num  0.1906 -0.0977 0.1192 0.2367 0.2846 ...
    ##  $ fbodybodyaccjerkmag_mean           : num  -0.0571 -0.4427 0.0262 -0.9853 -0.9925 ...
    ##  $ fbodybodyaccjerkmag_meanfreq       : num  0.0938 0.0854 0.0765 0.3519 0.4222 ...
    ##  $ fbodybodygyromag_mean              : num  -0.199 -0.326 -0.186 -0.958 -0.985 ...
    ##  $ fbodybodygyromag_meanfreq          : num  0.268844 -0.219303 0.349614 -0.000262 -0.028606 ...
    ##  $ fbodybodygyrojerkmag_mean          : num  -0.319 -0.635 -0.282 -0.99 -0.995 ...
    ##  $ fbodybodygyrojerkmag_meanfreq      : num  0.191 0.114 0.19 0.185 0.334 ...
    ##  $ angle_tbodyaccmean_gravity         : num  0.060454 0.096086 -0.002695 0.027442 -0.000222 ...
    ##  $ angle_tbodyaccjerkmean_gravitymean : num  -0.00793 -0.06108 0.08993 0.02971 0.02196 ...
    ##  $ angle_tbodygyromean_gravitymean    : num  0.0131 -0.1947 0.0633 0.0677 -0.0338 ...
    ##  $ angle_tbodygyrojerkmean_gravitymean: num  -0.0187 0.0657 -0.04 -0.0649 -0.0279 ...
    ##  $ angle_x_gravitymean                : num  -0.729 -0.647 -0.744 -0.591 -0.743 ...
    ##  $ angle_y_gravitymean                : num  0.277 0.3348 0.2672 -0.0605 0.2702 ...
    ##  $ angle_z_gravitymean                : num  0.0689 0.0742 0.065 -0.218 0.0123 ...
    ##  $ tbodyacc_std_x                     : num  -0.284 -0.355 0.03 -0.977 -0.996 ...
    ##  $ tbodyacc_std_y                     : num  0.11446 -0.00232 -0.03194 -0.92262 -0.97319 ...
    ##  $ tbodyacc_std_z                     : num  -0.26 -0.0195 -0.2304 -0.9396 -0.9798 ...
    ##  $ tgravityacc_std_x                  : num  -0.977 -0.956 -0.951 -0.968 -0.994 ...
    ##  $ tgravityacc_std_y                  : num  -0.971 -0.953 -0.937 -0.936 -0.981 ...
    ##  $ tgravityacc_std_z                  : num  -0.948 -0.912 -0.896 -0.949 -0.976 ...
    ##  $ tbodyaccjerk_std_x                 : num  -0.1136 -0.4468 -0.0123 -0.9864 -0.9946 ...
    ##  $ tbodyaccjerk_std_y                 : num  0.067 -0.378 -0.102 -0.981 -0.986 ...
    ##  $ tbodyaccjerk_std_z                 : num  -0.503 -0.707 -0.346 -0.988 -0.992 ...
    ##  $ tbodygyro_std_x                    : num  -0.474 -0.545 -0.458 -0.977 -0.987 ...
    ##  $ tbodygyro_std_y                    : num  -0.05461 0.00411 -0.12635 -0.96647 -0.98773 ...
    ##  $ tbodygyro_std_z                    : num  -0.344 -0.507 -0.125 -0.941 -0.981 ...
    ##  $ tbodygyrojerk_std_x                : num  -0.207 -0.615 -0.487 -0.992 -0.993 ...
    ##  $ tbodygyrojerk_std_y                : num  -0.304 -0.602 -0.239 -0.99 -0.995 ...
    ##  $ tbodygyrojerk_std_z                : num  -0.404 -0.606 -0.269 -0.988 -0.992 ...
    ##  $ tbodyaccmag_std                    : num  -0.2197 -0.325 0.0199 -0.9271 -0.9819 ...
    ##  $ tgravityaccmag_std                 : num  -0.2197 -0.325 0.0199 -0.9271 -0.9819 ...
    ##  $ tbodyaccjerkmag_std                : num  -0.0745 -0.479 -0.0258 -0.9841 -0.9931 ...
    ##  $ tbodygyromag_std                   : num  -0.187 -0.149 -0.226 -0.935 -0.979 ...
    ##  $ tbodygyrojerkmag_std               : num  -0.325 -0.649 -0.307 -0.988 -0.995 ...
    ##  $ fbodyacc_std_x                     : num  -0.3191 -0.3374 0.0243 -0.9764 -0.996 ...
    ##  $ fbodyacc_std_y                     : num  0.056 0.0218 -0.113 -0.9173 -0.9723 ...
    ##  $ fbodyacc_std_z                     : num  -0.28 0.086 -0.298 -0.934 -0.978 ...
    ##  $ fbodyaccjerk_std_x                 : num  -0.1336 -0.4619 -0.0863 -0.9875 -0.9951 ...
    ##  $ fbodyaccjerk_std_y                 : num  0.107 -0.382 -0.135 -0.983 -0.987 ...
    ##  $ fbodyaccjerk_std_z                 : num  -0.535 -0.726 -0.402 -0.988 -0.992 ...
    ##  $ fbodygyro_std_x                    : num  -0.517 -0.566 -0.495 -0.978 -0.987 ...
    ##  $ fbodygyro_std_y                    : num  -0.0335 0.1515 -0.1814 -0.9623 -0.9871 ...
    ##  $ fbodygyro_std_z                    : num  -0.437 -0.572 -0.238 -0.944 -0.982 ...
    ##  $ fbodyaccmag_std                    : num  -0.398 -0.416 -0.187 -0.928 -0.982 ...
    ##  $ fbodybodyaccjerkmag_std            : num  -0.103 -0.533 -0.104 -0.982 -0.993 ...
    ##  $ fbodybodygyromag_std               : num  -0.321 -0.183 -0.398 -0.932 -0.978 ...
    ##  $ fbodybodygyrojerkmag_std           : num  -0.382 -0.694 -0.392 -0.987 -0.995 ...
    ##  - attr(*, "vars")= chr "subject_test"
    ##  - attr(*, "drop")= logi TRUE

### Final dataset

``` r
kable(head(final_dataset), format = "markdown", padding = 0, caption = "Final dataset")
```

<table style="width:100%;">
<colgroup>
<col width="0%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="2%" />
<col width="1%" />
<col width="2%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">subject_test</th>
<th align="left">exercise_mode</th>
<th align="right">tbodyacc_mean_x</th>
<th align="right">tbodyacc_mean_y</th>
<th align="right">tbodyacc_mean_z</th>
<th align="right">tgravityacc_mean_x</th>
<th align="right">tgravityacc_mean_y</th>
<th align="right">tgravityacc_mean_z</th>
<th align="right">tbodyaccjerk_mean_x</th>
<th align="right">tbodyaccjerk_mean_y</th>
<th align="right">tbodyaccjerk_mean_z</th>
<th align="right">tbodygyro_mean_x</th>
<th align="right">tbodygyro_mean_y</th>
<th align="right">tbodygyro_mean_z</th>
<th align="right">tbodygyrojerk_mean_x</th>
<th align="right">tbodygyrojerk_mean_y</th>
<th align="right">tbodygyrojerk_mean_z</th>
<th align="right">tbodyaccmag_mean</th>
<th align="right">tgravityaccmag_mean</th>
<th align="right">tbodyaccjerkmag_mean</th>
<th align="right">tbodygyromag_mean</th>
<th align="right">tbodygyrojerkmag_mean</th>
<th align="right">fbodyacc_mean_x</th>
<th align="right">fbodyacc_mean_y</th>
<th align="right">fbodyacc_mean_z</th>
<th align="right">fbodyacc_meanfreq_x</th>
<th align="right">fbodyacc_meanfreq_y</th>
<th align="right">fbodyacc_meanfreq_z</th>
<th align="right">fbodyaccjerk_mean_x</th>
<th align="right">fbodyaccjerk_mean_y</th>
<th align="right">fbodyaccjerk_mean_z</th>
<th align="right">fbodyaccjerk_meanfreq_x</th>
<th align="right">fbodyaccjerk_meanfreq_y</th>
<th align="right">fbodyaccjerk_meanfreq_z</th>
<th align="right">fbodygyro_mean_x</th>
<th align="right">fbodygyro_mean_y</th>
<th align="right">fbodygyro_mean_z</th>
<th align="right">fbodygyro_meanfreq_x</th>
<th align="right">fbodygyro_meanfreq_y</th>
<th align="right">fbodygyro_meanfreq_z</th>
<th align="right">fbodyaccmag_mean</th>
<th align="right">fbodyaccmag_meanfreq</th>
<th align="right">fbodybodyaccjerkmag_mean</th>
<th align="right">fbodybodyaccjerkmag_meanfreq</th>
<th align="right">fbodybodygyromag_mean</th>
<th align="right">fbodybodygyromag_meanfreq</th>
<th align="right">fbodybodygyrojerkmag_mean</th>
<th align="right">fbodybodygyrojerkmag_meanfreq</th>
<th align="right">angle_tbodyaccmean_gravity</th>
<th align="right">angle_tbodyaccjerkmean_gravitymean</th>
<th align="right">angle_tbodygyromean_gravitymean</th>
<th align="right">angle_tbodygyrojerkmean_gravitymean</th>
<th align="right">angle_x_gravitymean</th>
<th align="right">angle_y_gravitymean</th>
<th align="right">angle_z_gravitymean</th>
<th align="right">tbodyacc_std_x</th>
<th align="right">tbodyacc_std_y</th>
<th align="right">tbodyacc_std_z</th>
<th align="right">tgravityacc_std_x</th>
<th align="right">tgravityacc_std_y</th>
<th align="right">tgravityacc_std_z</th>
<th align="right">tbodyaccjerk_std_x</th>
<th align="right">tbodyaccjerk_std_y</th>
<th align="right">tbodyaccjerk_std_z</th>
<th align="right">tbodygyro_std_x</th>
<th align="right">tbodygyro_std_y</th>
<th align="right">tbodygyro_std_z</th>
<th align="right">tbodygyrojerk_std_x</th>
<th align="right">tbodygyrojerk_std_y</th>
<th align="right">tbodygyrojerk_std_z</th>
<th align="right">tbodyaccmag_std</th>
<th align="right">tgravityaccmag_std</th>
<th align="right">tbodyaccjerkmag_std</th>
<th align="right">tbodygyromag_std</th>
<th align="right">tbodygyrojerkmag_std</th>
<th align="right">fbodyacc_std_x</th>
<th align="right">fbodyacc_std_y</th>
<th align="right">fbodyacc_std_z</th>
<th align="right">fbodyaccjerk_std_x</th>
<th align="right">fbodyaccjerk_std_y</th>
<th align="right">fbodyaccjerk_std_z</th>
<th align="right">fbodygyro_std_x</th>
<th align="right">fbodygyro_std_y</th>
<th align="right">fbodygyro_std_z</th>
<th align="right">fbodyaccmag_std</th>
<th align="right">fbodybodyaccjerkmag_std</th>
<th align="right">fbodybodygyromag_std</th>
<th align="right">fbodybodygyrojerkmag_std</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">1</td>
<td align="left">WALKING</td>
<td align="right">0.2773308</td>
<td align="right">-0.0173838</td>
<td align="right">-0.1111481</td>
<td align="right">0.9352232</td>
<td align="right">-0.2821650</td>
<td align="right">-0.0681029</td>
<td align="right">0.0740416</td>
<td align="right">0.0282721</td>
<td align="right">-0.0041684</td>
<td align="right">-0.0418310</td>
<td align="right">-0.0695300</td>
<td align="right">0.0849448</td>
<td align="right">-0.0899975</td>
<td align="right">-0.0398429</td>
<td align="right">-0.0461309</td>
<td align="right">-0.1369712</td>
<td align="right">-0.1369712</td>
<td align="right">-0.1414288</td>
<td align="right">-0.1609796</td>
<td align="right">-0.2987037</td>
<td align="right">-0.2027943</td>
<td align="right">0.0897127</td>
<td align="right">-0.3315601</td>
<td align="right">-0.2075484</td>
<td align="right">0.1130936</td>
<td align="right">0.0497265</td>
<td align="right">-0.1705470</td>
<td align="right">-0.0352255</td>
<td align="right">-0.4689992</td>
<td align="right">-0.2092620</td>
<td align="right">-0.3862371</td>
<td align="right">-0.1855303</td>
<td align="right">-0.3390322</td>
<td align="right">-0.1030594</td>
<td align="right">-0.2559409</td>
<td align="right">0.0147845</td>
<td align="right">-0.0657746</td>
<td align="right">0.0007733</td>
<td align="right">-0.1286235</td>
<td align="right">0.1906437</td>
<td align="right">-0.0571194</td>
<td align="right">0.0938222</td>
<td align="right">-0.1992526</td>
<td align="right">0.2688444</td>
<td align="right">-0.3193086</td>
<td align="right">0.1906634</td>
<td align="right">0.0604537</td>
<td align="right">-0.0079304</td>
<td align="right">0.0130595</td>
<td align="right">-0.0187432</td>
<td align="right">-0.7292472</td>
<td align="right">0.2769530</td>
<td align="right">0.0688589</td>
<td align="right">-0.2837403</td>
<td align="right">0.1144613</td>
<td align="right">-0.2600279</td>
<td align="right">-0.9766096</td>
<td align="right">-0.9713060</td>
<td align="right">-0.9477172</td>
<td align="right">-0.1136156</td>
<td align="right">0.0670025</td>
<td align="right">-0.5026998</td>
<td align="right">-0.4735355</td>
<td align="right">-0.0546078</td>
<td align="right">-0.3442666</td>
<td align="right">-0.2074219</td>
<td align="right">-0.3044685</td>
<td align="right">-0.4042555</td>
<td align="right">-0.2196886</td>
<td align="right">-0.2196886</td>
<td align="right">-0.0744718</td>
<td align="right">-0.1869784</td>
<td align="right">-0.3253249</td>
<td align="right">-0.3191347</td>
<td align="right">0.0560400</td>
<td align="right">-0.2796868</td>
<td align="right">-0.1335866</td>
<td align="right">0.1067399</td>
<td align="right">-0.5347134</td>
<td align="right">-0.5166919</td>
<td align="right">-0.0335082</td>
<td align="right">-0.4365622</td>
<td align="right">-0.3980326</td>
<td align="right">-0.1034924</td>
<td align="right">-0.3210180</td>
<td align="right">-0.3816019</td>
</tr>
<tr class="even">
<td align="left">1</td>
<td align="left">WALKING_UPSTAIRS</td>
<td align="right">0.2554617</td>
<td align="right">-0.0239531</td>
<td align="right">-0.0973020</td>
<td align="right">0.8933511</td>
<td align="right">-0.3621534</td>
<td align="right">-0.0754029</td>
<td align="right">0.1013727</td>
<td align="right">0.0194863</td>
<td align="right">-0.0455625</td>
<td align="right">0.0505494</td>
<td align="right">-0.1661700</td>
<td align="right">0.0583595</td>
<td align="right">-0.1222328</td>
<td align="right">-0.0421486</td>
<td align="right">-0.0407126</td>
<td align="right">-0.1299276</td>
<td align="right">-0.1299276</td>
<td align="right">-0.4665034</td>
<td align="right">-0.1267356</td>
<td align="right">-0.5948829</td>
<td align="right">-0.4043218</td>
<td align="right">-0.1909767</td>
<td align="right">-0.4333497</td>
<td align="right">-0.4187350</td>
<td align="right">-0.1606972</td>
<td align="right">-0.5201148</td>
<td align="right">-0.4798752</td>
<td align="right">-0.4134446</td>
<td align="right">-0.6854744</td>
<td align="right">-0.3770231</td>
<td align="right">-0.5094955</td>
<td align="right">-0.5511043</td>
<td align="right">-0.4926117</td>
<td align="right">-0.3194746</td>
<td align="right">-0.4535972</td>
<td align="right">-0.1874502</td>
<td align="right">-0.4735748</td>
<td align="right">-0.1333739</td>
<td align="right">-0.3523959</td>
<td align="right">-0.0977433</td>
<td align="right">-0.4426522</td>
<td align="right">0.0853524</td>
<td align="right">-0.3259615</td>
<td align="right">-0.2193034</td>
<td align="right">-0.6346651</td>
<td align="right">0.1142773</td>
<td align="right">0.0960861</td>
<td align="right">-0.0610838</td>
<td align="right">-0.1947000</td>
<td align="right">0.0656836</td>
<td align="right">-0.6471957</td>
<td align="right">0.3347633</td>
<td align="right">0.0741664</td>
<td align="right">-0.3547080</td>
<td align="right">-0.0023203</td>
<td align="right">-0.0194792</td>
<td align="right">-0.9563670</td>
<td align="right">-0.9528492</td>
<td align="right">-0.9123794</td>
<td align="right">-0.4468439</td>
<td align="right">-0.3782744</td>
<td align="right">-0.7065935</td>
<td align="right">-0.5448711</td>
<td align="right">0.0041052</td>
<td align="right">-0.5071687</td>
<td align="right">-0.6147865</td>
<td align="right">-0.6016967</td>
<td align="right">-0.6063320</td>
<td align="right">-0.3249709</td>
<td align="right">-0.3249709</td>
<td align="right">-0.4789916</td>
<td align="right">-0.1486193</td>
<td align="right">-0.6485530</td>
<td align="right">-0.3374282</td>
<td align="right">0.0217695</td>
<td align="right">0.0859566</td>
<td align="right">-0.4619070</td>
<td align="right">-0.3817771</td>
<td align="right">-0.7260402</td>
<td align="right">-0.5658925</td>
<td align="right">0.1515389</td>
<td align="right">-0.5717078</td>
<td align="right">-0.4162601</td>
<td align="right">-0.5330599</td>
<td align="right">-0.1829855</td>
<td align="right">-0.6939305</td>
</tr>
<tr class="odd">
<td align="left">1</td>
<td align="left">WALKING_DOWNSTAIRS</td>
<td align="right">0.2891883</td>
<td align="right">-0.0099185</td>
<td align="right">-0.1075662</td>
<td align="right">0.9318744</td>
<td align="right">-0.2666103</td>
<td align="right">-0.0621200</td>
<td align="right">0.0541553</td>
<td align="right">0.0296504</td>
<td align="right">-0.0109720</td>
<td align="right">-0.0350782</td>
<td align="right">-0.0909371</td>
<td align="right">0.0900850</td>
<td align="right">-0.0739592</td>
<td align="right">-0.0439903</td>
<td align="right">-0.0270461</td>
<td align="right">0.0271883</td>
<td align="right">0.0271883</td>
<td align="right">-0.0894475</td>
<td align="right">-0.0757413</td>
<td align="right">-0.2954638</td>
<td align="right">0.0382292</td>
<td align="right">0.0015499</td>
<td align="right">-0.2255745</td>
<td align="right">-0.3073952</td>
<td align="right">0.0632201</td>
<td align="right">0.2943227</td>
<td align="right">-0.0276639</td>
<td align="right">-0.1286672</td>
<td align="right">-0.2883347</td>
<td align="right">-0.2531643</td>
<td align="right">-0.3375897</td>
<td align="right">0.0093722</td>
<td align="right">-0.3524496</td>
<td align="right">-0.0557023</td>
<td align="right">-0.0318694</td>
<td align="right">-0.1004537</td>
<td align="right">0.0825511</td>
<td align="right">-0.0756762</td>
<td align="right">0.0965845</td>
<td align="right">0.1191871</td>
<td align="right">0.0262185</td>
<td align="right">0.0764915</td>
<td align="right">-0.1857203</td>
<td align="right">0.3496139</td>
<td align="right">-0.2819634</td>
<td align="right">0.1900007</td>
<td align="right">-0.0026951</td>
<td align="right">0.0899317</td>
<td align="right">0.0633383</td>
<td align="right">-0.0399768</td>
<td align="right">-0.7444838</td>
<td align="right">0.2672458</td>
<td align="right">0.0650047</td>
<td align="right">0.0300353</td>
<td align="right">-0.0319359</td>
<td align="right">-0.2304342</td>
<td align="right">-0.9505598</td>
<td align="right">-0.9370187</td>
<td align="right">-0.8959397</td>
<td align="right">-0.0122839</td>
<td align="right">-0.1016014</td>
<td align="right">-0.3457350</td>
<td align="right">-0.4580305</td>
<td align="right">-0.1263492</td>
<td align="right">-0.1247025</td>
<td align="right">-0.4870273</td>
<td align="right">-0.2388248</td>
<td align="right">-0.2687615</td>
<td align="right">0.0198844</td>
<td align="right">0.0198844</td>
<td align="right">-0.0257877</td>
<td align="right">-0.2257244</td>
<td align="right">-0.3065106</td>
<td align="right">0.0243308</td>
<td align="right">-0.1129637</td>
<td align="right">-0.2979279</td>
<td align="right">-0.0863279</td>
<td align="right">-0.1345800</td>
<td align="right">-0.4017215</td>
<td align="right">-0.4954225</td>
<td align="right">-0.1814147</td>
<td align="right">-0.2384436</td>
<td align="right">-0.1865303</td>
<td align="right">-0.1040523</td>
<td align="right">-0.3983504</td>
<td align="right">-0.3919199</td>
</tr>
<tr class="even">
<td align="left">1</td>
<td align="left">SITTING</td>
<td align="right">0.2612376</td>
<td align="right">-0.0013083</td>
<td align="right">-0.1045442</td>
<td align="right">0.8315099</td>
<td align="right">0.2044116</td>
<td align="right">0.3320437</td>
<td align="right">0.0774825</td>
<td align="right">-0.0006191</td>
<td align="right">-0.0033678</td>
<td align="right">-0.0453501</td>
<td align="right">-0.0919242</td>
<td align="right">0.0629314</td>
<td align="right">-0.0936794</td>
<td align="right">-0.0402118</td>
<td align="right">-0.0467026</td>
<td align="right">-0.9485368</td>
<td align="right">-0.9485368</td>
<td align="right">-0.9873642</td>
<td align="right">-0.9308925</td>
<td align="right">-0.9919763</td>
<td align="right">-0.9796412</td>
<td align="right">-0.9440846</td>
<td align="right">-0.9591849</td>
<td align="right">-0.0495136</td>
<td align="right">0.0759461</td>
<td align="right">0.2388299</td>
<td align="right">-0.9865970</td>
<td align="right">-0.9815795</td>
<td align="right">-0.9860531</td>
<td align="right">0.2566108</td>
<td align="right">0.0475438</td>
<td align="right">0.0923920</td>
<td align="right">-0.9761615</td>
<td align="right">-0.9758386</td>
<td align="right">-0.9513155</td>
<td align="right">0.1891530</td>
<td align="right">0.0631271</td>
<td align="right">-0.0297839</td>
<td align="right">-0.9477829</td>
<td align="right">0.2366550</td>
<td align="right">-0.9852621</td>
<td align="right">0.3518522</td>
<td align="right">-0.9584356</td>
<td align="right">-0.0002622</td>
<td align="right">-0.9897975</td>
<td align="right">0.1847759</td>
<td align="right">0.0274415</td>
<td align="right">0.0297098</td>
<td align="right">0.0676981</td>
<td align="right">-0.0648816</td>
<td align="right">-0.5912475</td>
<td align="right">-0.0604660</td>
<td align="right">-0.2180172</td>
<td align="right">-0.9772290</td>
<td align="right">-0.9226186</td>
<td align="right">-0.9395863</td>
<td align="right">-0.9684571</td>
<td align="right">-0.9355171</td>
<td align="right">-0.9490409</td>
<td align="right">-0.9864307</td>
<td align="right">-0.9813720</td>
<td align="right">-0.9879108</td>
<td align="right">-0.9772113</td>
<td align="right">-0.9664739</td>
<td align="right">-0.9414259</td>
<td align="right">-0.9917316</td>
<td align="right">-0.9895181</td>
<td align="right">-0.9879358</td>
<td align="right">-0.9270784</td>
<td align="right">-0.9270784</td>
<td align="right">-0.9841200</td>
<td align="right">-0.9345318</td>
<td align="right">-0.9883087</td>
<td align="right">-0.9764123</td>
<td align="right">-0.9172750</td>
<td align="right">-0.9344696</td>
<td align="right">-0.9874930</td>
<td align="right">-0.9825139</td>
<td align="right">-0.9883392</td>
<td align="right">-0.9779042</td>
<td align="right">-0.9623450</td>
<td align="right">-0.9439178</td>
<td align="right">-0.9284448</td>
<td align="right">-0.9816062</td>
<td align="right">-0.9321984</td>
<td align="right">-0.9870496</td>
</tr>
<tr class="odd">
<td align="left">1</td>
<td align="left">STANDING</td>
<td align="right">0.2789176</td>
<td align="right">-0.0161376</td>
<td align="right">-0.1106018</td>
<td align="right">0.9429520</td>
<td align="right">-0.2729838</td>
<td align="right">0.0134906</td>
<td align="right">0.0753767</td>
<td align="right">0.0079757</td>
<td align="right">-0.0036852</td>
<td align="right">-0.0239877</td>
<td align="right">-0.0593972</td>
<td align="right">0.0748008</td>
<td align="right">-0.0996092</td>
<td align="right">-0.0440628</td>
<td align="right">-0.0489505</td>
<td align="right">-0.9842782</td>
<td align="right">-0.9842782</td>
<td align="right">-0.9923678</td>
<td align="right">-0.9764938</td>
<td align="right">-0.9949668</td>
<td align="right">-0.9952499</td>
<td align="right">-0.9770708</td>
<td align="right">-0.9852971</td>
<td align="right">0.0865154</td>
<td align="right">0.1174789</td>
<td align="right">0.2448586</td>
<td align="right">-0.9946308</td>
<td align="right">-0.9854187</td>
<td align="right">-0.9907522</td>
<td align="right">0.3141829</td>
<td align="right">0.0391619</td>
<td align="right">0.1385815</td>
<td align="right">-0.9863868</td>
<td align="right">-0.9889845</td>
<td align="right">-0.9807731</td>
<td align="right">-0.1202930</td>
<td align="right">-0.0447192</td>
<td align="right">0.1006076</td>
<td align="right">-0.9853564</td>
<td align="right">0.2845553</td>
<td align="right">-0.9925425</td>
<td align="right">0.4222201</td>
<td align="right">-0.9846176</td>
<td align="right">-0.0286058</td>
<td align="right">-0.9948154</td>
<td align="right">0.3344987</td>
<td align="right">-0.0002223</td>
<td align="right">0.0219638</td>
<td align="right">-0.0337938</td>
<td align="right">-0.0279229</td>
<td align="right">-0.7434079</td>
<td align="right">0.2701750</td>
<td align="right">0.0122529</td>
<td align="right">-0.9957599</td>
<td align="right">-0.9731901</td>
<td align="right">-0.9797759</td>
<td align="right">-0.9937630</td>
<td align="right">-0.9812260</td>
<td align="right">-0.9763241</td>
<td align="right">-0.9946045</td>
<td align="right">-0.9856487</td>
<td align="right">-0.9922512</td>
<td align="right">-0.9871919</td>
<td align="right">-0.9877344</td>
<td align="right">-0.9806456</td>
<td align="right">-0.9929451</td>
<td align="right">-0.9951379</td>
<td align="right">-0.9921085</td>
<td align="right">-0.9819429</td>
<td align="right">-0.9819429</td>
<td align="right">-0.9930962</td>
<td align="right">-0.9786900</td>
<td align="right">-0.9947332</td>
<td align="right">-0.9960283</td>
<td align="right">-0.9722931</td>
<td align="right">-0.9779373</td>
<td align="right">-0.9950738</td>
<td align="right">-0.9870182</td>
<td align="right">-0.9923498</td>
<td align="right">-0.9874971</td>
<td align="right">-0.9871077</td>
<td align="right">-0.9823453</td>
<td align="right">-0.9823138</td>
<td align="right">-0.9925360</td>
<td align="right">-0.9784661</td>
<td align="right">-0.9946711</td>
</tr>
<tr class="even">
<td align="left">1</td>
<td align="left">LAYING</td>
<td align="right">0.2215982</td>
<td align="right">-0.0405140</td>
<td align="right">-0.1132036</td>
<td align="right">-0.2488818</td>
<td align="right">0.7055498</td>
<td align="right">0.4458177</td>
<td align="right">0.0810865</td>
<td align="right">0.0038382</td>
<td align="right">0.0108342</td>
<td align="right">-0.0165531</td>
<td align="right">-0.0644861</td>
<td align="right">0.1486894</td>
<td align="right">-0.1072709</td>
<td align="right">-0.0415173</td>
<td align="right">-0.0740501</td>
<td align="right">-0.8419292</td>
<td align="right">-0.8419292</td>
<td align="right">-0.9543963</td>
<td align="right">-0.8747595</td>
<td align="right">-0.9634610</td>
<td align="right">-0.9390991</td>
<td align="right">-0.8670652</td>
<td align="right">-0.8826669</td>
<td align="right">-0.1587927</td>
<td align="right">0.0975348</td>
<td align="right">0.0894377</td>
<td align="right">-0.9570739</td>
<td align="right">-0.9224626</td>
<td align="right">-0.9480609</td>
<td align="right">0.1324191</td>
<td align="right">0.0245136</td>
<td align="right">0.0243879</td>
<td align="right">-0.8502492</td>
<td align="right">-0.9521915</td>
<td align="right">-0.9093027</td>
<td align="right">-0.0035468</td>
<td align="right">-0.0915291</td>
<td align="right">0.0104581</td>
<td align="right">-0.8617676</td>
<td align="right">0.0864086</td>
<td align="right">-0.9333004</td>
<td align="right">0.2663912</td>
<td align="right">-0.8621902</td>
<td align="right">-0.1397750</td>
<td align="right">-0.9423669</td>
<td align="right">0.1764859</td>
<td align="right">0.0213660</td>
<td align="right">0.0030604</td>
<td align="right">-0.0016670</td>
<td align="right">0.0844372</td>
<td align="right">0.4267062</td>
<td align="right">-0.5203438</td>
<td align="right">-0.3524131</td>
<td align="right">-0.9280565</td>
<td align="right">-0.8368274</td>
<td align="right">-0.8260614</td>
<td align="right">-0.8968300</td>
<td align="right">-0.9077200</td>
<td align="right">-0.8523663</td>
<td align="right">-0.9584821</td>
<td align="right">-0.9241493</td>
<td align="right">-0.9548551</td>
<td align="right">-0.8735439</td>
<td align="right">-0.9510904</td>
<td align="right">-0.9082847</td>
<td align="right">-0.9186085</td>
<td align="right">-0.9679072</td>
<td align="right">-0.9577902</td>
<td align="right">-0.7951449</td>
<td align="right">-0.7951449</td>
<td align="right">-0.9282456</td>
<td align="right">-0.8190102</td>
<td align="right">-0.9358410</td>
<td align="right">-0.9244374</td>
<td align="right">-0.8336256</td>
<td align="right">-0.8128916</td>
<td align="right">-0.9641607</td>
<td align="right">-0.9322179</td>
<td align="right">-0.9605870</td>
<td align="right">-0.8822965</td>
<td align="right">-0.9512320</td>
<td align="right">-0.9165825</td>
<td align="right">-0.7983009</td>
<td align="right">-0.9218040</td>
<td align="right">-0.8243194</td>
<td align="right">-0.9326607</td>
</tr>
</tbody>
</table>

``` r
kable(tail(final_dataset), format = "markdown", padding = 0, caption = "Final dataset")
```

<table style="width:100%;">
<colgroup>
<col width="0%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="2%" />
<col width="1%" />
<col width="2%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="0%" />
<col width="1%" />
<col width="1%" />
<col width="1%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">subject_test</th>
<th align="left">exercise_mode</th>
<th align="right">tbodyacc_mean_x</th>
<th align="right">tbodyacc_mean_y</th>
<th align="right">tbodyacc_mean_z</th>
<th align="right">tgravityacc_mean_x</th>
<th align="right">tgravityacc_mean_y</th>
<th align="right">tgravityacc_mean_z</th>
<th align="right">tbodyaccjerk_mean_x</th>
<th align="right">tbodyaccjerk_mean_y</th>
<th align="right">tbodyaccjerk_mean_z</th>
<th align="right">tbodygyro_mean_x</th>
<th align="right">tbodygyro_mean_y</th>
<th align="right">tbodygyro_mean_z</th>
<th align="right">tbodygyrojerk_mean_x</th>
<th align="right">tbodygyrojerk_mean_y</th>
<th align="right">tbodygyrojerk_mean_z</th>
<th align="right">tbodyaccmag_mean</th>
<th align="right">tgravityaccmag_mean</th>
<th align="right">tbodyaccjerkmag_mean</th>
<th align="right">tbodygyromag_mean</th>
<th align="right">tbodygyrojerkmag_mean</th>
<th align="right">fbodyacc_mean_x</th>
<th align="right">fbodyacc_mean_y</th>
<th align="right">fbodyacc_mean_z</th>
<th align="right">fbodyacc_meanfreq_x</th>
<th align="right">fbodyacc_meanfreq_y</th>
<th align="right">fbodyacc_meanfreq_z</th>
<th align="right">fbodyaccjerk_mean_x</th>
<th align="right">fbodyaccjerk_mean_y</th>
<th align="right">fbodyaccjerk_mean_z</th>
<th align="right">fbodyaccjerk_meanfreq_x</th>
<th align="right">fbodyaccjerk_meanfreq_y</th>
<th align="right">fbodyaccjerk_meanfreq_z</th>
<th align="right">fbodygyro_mean_x</th>
<th align="right">fbodygyro_mean_y</th>
<th align="right">fbodygyro_mean_z</th>
<th align="right">fbodygyro_meanfreq_x</th>
<th align="right">fbodygyro_meanfreq_y</th>
<th align="right">fbodygyro_meanfreq_z</th>
<th align="right">fbodyaccmag_mean</th>
<th align="right">fbodyaccmag_meanfreq</th>
<th align="right">fbodybodyaccjerkmag_mean</th>
<th align="right">fbodybodyaccjerkmag_meanfreq</th>
<th align="right">fbodybodygyromag_mean</th>
<th align="right">fbodybodygyromag_meanfreq</th>
<th align="right">fbodybodygyrojerkmag_mean</th>
<th align="right">fbodybodygyrojerkmag_meanfreq</th>
<th align="right">angle_tbodyaccmean_gravity</th>
<th align="right">angle_tbodyaccjerkmean_gravitymean</th>
<th align="right">angle_tbodygyromean_gravitymean</th>
<th align="right">angle_tbodygyrojerkmean_gravitymean</th>
<th align="right">angle_x_gravitymean</th>
<th align="right">angle_y_gravitymean</th>
<th align="right">angle_z_gravitymean</th>
<th align="right">tbodyacc_std_x</th>
<th align="right">tbodyacc_std_y</th>
<th align="right">tbodyacc_std_z</th>
<th align="right">tgravityacc_std_x</th>
<th align="right">tgravityacc_std_y</th>
<th align="right">tgravityacc_std_z</th>
<th align="right">tbodyaccjerk_std_x</th>
<th align="right">tbodyaccjerk_std_y</th>
<th align="right">tbodyaccjerk_std_z</th>
<th align="right">tbodygyro_std_x</th>
<th align="right">tbodygyro_std_y</th>
<th align="right">tbodygyro_std_z</th>
<th align="right">tbodygyrojerk_std_x</th>
<th align="right">tbodygyrojerk_std_y</th>
<th align="right">tbodygyrojerk_std_z</th>
<th align="right">tbodyaccmag_std</th>
<th align="right">tgravityaccmag_std</th>
<th align="right">tbodyaccjerkmag_std</th>
<th align="right">tbodygyromag_std</th>
<th align="right">tbodygyrojerkmag_std</th>
<th align="right">fbodyacc_std_x</th>
<th align="right">fbodyacc_std_y</th>
<th align="right">fbodyacc_std_z</th>
<th align="right">fbodyaccjerk_std_x</th>
<th align="right">fbodyaccjerk_std_y</th>
<th align="right">fbodyaccjerk_std_z</th>
<th align="right">fbodygyro_std_x</th>
<th align="right">fbodygyro_std_y</th>
<th align="right">fbodygyro_std_z</th>
<th align="right">fbodyaccmag_std</th>
<th align="right">fbodybodyaccjerkmag_std</th>
<th align="right">fbodybodygyromag_std</th>
<th align="right">fbodybodygyrojerkmag_std</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">30</td>
<td align="left">WALKING</td>
<td align="right">0.2764068</td>
<td align="right">-0.0175880</td>
<td align="right">-0.0986247</td>
<td align="right">0.9652176</td>
<td align="right">-0.1576738</td>
<td align="right">-0.0039256</td>
<td align="right">0.0688690</td>
<td align="right">0.0219657</td>
<td align="right">-0.0073953</td>
<td align="right">-0.0459505</td>
<td align="right">-0.0649171</td>
<td align="right">0.0839568</td>
<td align="right">-0.0873840</td>
<td align="right">-0.0617029</td>
<td align="right">-0.0446007</td>
<td align="right">-0.1951400</td>
<td align="right">-0.1951400</td>
<td align="right">-0.3521117</td>
<td align="right">-0.0229641</td>
<td align="right">-0.4720687</td>
<td align="right">-0.3514029</td>
<td align="right">-0.1938567</td>
<td align="right">-0.3095589</td>
<td align="right">-0.3463571</td>
<td align="right">0.0306217</td>
<td align="right">-0.0837533</td>
<td align="right">-0.3895956</td>
<td align="right">-0.2995253</td>
<td align="right">-0.4670307</td>
<td align="right">-0.2688778</td>
<td align="right">-0.2748389</td>
<td align="right">-0.2012297</td>
<td align="right">-0.3744403</td>
<td align="right">-0.1759009</td>
<td align="right">-0.2473503</td>
<td align="right">-0.2076857</td>
<td align="right">-0.3807315</td>
<td align="right">-0.2626507</td>
<td align="right">-0.3423638</td>
<td align="right">0.0483162</td>
<td align="right">-0.3471801</td>
<td align="right">0.1150108</td>
<td align="right">-0.3583444</td>
<td align="right">-0.0688872</td>
<td align="right">-0.5476218</td>
<td align="right">0.1023463</td>
<td align="right">0.0530153</td>
<td align="right">-0.0152830</td>
<td align="right">0.0157766</td>
<td align="right">-0.0720168</td>
<td align="right">-0.8613229</td>
<td align="right">0.1916256</td>
<td align="right">0.0244500</td>
<td align="right">-0.3463943</td>
<td align="right">-0.1735500</td>
<td align="right">-0.1204768</td>
<td align="right">-0.9797525</td>
<td align="right">-0.9701766</td>
<td align="right">-0.9439584</td>
<td align="right">-0.3744009</td>
<td align="right">-0.2707093</td>
<td align="right">-0.5213519</td>
<td align="right">-0.3879206</td>
<td align="right">0.0060028</td>
<td align="right">-0.1825697</td>
<td align="right">-0.4603454</td>
<td align="right">-0.4976218</td>
<td align="right">-0.4762088</td>
<td align="right">-0.3598734</td>
<td align="right">-0.3598734</td>
<td align="right">-0.3537510</td>
<td align="right">-0.2668457</td>
<td align="right">-0.5469773</td>
<td align="right">-0.3449281</td>
<td align="right">-0.2156343</td>
<td align="right">-0.0931448</td>
<td align="right">-0.4151354</td>
<td align="right">-0.2894661</td>
<td align="right">-0.5754103</td>
<td align="right">-0.3990323</td>
<td align="right">0.0955456</td>
<td align="right">-0.2379415</td>
<td align="right">-0.4698528</td>
<td align="right">-0.3665374</td>
<td align="right">-0.3315417</td>
<td align="right">-0.5785800</td>
</tr>
<tr class="even">
<td align="left">30</td>
<td align="left">WALKING_UPSTAIRS</td>
<td align="right">0.2714156</td>
<td align="right">-0.0253312</td>
<td align="right">-0.1246975</td>
<td align="right">0.9318298</td>
<td align="right">-0.2266473</td>
<td align="right">-0.0221401</td>
<td align="right">0.0579840</td>
<td align="right">-0.0035872</td>
<td align="right">0.0161506</td>
<td align="right">-0.0035597</td>
<td align="right">-0.0779606</td>
<td align="right">0.0814699</td>
<td align="right">-0.1084143</td>
<td align="right">-0.0141113</td>
<td align="right">-0.0364158</td>
<td align="right">-0.1376279</td>
<td align="right">-0.1376279</td>
<td align="right">-0.5966001</td>
<td align="right">-0.1136084</td>
<td align="right">-0.7187803</td>
<td align="right">-0.4204028</td>
<td align="right">-0.2978138</td>
<td align="right">-0.3675198</td>
<td align="right">-0.5902615</td>
<td align="right">-0.3338217</td>
<td align="right">-0.5122484</td>
<td align="right">-0.5506784</td>
<td align="right">-0.5929194</td>
<td align="right">-0.7378039</td>
<td align="right">-0.4436963</td>
<td align="right">-0.5367438</td>
<td align="right">-0.6261676</td>
<td align="right">-0.4880390</td>
<td align="right">-0.3660584</td>
<td align="right">-0.3189370</td>
<td align="right">-0.3369542</td>
<td align="right">-0.6427040</td>
<td align="right">-0.4836833</td>
<td align="right">-0.4005884</td>
<td align="right">-0.3123380</td>
<td align="right">-0.5497849</td>
<td align="right">-0.0484763</td>
<td align="right">-0.4491507</td>
<td align="right">-0.4566387</td>
<td align="right">-0.7739745</td>
<td align="right">-0.0714399</td>
<td align="right">-0.0001802</td>
<td align="right">0.0868940</td>
<td align="right">-0.0362012</td>
<td align="right">0.0174889</td>
<td align="right">-0.7901143</td>
<td align="right">0.2409754</td>
<td align="right">0.0373907</td>
<td align="right">-0.3505045</td>
<td align="right">-0.1273112</td>
<td align="right">0.0249468</td>
<td align="right">-0.9540336</td>
<td align="right">-0.9149339</td>
<td align="right">-0.8624028</td>
<td align="right">-0.5354202</td>
<td align="right">-0.5872145</td>
<td align="right">-0.7619420</td>
<td align="right">-0.4938375</td>
<td align="right">-0.0840482</td>
<td align="right">-0.2115736</td>
<td align="right">-0.7427495</td>
<td align="right">-0.7433370</td>
<td align="right">-0.6651506</td>
<td align="right">-0.3274108</td>
<td align="right">-0.3274108</td>
<td align="right">-0.5618377</td>
<td align="right">-0.1692935</td>
<td align="right">-0.7744391</td>
<td align="right">-0.3262604</td>
<td align="right">-0.1042992</td>
<td align="right">0.1214474</td>
<td align="right">-0.5615652</td>
<td align="right">-0.6108266</td>
<td align="right">-0.7847539</td>
<td align="right">-0.5034842</td>
<td align="right">0.0449546</td>
<td align="right">-0.2534271</td>
<td align="right">-0.3945081</td>
<td align="right">-0.5808781</td>
<td align="right">-0.1514723</td>
<td align="right">-0.7913494</td>
</tr>
<tr class="odd">
<td align="left">30</td>
<td align="left">WALKING_DOWNSTAIRS</td>
<td align="right">0.2831906</td>
<td align="right">-0.0174384</td>
<td align="right">-0.0999781</td>
<td align="right">0.9580005</td>
<td align="right">-0.1267104</td>
<td align="right">0.0288082</td>
<td align="right">0.0883933</td>
<td align="right">-0.0075611</td>
<td align="right">-0.0118301</td>
<td align="right">-0.0745591</td>
<td align="right">-0.0693112</td>
<td align="right">0.0895768</td>
<td align="right">-0.0615955</td>
<td align="right">-0.0496808</td>
<td align="right">-0.0543595</td>
<td align="right">-0.0373901</td>
<td align="right">-0.0373901</td>
<td align="right">-0.2937388</td>
<td align="right">-0.0955373</td>
<td align="right">-0.5743370</td>
<td align="right">-0.1069670</td>
<td align="right">-0.0216637</td>
<td align="right">-0.2580674</td>
<td align="right">-0.4633029</td>
<td align="right">-0.0640946</td>
<td align="right">0.0428810</td>
<td align="right">-0.2349231</td>
<td align="right">-0.2249928</td>
<td align="right">-0.3996887</td>
<td align="right">-0.3071167</td>
<td align="right">-0.4250517</td>
<td align="right">-0.1683407</td>
<td align="right">-0.2630616</td>
<td align="right">-0.3480651</td>
<td align="right">-0.2637208</td>
<td align="right">-0.3957702</td>
<td align="right">-0.2864671</td>
<td align="right">-0.2527629</td>
<td align="right">0.0041015</td>
<td align="right">-0.0724828</td>
<td align="right">-0.1259614</td>
<td align="right">-0.1252104</td>
<td align="right">-0.3567723</td>
<td align="right">-0.1759313</td>
<td align="right">-0.6175788</td>
<td align="right">0.0227158</td>
<td align="right">0.0065700</td>
<td align="right">-0.0615355</td>
<td align="right">0.0811338</td>
<td align="right">-0.1089736</td>
<td align="right">-0.8831362</td>
<td align="right">0.1709113</td>
<td align="right">0.0013913</td>
<td align="right">-0.0577703</td>
<td align="right">-0.0272628</td>
<td align="right">-0.2172757</td>
<td align="right">-0.9588904</td>
<td align="right">-0.9186788</td>
<td align="right">-0.8776671</td>
<td align="right">-0.2266439</td>
<td align="right">-0.1946561</td>
<td align="right">-0.4670702</td>
<td align="right">-0.2659232</td>
<td align="right">-0.2853924</td>
<td align="right">-0.2953704</td>
<td align="right">-0.5427645</td>
<td align="right">-0.6137963</td>
<td align="right">-0.4988829</td>
<td align="right">-0.0135771</td>
<td align="right">-0.0135771</td>
<td align="right">-0.1252832</td>
<td align="right">-0.2082675</td>
<td align="right">-0.6176621</td>
<td align="right">-0.0405257</td>
<td align="right">-0.0934683</td>
<td align="right">-0.2573934</td>
<td align="right">-0.2898027</td>
<td align="right">-0.2174320</td>
<td align="right">-0.5355150</td>
<td align="right">-0.2783866</td>
<td align="right">-0.2557321</td>
<td align="right">-0.3715415</td>
<td align="right">-0.1785352</td>
<td align="right">-0.1331243</td>
<td align="right">-0.2523619</td>
<td align="right">-0.6455039</td>
</tr>
<tr class="even">
<td align="left">30</td>
<td align="left">SITTING</td>
<td align="right">0.2683361</td>
<td align="right">-0.0080473</td>
<td align="right">-0.0995154</td>
<td align="right">0.8254738</td>
<td align="right">0.1145884</td>
<td align="right">0.3447660</td>
<td align="right">0.0760058</td>
<td align="right">0.0097569</td>
<td align="right">-0.0027816</td>
<td align="right">-0.0358426</td>
<td align="right">-0.0743536</td>
<td align="right">0.0702003</td>
<td align="right">-0.0952708</td>
<td align="right">-0.0407931</td>
<td align="right">-0.0488205</td>
<td align="right">-0.9574872</td>
<td align="right">-0.9574872</td>
<td align="right">-0.9877991</td>
<td align="right">-0.9558473</td>
<td align="right">-0.9937374</td>
<td align="right">-0.9850088</td>
<td align="right">-0.9540760</td>
<td align="right">-0.9662741</td>
<td align="right">-0.1227005</td>
<td align="right">0.0071382</td>
<td align="right">0.0927750</td>
<td align="right">-0.9887754</td>
<td align="right">-0.9804057</td>
<td align="right">-0.9859783</td>
<td align="right">0.1366891</td>
<td align="right">-0.1277838</td>
<td align="right">-0.0214875</td>
<td align="right">-0.9870346</td>
<td align="right">-0.9820059</td>
<td align="right">-0.9611771</td>
<td align="right">-0.0154022</td>
<td align="right">-0.3127816</td>
<td align="right">-0.0503841</td>
<td align="right">-0.9599234</td>
<td align="right">0.0705937</td>
<td align="right">-0.9858263</td>
<td align="right">0.3187685</td>
<td align="right">-0.9738763</td>
<td align="right">-0.1466085</td>
<td align="right">-0.9917507</td>
<td align="right">0.2166511</td>
<td align="right">-0.0151925</td>
<td align="right">0.0458754</td>
<td align="right">-0.0075999</td>
<td align="right">-0.0208555</td>
<td align="right">-0.5963054</td>
<td align="right">0.0010395</td>
<td align="right">-0.2275346</td>
<td align="right">-0.9836227</td>
<td align="right">-0.9378570</td>
<td align="right">-0.9506540</td>
<td align="right">-0.9783647</td>
<td align="right">-0.9593613</td>
<td align="right">-0.9566357</td>
<td align="right">-0.9888646</td>
<td align="right">-0.9804209</td>
<td align="right">-0.9881644</td>
<td align="right">-0.9881327</td>
<td align="right">-0.9764776</td>
<td align="right">-0.9550532</td>
<td align="right">-0.9938685</td>
<td align="right">-0.9924913</td>
<td align="right">-0.9881245</td>
<td align="right">-0.9429015</td>
<td align="right">-0.9429015</td>
<td align="right">-0.9860576</td>
<td align="right">-0.9606413</td>
<td align="right">-0.9912802</td>
<td align="right">-0.9832310</td>
<td align="right">-0.9339926</td>
<td align="right">-0.9462038</td>
<td align="right">-0.9900176</td>
<td align="right">-0.9819021</td>
<td align="right">-0.9889712</td>
<td align="right">-0.9884848</td>
<td align="right">-0.9738050</td>
<td align="right">-0.9573087</td>
<td align="right">-0.9435437</td>
<td align="right">-0.9852950</td>
<td align="right">-0.9595139</td>
<td align="right">-0.9909464</td>
</tr>
<tr class="odd">
<td align="left">30</td>
<td align="left">STANDING</td>
<td align="right">0.2771127</td>
<td align="right">-0.0170164</td>
<td align="right">-0.1087562</td>
<td align="right">0.9685567</td>
<td align="right">-0.1002968</td>
<td align="right">0.0243044</td>
<td align="right">0.0752414</td>
<td align="right">0.0120860</td>
<td align="right">0.0019084</td>
<td align="right">-0.0276139</td>
<td align="right">-0.0670334</td>
<td align="right">0.0802515</td>
<td align="right">-0.0997160</td>
<td align="right">-0.0437760</td>
<td align="right">-0.0520307</td>
<td align="right">-0.9305736</td>
<td align="right">-0.9305736</td>
<td align="right">-0.9712252</td>
<td align="right">-0.9138906</td>
<td align="right">-0.9729953</td>
<td align="right">-0.9720141</td>
<td align="right">-0.9194751</td>
<td align="right">-0.9380898</td>
<td align="right">-0.0532792</td>
<td align="right">-0.2378598</td>
<td align="right">-0.0288534</td>
<td align="right">-0.9678576</td>
<td align="right">-0.9574349</td>
<td align="right">-0.9628926</td>
<td align="right">0.0608341</td>
<td align="right">-0.2659804</td>
<td align="right">-0.0606642</td>
<td align="right">-0.9157429</td>
<td align="right">-0.9456288</td>
<td align="right">-0.9377981</td>
<td align="right">-0.3935262</td>
<td align="right">-0.3023487</td>
<td align="right">-0.2486312</td>
<td align="right">-0.9320475</td>
<td align="right">-0.1115002</td>
<td align="right">-0.9533576</td>
<td align="right">0.0494753</td>
<td align="right">-0.9174494</td>
<td align="right">-0.3183091</td>
<td align="right">-0.9592422</td>
<td align="right">-0.1317205</td>
<td align="right">0.0172814</td>
<td align="right">0.0041669</td>
<td align="right">0.0515531</td>
<td align="right">0.0328842</td>
<td align="right">-0.8781591</td>
<td align="right">0.1522859</td>
<td align="right">0.0046999</td>
<td align="right">-0.9775594</td>
<td align="right">-0.8916545</td>
<td align="right">-0.9128506</td>
<td align="right">-0.9964209</td>
<td align="right">-0.9581458</td>
<td align="right">-0.9492074</td>
<td align="right">-0.9684307</td>
<td align="right">-0.9573189</td>
<td align="right">-0.9688973</td>
<td align="right">-0.9114085</td>
<td align="right">-0.9407054</td>
<td align="right">-0.9308347</td>
<td align="right">-0.9601191</td>
<td align="right">-0.9681350</td>
<td align="right">-0.9708457</td>
<td align="right">-0.9165704</td>
<td align="right">-0.9165704</td>
<td align="right">-0.9507623</td>
<td align="right">-0.8872476</td>
<td align="right">-0.9559957</td>
<td align="right">-0.9804266</td>
<td align="right">-0.8855728</td>
<td align="right">-0.9072868</td>
<td align="right">-0.9722167</td>
<td align="right">-0.9604424</td>
<td align="right">-0.9739543</td>
<td align="right">-0.9117182</td>
<td align="right">-0.9394698</td>
<td align="right">-0.9351769</td>
<td align="right">-0.9217332</td>
<td align="right">-0.9466398</td>
<td align="right">-0.8888722</td>
<td align="right">-0.9550086</td>
</tr>
<tr class="even">
<td align="left">30</td>
<td align="left">LAYING</td>
<td align="right">0.2810339</td>
<td align="right">-0.0194494</td>
<td align="right">-0.1036582</td>
<td align="right">-0.3447378</td>
<td align="right">0.7326612</td>
<td align="right">0.6814592</td>
<td align="right">0.0752197</td>
<td align="right">0.0107680</td>
<td align="right">-0.0003742</td>
<td align="right">-0.0267812</td>
<td align="right">-0.0761476</td>
<td align="right">0.0938472</td>
<td align="right">-0.1022774</td>
<td align="right">-0.0384876</td>
<td align="right">-0.0595737</td>
<td align="right">-0.9698300</td>
<td align="right">-0.9698300</td>
<td align="right">-0.9792328</td>
<td align="right">-0.9622849</td>
<td align="right">-0.9850864</td>
<td align="right">-0.9747900</td>
<td align="right">-0.9599743</td>
<td align="right">-0.9703220</td>
<td align="right">-0.2229584</td>
<td align="right">0.0705691</td>
<td align="right">0.2302698</td>
<td align="right">-0.9768879</td>
<td align="right">-0.9716963</td>
<td align="right">-0.9756324</td>
<td align="right">0.0410382</td>
<td align="right">-0.1419612</td>
<td align="right">0.0650537</td>
<td align="right">-0.9717891</td>
<td align="right">-0.9681703</td>
<td align="right">-0.9675774</td>
<td align="right">-0.0661794</td>
<td align="right">-0.2399248</td>
<td align="right">0.0674681</td>
<td align="right">-0.9628408</td>
<td align="right">0.1760172</td>
<td align="right">-0.9699493</td>
<td align="right">0.1721211</td>
<td align="right">-0.9620012</td>
<td align="right">-0.0802049</td>
<td align="right">-0.9778213</td>
<td align="right">0.1072526</td>
<td align="right">-0.0106131</td>
<td align="right">0.0369006</td>
<td align="right">0.0372059</td>
<td align="right">0.0173555</td>
<td align="right">0.4998487</td>
<td align="right">-0.4922870</td>
<td align="right">-0.5070199</td>
<td align="right">-0.9763625</td>
<td align="right">-0.9542018</td>
<td align="right">-0.9670442</td>
<td align="right">-0.9795639</td>
<td align="right">-0.9889307</td>
<td align="right">-0.9832745</td>
<td align="right">-0.9774638</td>
<td align="right">-0.9710498</td>
<td align="right">-0.9795179</td>
<td align="right">-0.9736628</td>
<td align="right">-0.9660417</td>
<td align="right">-0.9688892</td>
<td align="right">-0.9837758</td>
<td align="right">-0.9803571</td>
<td align="right">-0.9807689</td>
<td align="right">-0.9601679</td>
<td align="right">-0.9601679</td>
<td align="right">-0.9696423</td>
<td align="right">-0.9512644</td>
<td align="right">-0.9761771</td>
<td align="right">-0.9770453</td>
<td align="right">-0.9535584</td>
<td align="right">-0.9671882</td>
<td align="right">-0.9803507</td>
<td align="right">-0.9724342</td>
<td align="right">-0.9822816</td>
<td align="right">-0.9744884</td>
<td align="right">-0.9651342</td>
<td align="right">-0.9721992</td>
<td align="right">-0.9640518</td>
<td align="right">-0.9680878</td>
<td align="right">-0.9526444</td>
<td align="right">-0.9754815</td>
</tr>
</tbody>
</table>
