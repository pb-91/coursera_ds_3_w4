#load libraries
library(dplyr)
library(data.table)

# read the header of the datasets
header_df <- fread("UCI HAR Dataset/features.txt", header = FALSE)

# read train and test dataset
train_df <- fread("UCI HAR Dataset/train/X_train.txt", header=FALSE)
test_df <- fread("UCI HAR Dataset/test/X_test.txt", header=FALSE)

# merge train and test dataset
df_merged <- bind_rows(train_df, test_df)
names(df_merged) <- header_df$V2

# read train and test labels
train_labels <- fread("UCI HAR Dataset/train/y_train.txt", header = FALSE)
test_labels <- fread("UCI HAR Dataset/test/y_test.txt", header=FALSE)

# merge train and test labels
labels <- bind_rows(train_labels, test_labels)
names(labels) <- "activity"

# replace the label numbers into activity names
activity_labels <- fread("UCI HAR Dataset/activity_labels.txt", header=FALSE)
replaceLabel <- function(x){x = activity_labels$V2[x]}
labels$activity <- sapply(labels$activity, replaceLabel)

# merge labels with the dataframe
df_w_lab <- cbind(df_merged, labels)

# extracts mean-s and std-s
col_indices <- grep("mean|std|activity", names(df_w_lab), value=TRUE)
df <- df_w_lab[,..col_indices]

# read train and test subjects
subject_train <- fread("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
subject_test <- fread("UCI HAR Dataset/test/subject_test.txt", header=FALSE)

# merge train and test subjects
subject <- bind_rows(subject_train, subject_test)
names(subject) <- "subject"

# merge subjects with the dataframe
df_w_sub <- cbind(df, subject)

# calculate the average of each variable for each activity and each subject.
df_mean_by_subject <- df_by_subject %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean))