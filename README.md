# Getting & Cleaning Data

## Course Project

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Steps to reproduce this course project

1. Download the data source from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. Unzip Data.
3. Source run_analysis.R 
3. Set working directory to the directory where you have unzipped the UCI HAR Dataset directory.
4. Run run_analysis(). This will generate the file tidy_data_set_2.txt in your working directory.

## Dependencies

run_analysis.R depends on reshape2 and data.table. 
