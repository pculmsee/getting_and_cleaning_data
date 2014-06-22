run_analysis <- function() {
        
        require("data.table")
        require("reshape2")
        
        # load activities
        activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
        names(activity_labels) = c("activity_id", "activity_label")
        
        # load features
        features <- read.table("./UCI HAR Dataset/features.txt")[,2]
        
        # get mean and sd features
        extract_features <- grepl("mean|std", features)
        data_labels <- features[extract_features] 
        
        # Load test data.
        x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
        names(x_test) <- features
        
        y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
        names(y_test)<-"activity_id"
        y_test[,2]<- activity_labels[y_test[,1],2]
        names(y_test)[2]<-"activity_label"
        
        subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        names(subject_test) = "subject"
        
        # create the test data set
        test_data <- cbind(subject_test, y_test, x_test)
        
        # Load training data.
        x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
        names(x_train) <- features
        
        y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
        names(y_train)<-"activity_id"
        y_train[,2]<- activity_labels[y_train[,1],2]
        names(y_train)[2]<-"activity_label"
        
        subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        names(subject_train) = "subject"
        
        # create the training data set
        train_data <- cbind(subject_train, y_train, x_train)
        
        # merge test and training - this is the first dataset
        ds1 <- rbind(test_data, train_data)
        
        # Extract only the measurements on the mean and standard deviation for each measurement.
        # This is the second dataset
        ds2 <- ds1[,c(TRUE,TRUE,TRUE,extract_features)]
        
        #Melt it so we can summarise
        id_cols <- c("subject", "activity_id", "activity_label")
        dsMelt<- melt(ds2, id = id_cols,  measure.vars = data_labels) 
        
        # Get the averages
        ds3 <- dcast(dsMelt, subject + activity_label ~ variable, mean)
        names(ds3)<- c("subject", "activity_label",paste("average-",features[extract_features],sep = ""))
        
        write.table(ds3, file = "./tidy_data_set_2.txt")
        return(ds3)
        
}