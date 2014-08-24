run_analysis <- function(){
        #read in all test files.
        x.test = read.table("UCI HAR Dataset/test/X_test.txt")
        y.test = read.table("UCI HAR Dataset/test/y_test.txt")
        subject.test = read.table("UCI HAR Dataset/test/subject_test.txt")
        #read in all train files.
        x.train = read.table("UCI HAR Dataset/train/X_train.txt")
        y.train = read.table("UCI HAR Dataset/train/y_train.txt")
        subject.train = read.table("UCI HAR Dataset/train/subject_train.txt")
        #read in the feature listing file
        features = read.table("UCI HAR Dataset/features.txt")
        activities = read.table("UCI HAR Dataset//activity_labels.txt")
        
        #merge training and test files
        combined.x = rbind(x.test, x.train)
        combined.y = rbind(y.test, y.train)
        combined.subject = rbind(subject.test, subject.train)
        
        #name columns of combined.x dataframe.
        #setNames(as.data.frame(combined.x),features$V2)
        names(combined.x) = features$V2
        
        #build comparison and remove columns that are not needed.
        features.mean <- grepl(glob2rx("*mean*"), features$V2)
        features.std <- grepl(glob2rx("*std*"), features$V2)
        features.good <- features.mean | features.std
        combined.x = subset(combined.x, select = features.good)
        
        #trim activity strings
        activity <- sub("_", "", activities[1,2])
        
        #replace numbers with descriptions combined.y dataframe
        y <- sub(activities[1,1],activity,combined.y$V1)
        for(i in 2:nrow(activities)){
                activity <- sub("_", "", activities[i,2]) #trim activity strings
                y <- sub(activities[i,1],activity,y)
        }
        combined.y <- data.frame(y)
        
        #name remaining columns
        names(combined.y) = "activity"
        names(combined.subject) = "subject"
        
        
        #merge the merged files using rbind
        tidy_data = cbind(combined.subject,combined.y, combined.x)
        
        write.table(tidy_data, file="tidy.txt")
        
        #5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
        good_names <- names(tidy_data)
        tidy_averages <- aggregate(tidy_data[,3] ~tidy_data$subject + tidy_data$activity, data=tidy_data, FUN=mean)
        for(i in 4:ncol(tidy_data)){
                tidy_averages[,i] <- aggregate(tidy_data[,i] ~tidy_data$subject + tidy_data$activity, data=tidy_data, FUN=mean)
        }
        
        names(tidy_averages) <- good_names
        
        write.table(tidy_averages, file="tidy_averages.txt")
}





