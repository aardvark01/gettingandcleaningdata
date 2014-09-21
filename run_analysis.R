#load packages
install.packages("data.table")
library("data.table")

run_analysis <- function(){
        #check if data directory exists
        if(!file.exists("UCI HAR Dataset")){
                temp <- tempfile()
                fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                download.file(fileUrl, temp)
                unzip(temp)
                rm(temp)
                rm(fileUrl)
        }
        #set the working directory
        setwd("UCI HAR Dataset/")
        
        #read in the feature listing file
        features = read.table("features.txt", col.names = c("id","label"))

        #build comparison and remove columns that are not needed.
        featmean <- grepl(glob2rx("*mean*"), features$label)
        featstd <- grepl(glob2rx("*std*"), features$label)
        goodFeatures <- featmean | featstd
        rm(featmean)
        rm(featstd)
        selectFeatures <- features[goodFeatures,]
        rm(features)
        
        
        #read in the activities
        activities = read.table("activity_labels.txt", col.names = c("id","label"))

        ##################################################
        #deal with testing data
        #read in all test files.
        xtest <- read.table("test/X_test.txt")[,selectFeatures$id]
        ytest <- read.table("test/y_test.txt")
        stest <- read.table("test/subject_test.txt")
        
        #name the columns
        names(xtest) <- selectFeatures$label
        names(ytest) <- "activity"
        names(stest) <- "subject"
        
        #merge test variables
        test <- cbind(stest, ytest, xtest)
        
        #remove unused test variables
        rm(xtest)
        rm(ytest)
        rm(stest)
        
        
        ##################################################
        #deal with training data
        #read in all train files.
        xtrain = read.table("train/X_train.txt")[,selectFeatures$id]
        ytrain = read.table("train/y_train.txt")
        strain = read.table("train/subject_train.txt")
        
        #name the columns
        names(xtrain) <- selectFeatures$label
        names(ytrain) <- "activity"
        names(strain) <- "subject"
        
        #merge test variables
        train <- cbind(strain, ytrain, xtrain)
        
        #remove unused test variables
        rm(xtrain)
        rm(ytrain)
        rm(strain)

        
        ##################################################
        #combine training and testing data
        combined <- rbind(test, train)
        
        #remove unused variables
        rm(test)
        rm(train)
        
        #replace activity numbers with activity names
        combined$activity <- factor(combined$activity, levels=activities$id, labels = activities$label)
        
        rm(activities)
        rm(selectFeatures)
        
        
        tidy_data <-data.frame(combined)
        tidy_names <- names(combined)
        
        write.table(tidy_data, file="../tidy.txt")
        
        #5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
        tidy_averages <- aggregate(tidy_data[,3] ~tidy_data$subject + tidy_data$activity, data=tidy_data, FUN=mean)
        for(i in 4:ncol(tidy_data)){
                tidy_averages[,i] <- aggregate(tidy_data[,i] ~tidy_data$subject + tidy_data$activity, data=tidy_data, FUN=mean)
        }
        
        names(tidy_averages) <- tidy_names
        
        write.table(tidy_averages, file="tidy_averages.txt")
        }
}





