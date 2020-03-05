# 1. Merges the training and the test sets to create one data set.
## Uses library(plyr); library(dplyr).  Set as working directory the directory external to "UCI HAR Dataset" folder

## Read data
testdata <- read.table("./UCI HAR Dataset/test/X_test.txt")
traindata <- read.table("./UCI HAR Dataset/train/X_train.txt")

## Read names, labels and subjects
datanames <- read.table("./UCI HAR Dataset/features.txt")
testlabels <- read.table("./UCI HAR Dataset/test/y_test.txt")
trainlabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

alldata <- tbl_df(rbind(testdata, traindata))          # merges the data sets
names(alldata) <- paste(datanames[,1],datanames[,2])   # adds names to the variables

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

dataextract <- select(alldata, matches("mean()|std()"))
dataextract <- select(dataextract, -matches("meanFreq()|angle"))

# 3. Uses descriptive activity names to name the activities in the data set
ftest <- factor(testlabels$V1, levels = activities$V1, labels = activities$V2)
ftrain <- factor(trainlabels$V1, levels = activities$V1, labels = activities$V2)

dataextract <- cbind(dataextract, rbind(tbl_df(as.character(ftest)), tbl_df(as.character(ftrain))))   # Adds activity variable to the dataset

# 4. Appropriately labels the data set with descriptive variable names.
n <- names(dataextract) %>% tolower               # Converts names to lowercase
n <- sapply(strsplit(n," "), function(x){x[2]})   # Removes the numeric part of the names
n <- gsub("-","",n)                               # Removes "-" from the names
n <- gsub("\\()","",n)                            # Removes "()" from the names
n[67] <- "activity"

names(dataextract) <- n

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subject <- rbind(subjecttest,subjecttrain)
names(subject) <- "subject"
dataextract2 <- cbind(dataextract,as.data.frame(subject))                   # Adds subject variable to the dataset
dataextract2 <- ddply(dataextract2,.(activity, subject), colwise(mean))
write.table(dataextract2, file = "./UCI HAR Dataset/mean_activity_subject.txt", row.names = FALSE)
