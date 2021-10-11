# Setting workplace
setwd("C:/Users/amoon/Desktop/JohnsHopkins/Getting_and_Cleaning_Data/")

# 1. Merges the training and the test sets to create one data set.
trainData <- read.table("./Dataset/train/X_train.txt")
dim(trainData) 
head(trainData)

trainLabel <- read.table("./Dataset/train/y_train.txt")
table(trainLabel)

trainSubject <- read.table("./Dataset/train/subject_train.txt")

testData <- read.table("./Dataset/test/X_test.txt")
dim(testData) 

testLabel <- read.table("./Dataset/test/y_test.txt") 
table(testLabel) 

testSubject <- read.table("./Dataset/test/subject_test.txt")

joinData <- rbind(trainData, testData)
dim(joinData)

joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel)

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject)

# 2. Extracts only the measurements on the mean and standard deviation for each
# measurement. 
features <- read.table("./Dataset/features.txt")
dim(features)

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)

joinData <- joinData[, meanStdIndices]
dim(joinData)

names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
head(joinData)

# 3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./Dataset/activity_labels.txt")
head(activity)

activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names. 
names(joinSubject) <- "subject"

cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData)
head(cleanedData)

write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# 5. Creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject. 
subjectLen <- length(table(joinSubject))
activityLen <- dim(activity)[1]
columnLen <- dim(cleanedData)[2]

result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, "data_with_means.txt") # write out the 2nd dataset
