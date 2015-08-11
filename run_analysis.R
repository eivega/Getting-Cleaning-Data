### Step 1  Merges and test ###
setwd("~/Desktop/Coursera//Getting-Cleaning-Data")
trainData <- read.table("./GCD/train/X_train.txt")
dim(trainData)
head(trainData)
trainLabel <- read.table("./GCD/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./GCD/train/subject_train.txt")
testData <- read.table("./GCD/test/X_test.txt")
dim(testData)
testLabel <- read.table("./GCD/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./GCD/test/subject_test.txt")
joinData <- rbind(trainData, testData)
dim(joinData)
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel)
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) 

### Step 2 Extract mean and standard ###
features <- read.table("./GCD/features.txt")
dim(features)
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)
joinData <- joinData[, meanStdIndices]
dim(joinData)
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) #Remove parenthesis
names(joinData) <- gsub("mean", "Mean", names(joinData)) #Capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # Capitalize S
names(joinData) <- gsub("-", "", names(joinData)) #Remove - in between columns names
### Step 3 Use descriptive ###
activity <- read.table("./GCD/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

### Step 4 Labels with descriptive names ###
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData)
write.table(cleanedData, "merged_data.txt") #Writes the 1st.data set

### Step 5 Creation of tidy data set ###
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
write.table(result, "data_with_means.txt") #Write 2nd data set
