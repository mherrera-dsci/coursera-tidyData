### Course Proyect - Getting and Cleaning Data

setwd("~/Mis documentos registrados/Coursera/0300-Getting and Cleaning Data/900-Course Project")
rm(list=ls())

## Define Konstants
K_HOME_DATADIR<- "./UCI\ HAR\ Dataset"

# Define Constants(K*) for files in Test DataSet
K_TEST_DS_DIR = paste(K_HOME_DATADIR, "test", sep = "/")
K_XTEST_DS = paste(K_TEST_DS_DIR, "X_test.txt",sep = "/")
K_YTEST_DS = paste(K_TEST_DS_DIR, "y_test.txt",sep = "/")
K_SUBJECT_TEST_DS = paste(K_TEST_DS_DIR, "subject_test.txt",sep = "/")

# Define Konstants for files in Train DataSet

K_TRAIN_DS_DIR = paste(K_HOME_DATADIR, "train", sep = "/")
K_XTRAIN_DS = paste(K_TRAIN_DS_DIR, "X_train.txt",sep = "/")
K_YTRAIN_DS = paste(K_TRAIN_DS_DIR, "y_train.txt",sep = "/")
K_SUBJECT_TRAIN_DS = paste(K_TRAIN_DS_DIR, "subject_train.txt",sep = "/")

# Define constants for others files
K_ACTIVITY_LABELS_FILE <-  paste(K_HOME_DATADIR, "activity_labels.txt", sep = "/")
K_FEATURES_FILE <-paste(K_HOME_DATADIR, "features.txt", sep = "/")

### Step 1: Merges the training and the test sets to create one data set.

# The original dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.
# This step is to merge this datasets


### Step 0: Prepare scenario

# Read globals

activityLabels = read.csv(K_ACTIVITY_LABELS_FILE, sep="", header=FALSE)
features = read.csv(K_FEATURES_FILE, sep="", header=FALSE)

# Read Test data
xTestDF <- read.csv(file = K_XTEST_DS, sep="", header = FALSE, stringsAsFactors=FALSE )
yTestDF <- read.csv(file = K_YTEST_DS, header = FALSE, sep = " ", stringsAsFactors=FALSE )
subjectTestDF<- read.csv(file = K_SUBJECT_TEST_DS, header = FALSE, sep = " ", stringsAsFactors=FALSE )

# Read Train data
xTrainDF <- read.table(file = K_XTRAIN_DS, sep="", header = FALSE, stringsAsFactors=FALSE )
yTrainDF <- read.csv(file = K_YTRAIN_DS, header = FALSE, sep = " ", stringsAsFactors=FALSE )
subjectTrainDF<- read.csv(file = K_SUBJECT_TRAIN_DS, header = FALSE, sep = " ", stringsAsFactors=FALSE )

# Complete the data with subject and activities plus xData
trainDF <- cbind(subjectTrainDF, yTrainDF,xTrainDF)
testDF <- cbind(subjectTestDF, yTestDF,xTestDF)


### Step 1: Merges the training and the test sets to create one data set.

# Append the tain data to test data to obtain a whole dataset
dataSet <- rbind(testDF, trainDF)

# Rename the fisrt 2 columns with pnemotecnicals names
names(dataSet)[1] <- "Subject"
names(dataSet)[2] <- "Activity"


### Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

# Transform names of features for readability
features[,2] = gsub("-mean", "Mean", features[,2])
features[,2] = gsub("-std", "Std", features[,2])
features[,2] = gsub("[-()]", "", features[,2])

# Obtain an idex with only the columns Mean and Std
featuresIndex <- grep(".*Mean.*|.*Std.*", features[,2])

# Redefine features wuth the index in the scope
features <- features[c(featuresIndex),]

# Create an idex for data including the new 2 cols Subject ad Activity
dataIndex <-c(1,2, (featuresIndex + 2))

# Obtain the subset of columns
dataSet <- dataSet[,dataIndex]


### Step 3: Uses descriptive activity names to name the activities in the data set

indxActivity = 1
for (indxActivityLabel in activityLabels$V2) {
        dataSet$Activity <- gsub(indxActivity, indxActivityLabel, dataSet$Activity)
        indxActivity <- indxActivity + 1
}

View(dataSet)

### Step 4: Appropriately labels the data set with descriptive variable names.


colnames(dataSet) <- c("Subject", "Activity", features$V2)

### Step 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable
### for each activity and each subject.

dataSet$Activity <- as.factor(dataSet$Activity)
dataSet$Subject <- as.factor(dataSet$Subject)


tidyDataSet = with (dataSet, table(Subject, Activity))

write.table(tidyDataSet, "tidyDataSet.txt", row.name=FALSE)

