## File: run_analysis.R

##Loading the needed library
##################################

library(dplyr)

##set your working directory
###############################

setwd(file.path("C:","Users","Shubham","Documents","GitHub","Getting-and-Cleaning-Data-Coursera"))


## Step: To get the data.
###################################

############################################################################################################
# download zip file containing data if it hasn't already been downloaded

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, destfile = zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

#######################################################################################################


##Step 0: Read the data
################################

###############################################################################
##  I will expain the data set here: 
##  Section 1:
## we have 3 objects here. The object trainingsubjects take the ids of the people
##  who were a part of the training group. trainingvalues take the 561 vector values as rows.
##  and trainingactvity takes in the trainging precudeure as in 1 to 6(walking,etc)
## Note: The number of columns is same for all because lets say the first entry values be:
## the id number as 5(example) from the first object. Now from the second object it has
## mapped the 561 vector values for this id  test activity of walking.
##
## same for section 2
##
##section 3: the name of the 561 vection values. as.in is used for non conversion into factors.
##  Note: it has two columns the first is just numbering and the second is the name of the vectors
##
## section 4: reads the names of activity lables(1 for walking etc)
#############################################################################

# section 1: read training data

trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# section 2: read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))


# section 3: read features, don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
## note: feature names (in features[, 2]) are not unique
##       e.g. fBodyAcc-bandsEnergy()-1,8

# section 4: read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

#################################################################################


##Step 1: Merging the training and test data sets
###########################################################

#################################################################################
# concatenate individual data tables to make single data table

## the column bind operations make a complete dataset for training and test respec.
## lets consider training, we have the ids, values corresponding(561 value vactor)
## to id, and the related activitity. Same for test.
## Next we rowbind this to create a complete dataset.


humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory

rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
## we have only 563(1+561+1) coloums now, subject id, 
## and the as activity as mentioned(1-6)

colnames(humanActivity) <- c("subject", features[, 2], "activity")

##############################################################################



# Step 2 - Extract only the measurements on the mean and 
##  standard deviation for each measurement(this is stated in the question )
#############################################################################

##############################################################################

# determine columns of data set to keep based on column name...
## we are using the regular experssion"|" which is 'or'.
## we are rearching the colnames vectors to find it as separate those out.
##in additon we included the subject and activity, because grepl will
## return a logical vector. We need true for this too

columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# ... and keep data in these columns only

humanActivity <- humanActivity[, columnsToKeep]

###############################################################################

##Step 3 - Using descriptive activity names
##        to name the activities in the data set.
##############################################################

##############################################################################

# replace activity values with named factor levels

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


#################################################################################

# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

###################################################################################
# get column names

humanActivityCols <- colnames(humanActivity)

# remove special characters: gsub is used for pattern replacement)

humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names

humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

##########################################################################################

# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

###########################################################################################

# group by subject and activity and summarise using mean

humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean)) 

# output to file "tidy_data.txt"

write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)