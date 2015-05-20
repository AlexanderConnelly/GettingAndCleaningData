## This anlalysis will take the raw data from the wearable computing public data,
## The UCI Machine Leaning Repository ran this test on a group of 30 people using 6 activities while holding 
##the smart phone in their on their person.
## A full description of this data set is available at the following URL: 
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
#######################################################################################
#######################################################################################

## This function will accomplish the following:

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the average
## of each variable for each activity and each subject..

library(dplyr)

## First, this function will read in the data from the fixed width file format .txt's 

## Read in Column Names from features.csv (converted from features.txt)
    col_names<- read.table(file="C:/Users/a.connelly.SIGMACUBED/Google Drive/Coursera/Course Work/Getting And Cleaning Data/Course Project/UCI HAR Dataset/features.txt",header=FALSE, sep=" ")
    col_names<-as.vector(col_names, mode="list")
    
## Read in xtest and xtrain data and append colnames to it into 2 seperate data frames
    x_test<-read.csv(file="C:/Users/a.connelly.SIGMACUBED/Google Drive/Coursera/Course Work/Getting And Cleaning Data/Course Project/UCI HAR Dataset/test/x_test.csv",header=FALSE, col.names=col_names[,2])           
    x_train<-read.csv(file="C:/Users/a.connelly.SIGMACUBED/Google Drive/Coursera/Course Work/Getting And Cleaning Data/Course Project/UCI HAR Dataset/train/x_train.csv",header=FALSE, col.names=col_names[,2]) 

## Read in y_train and y_test and append it as a new column called subject to x_train and x_test
    y_test<-read.table(file="C:/Users/a.connelly.SIGMACUBED/Google Drive/Coursera/Course Work/Getting And Cleaning Data/Course Project/UCI HAR Dataset/test/y_test.txt",header=FALSE,col.names="activity")
    y_train<-read.table(file="C:/Users/a.connelly.SIGMACUBED/Google Drive/Coursera/Course Work/Getting And Cleaning Data/Course Project/UCI HAR Dataset/train/y_train.txt",header=FALSE,col.names="activity")

## Read in subject number from subject_test.txt for train and test data sets
    subject_test<-read.table(file="C:/Users/a.connelly.SIGMACUBED/Google Drive/Coursera/Course Work/Getting And Cleaning Data/Course Project/UCI HAR Dataset/test/subject_test.txt",header=FALSE,col.names="subject")
    subject_train<-read.table(file="C:/Users/a.connelly.SIGMACUBED/Google Drive/Coursera/Course Work/Getting And Cleaning Data/Course Project/UCI HAR Dataset/train/subject_train.txt",header=FALSE,col.names="subject")

## Now to merge the datasets all into one big data frame, starting by mergeing columns
## Activity column number:
    x_test$activity<-y_test$activity
    x_train$activity<-y_train$activity
## Subject column number:
    x_test$subject<-subject_test$subject
    x_train$subject<-subject_train$subject

## Then, adding the xtest and xtrain data tables end to end using rbind:
    main.data<-rbind(x_train,x_test)

## Extract the mean and standard deviation columns by names containing "mean" or "std":
col_names<-tbl_df(col_names)
col_names.mean<-filter(col_names,grepl('mean|std',V2))
col_names.int<-col_names.mean[,1]

##Use dplyr to select columns 
main.data<-tbl_df(main.data)
mean.data<-select(main.data, 1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,294,295,296,345,346,347,348,349,350,373,374,375,424,425,426,427,428,429,452,453,454,503,504,513,516,517,526,529,530,539,542,543,552,562,563)

##Cylce thru the activity # column and add to the activityname column a descriptive name EG walking, walking upstairs, ect
activity.num<-mean.data$activity
activity.name<-data.frame(matrix(nrow=10299,ncol=1))
names(activity.name)<-c("activity.name")

for (i in 1:10299){
  if (activity.num[i]== 1){
    activity.name[i,1]<-"Walking"
  }
  if (activity.num[i]== 2){
    activity.name[i,1]<-"Walking Upstairs"
  }
  if (activity.num[i]== 3){
    activity.name[i,1]<-"Walking Downstairs"
  }
  if (activity.num[i]== 4){
    activity.name[i,1]<-"Sitting"
  }
  if (activity.num[i]== 5){
    activity.name[i,1]<-"Standing"
  }
  if (activity.num[i]== 6){
    activity.name[i,1]<-"Laying"
  }  
}
##Now attach new column to mean.data:
mean.data$activity.name<-activity.name$activity.name

##group data by subject and then activity to calculate the means (step 5)

  mean.data %>% group_by(activity.name) %>% summarise_each(funs(mean))->activity.sum
  mean.data %>% group_by(subject) %>% summarise_each(funs(mean))->subject.sum

##Stack them into one dataset:
tidy.dataset<-bind_rows(activity.sum,subject.sum)
##reorder columns into tidy dataset as per the principlals of tidy data 
front<-select(tidy.dataset,activity.name,subject)
back<-select(tidy.dataset,-(activity.name),-(subject),-activity)
tidy.dataset<-bind_cols(front,back)

##Submit the Tidy Dataset:
write.table(tidy.dataset,file="tidyDataset.txt",row.name=FALSE)

View(tidy.dataset)
