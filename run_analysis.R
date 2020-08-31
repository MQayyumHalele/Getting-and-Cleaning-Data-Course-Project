library(data.table)

#data downloading----
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if(!file.exists('./UCI HAR Dataset.zip')){
  download.file(url, './UCI HAR Dataset.zip', mode = 'wb')
  unzip('./UCI HAR Dataset.zip', exdir = getwd())
}
#

#data reading and preparation----
##activity_labels.txt
activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt',
                              header = FALSE)
activity_labels <- as.character(activity_labels[, 2])

##features.txt
features <- read.csv('./UCI HAR Dataset/features.txt', 
                     header = FALSE, sep = ' ')
features <- as.character(features[, 2])

##test folder
X_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
y_test <- read.csv('./UCI HAR Dataset/test/y_test.txt', 
                   header = FALSE, sep = ' ')
subject_test <- read.csv('./UCI HAR Dataset/test/subject_test.txt', 
                         header = FALSE, sep = ' ')

test <-  data.frame(subject_test, y_test, X_test)
names(test) <- c(c('subject', 'activity'), features)

##train folder
X_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <- read.csv('./UCI HAR Dataset/train/y_train.txt',
                    header = FALSE, sep = ' ')
subject_train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',
                          header = FALSE, sep = ' ')

train <-  data.frame(subject_train, y_train, X_train)
names(train) <- c(c('subject', 'activity'), features)
#

#1. data merging----
merged <- rbind(train, test)
#

#2. mean|standard deviation extraction----
extractor <- grep('mean\\(\\)|std\\(\\)', features)
extracted <- merged[, c(1, 2, extractor + 2)]
#

#3. activity naming----
extracted$activity <- activity_labels[extracted$activity]
#4. variable relabeling----
name.new <- names(extracted)
name.new <- gsub("^t", "time ", name.new)
name.new <- gsub("^f", "frequency ", name.new)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("Acc", "accelerometer ", name.new)
name.new <- gsub("Gyro", "gyroscope ", name.new)
name.new <- gsub("Mag", "magnitude", name.new)
name.new <- gsub("mean", " mean ", name.new)
name.new <- gsub("std", " standard deviation ", name.new)
name.new <- gsub("Jerk", "jerk ", name.new)
name.new <- gsub("-", " ", name.new)
name.new <- gsub("Body", " body ", name.new)
name.new <- gsub("Gravity", " gravity ", name.new)
names(extracted) <- name.new
#
#5. independent tidy data creation----
average <- aggregate(extracted[,3:68], 
                     by = list(activity = extracted$activity, 
                               subject = extracted$subject),
                     FUN = mean)
write.table(x = average, file = 'average.txt', row.names = FALSE)
