LoadRequiredPackages <- function() {
  # Load the required packages.
  if (!require('pacman')) {
    install.packages('pacman')
  }
  pacman::p_load(data.table, dplyr, dtplyr, readr, tidyr)
}

dataset.dir <- 'UCI HAR Dataset'
  
DownloadDataset <- function() {
  # Download dataset if the dataset directory does not exists.
  if (!dir.exists(dataset.dir)) {
    dataset.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    temp <- tempfile()
    download.file(dataset.url, temp)
    unzip(temp)
  }
}

MakeTidyDataset <- function() {
  ### Tyding up test and train sets
  
  path.test <- file.path(dataset.dir, 'test')
  path.train <- file.path(dataset.dir, 'train')
  
  data.test.set <- read_table(file.path(path.test, 'X_test.txt'), 
                              col_names = FALSE)
  data.train.set <- read_table(file.path(path.train, 'X_train.txt'), 
                               col_names = FALSE)
  data.merged.set <- bind_rows(data.test.set, data.train.set)
  
  # read_table results in a single column with '1 tBodyAcc-mean()-X' like values :( ...
  data.features <- read.table(file.path(dataset.dir, 'features.txt'), 
                              stringsAsFactors = FALSE)
  data.features <- rename(data.features, FeatureCode = V1 , FeatureName = V2)
  data.features <- filter(data.features, grepl("mean\\(\\)|std\\(\\)", FeatureName))
  # Matching the X in data.merged.set
  data.features <- mutate(data.features, FeatureCode = paste0("X", FeatureCode))
  
  data.merged.set <- select(data.merged.set, one_of(data.features$FeatureCode))
  setnames(data.merged.set, names(data.merged.set), data.features$FeatureName)
  
  ### By now data.merget.set contains only mean and std data, and more descriptive
  ### labels.
  ### Start working on subject and activity label data
  
  data.test.subject <- read_table(file.path(path.test, 'subject_test.txt'), 
                                  col_names = FALSE)
  data.train.subject <- read_table(file.path(path.train, 'subject_train.txt'), 
                                   col_names = FALSE)
  data.merged.subject <- bind_rows(data.test.subject, data.train.subject)
  data.merged.subject <- rename(data.merged.subject, Subject = X1)
  
  data.test.activity <- read_table(file.path(path.test, 'y_test.txt'), 
                                  col_names = FALSE)
  data.train.activity <- read_table(file.path(path.train, 'y_train.txt'), 
                                   col_names = FALSE)
  data.merged.activity <- bind_rows(data.test.activity, 
                                          data.train.activity)
  data.merged.activity <- rename(data.merged.activity, 
                                       ActivityLabel = X1)
  
  data.activity.label <- read_table(file.path(dataset.dir, 'activity_labels.txt'), 
                                    col_names = FALSE)
  data.activity.label <- rename(data.activity.label, ActivityLabel = X1, 
                                Activity = X2)
  
  data.merged.activity <- full_join(data.merged.activity, data.activity.label)
  data.merged.activity <- select(data.merged.activity, -ActivityLabel)
  
  data.merged.subject <- bind_cols(data.merged.subject, data.merged.activity)
  data.merged.set <- bind_cols(data.merged.subject, data.merged.set)
  
  data.merged.set$Activity <- as.factor(data.merged.set$Activity)
  
  data.merged.set <- gather(data.merged.set, Feature, Measurement, 
                            `tBodyAcc-mean()-X`:`fBodyBodyGyroJerkMag-std()`)
  
  ### From: https://github.com/benjamin-chan/GettingAndCleaningData/blob/master/Project/run_analysis.Rmd
  ###
  grepthis <- function(regex) {
    grepl(regex, data.merged.set$Feature)
  }
  n <- 2
  y <- matrix(seq(1, n), nrow = n)
  x <- matrix(c(grepthis('^t'), grepthis('^f')), ncol = nrow(y))
  data.merged.set <- mutate(data.merged.set, Domain = 
                              factor(x %*% y, labels = c('Time', 'Frequency')))
  
  x <- matrix(c(grepthis('Acc'), grepthis('Gyro')), ncol = nrow(y))
  data.merged.set <- mutate(data.merged.set, Instrument = 
                              factor(x %*% y, labels = c('Accelerometer', 'Gyroscope')))
  
  x <- matrix(c(grepthis('BodyAcc'), grepthis('GravityAcc')), ncol = nrow(y))
  data.merged.set <- mutate(data.merged.set, Acceleration = 
                              factor(x %*% y, labels = 
                                       c(NA, 'Body', 'Gravity')))
  
  x <- matrix(c(grepthis('mean()'), grepthis('std()')), ncol = nrow(y))
  data.merged.set <- mutate(data.merged.set, Variable = 
                              factor(x %*% y, labels = c('Mean', 'StandardDeviation')))
  
  data.merged.set <- mutate(data.merged.set, Jerk = 
                              factor(grepthis('Jerk'), labels = c(NA, 'Jerk')))
  
  data.merged.set <- mutate(data.merged.set, Magnitude = 
                              factor(grepthis('Mag'), labels = c(NA, 'Magnitude')))
  
  n <- 3
  y <- matrix(seq(1, n), nrow=n)
  x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
  data.merged.set <- mutate(data.merged.set, Axis = 
                              factor(x %*% y, labels = c(NA, 'X', 'Y', 'Z')))
  
  data.merged.set <- select(
    data.merged.set, -Measurement, everything(), -Feature)
  data.merged.set <- select(
    data.merged.set, Subject, Activity, Variable, Axis, everything())

  ### 
  
  data.merged.set <- 
    data.merged.set %>%
    group_by(Subject, Activity, Variable, Axis, Domain, Instrument,
             Acceleration, Jerk, Magnitude) %>%
    summarise(MeasurementCount = n(), AverageMeasurement = mean(Measurement))
  
  data.merged.set
}

run <- function() {
  print(LoadRequiredPackages())
  DownloadDataset()
  tidyDataSet <- MakeTidyDataset()
  file.name <- 'Tidy_UCI_HAR_Dataset.txt'
  if (file.exists(file.name)) {
    file.remove(file.name)
  }
  write.table(tidyDataSet, file = file.name, row.names = FALSE)
}

run()