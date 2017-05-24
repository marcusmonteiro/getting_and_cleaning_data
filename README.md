# Getting and Cleaning Data Course Project

Project Instructions at: https://www.coursera.org/learn/data-cleaning/peer/FIZtT/getting-and-cleaning-data-course-project

## Instructions

The `run_analysis.R` script to make a tidy 'Tidy_UCI_HAR_Dataset.txt' file, made from data data collected from the accelerometers from the Samsung Galaxy S smartphone, found at: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip .

If you don't have the 'UCI HAR Dataset' in your current directory, this script will download and unzip it. 

It will also install the [pacman](https://cran.r-project.org/package=pacman) package, which in it's turn will install and load the required packages (data.table, dplyr, dtplyr, readr, tidyr) if you don't have them.

It will also replace any 'Tidy_UCI_HAR_Dataset.txt' named file in your current directory.

The 'Tidy_UCI_HAR_Dataset_Code_Book.md' file describes the variables in the produced tidy dataset.

Thanks for your time!