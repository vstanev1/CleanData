  run_analysis <- function(){
    
    library(plyr)
    library(dplyr)
    
    # The first 6 lines load the data into 6 separate tables (3 each for the test abd train data)  
    dataXtrain  <- read.table("X_train.txt")
    data_Subject_train  <- read.table("subject_train.txt")
    dataYtrain  <- read.table("Y_train.txt")
    dataXtest  <- read.table("X_test.txt")
    dataYtest  <- read.table("Y_test.txt")
    data_Subject_test  <- read.table("subject_test.txt")
    
    # Lines 13 to 16 rename the subject and the activity variable in the data frames
    # The next steps (17 to 21) combine the 6 tables into one big data frame  (data_tot with 10299 obs and 563 variables)
    data_Subject_test  <- rename(data_Subject_test, subject = V1)
    data_Subject_train  <- rename(data_Subject_train, subject = V1)
    dataYtrain  <- rename(dataYtrain, activity = V1)
    dataYtest  <- rename(dataYtest, activity = V1)
    data_test  <- cbind(data_Subject_test, dataYtest)
    data_test  <- cbind(data_test, dataXtest)
    data_train  <- cbind(data_Subject_train, dataYtrain)
    data_train  <- cbind(data_train, dataXtrain)
    data_tot  <- rbind(data_train, data_test)
    
    # Next load the measurements and activity names, and change the column names 3 to 563.
    # Changing the activity codes from number to escriptive name requires transforming it to factor.
    meas_names  <- read.table("features.txt")
    act_names  <- read.table("activity_labels.txt")
    colnames(data_tot)[3:563]  <- as.character(meas_names[,2])
    data_tot$activity <- factor(data_tot$activity,levels = c(1,2,3, 4, 5, 6),labels = as.character(act_names[,2]))
    
    # Line 38 extract from the big data frame all the onservation with "mean()" and "std()" in their names.
    # the result is a data frame data_ext with 81 variables. 
    # The next line uses the dplyr %>% operator to group the data frame by activity, and then calculate within 
    # each group the mean and the standard deviation for each variable. The result is data_summ (6 obs of 161 variables).
     
    data_ext  <- cbind(data_tot[,1:2], data_tot[,grep("mean()", colnames(data_tot))], data_tot[,grep("std()", colnames(data_tot))])
    data_summ  <<- data_ext %>% group_by(activity) %>% summarise_each(funs(mean, sd))
    
  }