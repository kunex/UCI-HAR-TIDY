
run_analysis <- function(folder_containing_UCI_HAR_dataset) {

####  pre set environment and check if files already exist


        
setwd(folder_containing_UCI_HAR_dataset)       

if (!file.exists("UCI HAR Dataset")){
                stop("Source data folder: 'UCI HAR Dataset' was not found in the directory specified by the function parameter") }  
        
if (file.exists("UCI HAR Output Tidy Data/tidy_dataset1.csv")){
                stop("'tidy_dataset1.csv' result file already exists") }
        
if (file.exists("UCI HAR Output Tidy Data/tidy_dataset2.csv")){
                stop("'tidy_dataset2.csv' result file already exists") }        
        
library(dplyr)

####  load and prepare data sources

X_test <- read.table("UCI HAR Dataset/test/X_test.txt",stringsAsFactors = FALSE)

Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt",stringsAsFactors = FALSE)
names(Y_test) <- "class_label"

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",stringsAsFactors = FALSE)
names(subject_test) <- "subject"

X_train <- read.table("UCI HAR Dataset/train/X_train.txt",stringsAsFactors = FALSE)

Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt",stringsAsFactors = FALSE)
names(Y_train) <- "class_label"

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",stringsAsFactors = FALSE)
names(subject_train) <- "subject"

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors = FALSE)
names(activity_labels) <- c("activity_code","activity_name")

features <- read.table("UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
names(features) <- c("feature_code","feature_name")

#### 1 union test and train datasets into one

X <- union_all(X_test,X_train)
Y <- union_all(Y_test,Y_train)
subject <- union_all(subject_test,subject_train)


#### 2,3 merge datasets and keep only required columns from the dataset, assign names to columns

features_filtered <- features
features_filtered = features_filtered[grepl("mean|std",features_filtered$feature_name, ignore.case = TRUE) == TRUE,]

names(X) <- gsub("V","",names(X))
nx <- data.frame(names(X))
names(nx) <- "code"
nx$code = as.numeric(levels(nx$code))[nx$code]
xnames <- inner_join(nx,features_filtered,by = c("code"="feature_code"))

Xfiltered <- X[,xnames$code]
names(Xfiltered) = features_filtered$feature_name

f1 <- cbind(Y$class_label,subject$subject,Xfiltered)
names(f1) = c("activity_code","subject", features_filtered$feature_name)

f2 <- f1

# join activity descriptive name and reorder data frame

f2 <- inner_join(f1,activity_labels,by = c("activity_code" = "activity_code"))
f3 <- f2[,c(89,1:88)]
result1 <- f3


## 5 create independent dataset

result2 <- select(result1, activity_name, subject, contains("mean"),contains("std")) %>% 
        group_by(activity_name,subject) %>% 
        summarize_each(funs(mean))

## Check if output directory exists and write results to output files

OutputFolder <- "UCI HAR Output Tidy Data"

if (!file.exists(OutputFolder)){
        dir.create(OutputFolder) }


        write.table(result1,"UCI HAR Output Tidy Data/tidy_dataset1.txt",row.name=FALSE)
        write.table(result2,"UCI HAR Output Tidy Data/tidy_dataset2.txt",row.name=FALSE)

        print("Execution complete, output files created in 'UCI HAR Output Tidy Data' folder.")

}

