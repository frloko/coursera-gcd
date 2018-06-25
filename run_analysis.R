getwd()

# *************************************************************
# Load useful packages
# *************************************************************
library(tidyverse)
library(data.table)

# *************************************************************
# Read in features
# *************************************************************
features = read_table2("UCI HAR Dataset/features.txt", 
                       col_names = c("feature", "feature_name"),
                       n_max = Inf) # %>% setDT

anyDuplicated(features$feature_name)
# table(features$feature_name)

features = features %>% 
        mutate(feature_name_combo = paste(feature, feature_name, sep = "-"))

# *************************************************************
# Read in Activity Labels
# *************************************************************
alab = read_table("UCI HAR Dataset/activity_labels.txt", 
                  col_names = c("activity", "activity_name"), 
                  n_max = Inf) # %>% setDT

# *************************************************************
# Read in Test datasets
# *************************************************************
## Test data
xtest = read_table("UCI HAR Dataset/test/X_test.txt", 
                   # col_names = paste0("feature", seq(1,561,1)),
                   col_names = features$feature_name_combo,
                   n_max = Inf) # %>% setDT

## Test labels
ytest = read_table("UCI HAR Dataset/test/y_test.txt", 
                   col_names = "activity", 
                   n_max = Inf) # %>% setDT

## Test subject info
subtest = read_table("UCI HAR Dataset/test/subject_test.txt", 
                     col_names = "subject", 
                     n_max = Inf) # %>% setDT

# *************************************************************
# Merge Test datasets
# *************************************************************

test = cbind(subtest, ytest, xtest) %>% 
        merge(alab, by = "activity") %>% 
        select(subject, activity, activity_name, everything())


# *************************************************************
# Read in Training datasets
# *************************************************************
## Training data
xtrain = read_table("UCI HAR Dataset/train/X_train.txt", 
                    # col_names = paste0("feature", seq(1,561,1)),
                    col_names = features$feature_name_combo,
                    n_max = Inf) # %>% setDT

## Training labels
ytrain = read_table("UCI HAR Dataset/train/y_train.txt", 
                    col_names = "activity", 
                    n_max = Inf) # %>% setDT

## Training subject info
subtrain = read_table("UCI HAR Dataset/train/subject_train.txt", 
                      col_names = "subject", 
                      n_max = Inf) # %>% setDT

# *************************************************************
# Merge Training datasets
# *************************************************************

train = cbind(subtrain, ytrain, xtrain) %>% 
        merge(alab, by = "activity") %>% 
        select(subject, activity, activity_name, everything())

# *************************************************************
# Merge Test and Training datasets
# *************************************************************
combined_data = bind_rows(test, train)


# *************************************************************
# Pull mean and standard deviation data only
# *************************************************************
features_mean_std_only = features %>% filter(grepl("[Mm]ean|std", feature_name))
anyDuplicated(features_mean_std_only$feature_name)

combined_data_mean_std_only = combined_data %>% select(subject, activity, activity_name, matches("mean|std"))
setdiff(names(combined_data_mean_std_only), features_mean_std_only$feature_name_combo)

setnames(combined_data_mean_std_only, c("subject", "activity", "activity_name", features_mean_std_only$feature_name))

# **************************************************************************************************************************
# Create a second, independent tidy data set with the average of each variable for each activity and each subject
# **************************************************************************************************************************

combined_data_mean_std_only_avg = combined_data_mean_std_only %>% 
        group_by(subject, activity, activity_name) %>% 
        # summarize_if(is.numeric, mean, na.rm = TRUE)
        summarise_at(vars(`tBodyAcc-mean()-X`:`angle(Z,gravityMean)`), mean, na.rm = TRUE) %>% 
        ungroup()

write.table(combined_data_mean_std_only_avg, file = "Average of features by subject and activity.txt", row.names = FALSE)
