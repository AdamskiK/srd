source("featureEngineering.R")
source("models.R")
source("gini Function.R")

library("caret")
library("dplyr")
library("rpart")
library("rpart.plot")
library("Hmisc")

data <- read.csv('data/final_data.csv')

# create some additional features
data <- CreatureFeatures(data)

# analyse a whole dataset - numeric vriables - check correlations
numeric_data <- select_if(data, is.numeric)  # select only numeric columns
numeric_data <- numeric_data[, !(names(numeric_data)) %in% c("DefFlag")]  # don't select a target variable
correlations <- cor(numeric_data)  # check correlations

highy_corr_cols <- findCorrelation(correlations, cutoff = 0.99)  # remove highly correlated features
highy_corr_cols <- sort(highy_corr_cols)
reduced_data <- data[, -c(highy_corr_cols)]  # remove highly correlated columns from the original dataset

# check factor variables
factor_data <- select_if(reduced_data[, !(names(reduced_data)) %in% c("Application_ID")], is.factor)
factor_columns <- names(factor_data)

# convert factors to numeric
for(col in factor_columns){
  reduced_data[, col] <- as.numeric(reduced_data[, col])
}


# divide the dataset (train/test)
reduced_data_cleaned <- reduced_data[, !(names(reduced_data)) %in% c("Application_ID")]  # remove unnecesary columns
train_data <- reduced_data_cleaned[which(!is.na(reduced_data_cleaned$DefFlag)), ]
test_data <- reduced_data_cleaned[which(is.na(reduced_data_cleaned$DefFlag)), ]


## MODELS

# run model 1
RpartModel(reduced_data, train_data, cp=0.0003)

# run model 2
# CaretModel(train_data)  # need more enhancements

# run model 3
# maybe ANN ?

# run model 4,5,...




