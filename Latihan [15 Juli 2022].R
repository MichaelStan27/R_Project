
###########################
# Step 1: Import the data #
###########################

library(tidyverse)
path = 'titanic_data.csv'
titanic = read.csv(path)
head(titanic)
tail(titanic)

dim(titanic)
class(titanic)

titanic.tbl = as_tibble(titanic)
titanic.tbl
class(titanic.tbl)

tail(titanic.tbl)

# Shuffle the dataset
set.seed(678)
shuffle_index = sample(1:nrow(titanic))
head(shuffle_index)

length(shuffle_index)

shuffled_titanic = titanic[shuffle_index, ]
head(shuffled_titanic)
tail(shuffled_titanic)



#############################
# Step 2: Clean the dataset #
#############################

library(dplyr)
library(Hmisc)

describe(shuffled_titanic)
summary(shuffled_titanic)

clean_titanic = 
  shuffled_titanic %>%
  # Drop variables
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  # Convert to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes')),
         sex = factor(sex),
         embarked = factor(embarked)) %>%
  # Drop all the row which has missing value (NA)
  na.omit()

# Change all "?" to NA in age column
clean_titanic$age[clean_titanic$age == "?"] = NA
# Convert age column to numeric
clean_titanic$age = as.numeric(clean_titanic$age)

# Change all "?" to NA in fare column
clean_titanic$fare[clean_titanic$fare == "?"] = NA
# Convert fare column to numeric
clean_titanic$fare = as.numeric(clean_titanic$fare)

# Glimpse of the cleaned dataset
glimpse(clean_titanic)



#################################
# Step 3: Create train/test set #
#################################

# Function for splitting the dataset into train and test dataset
create_train_test = function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample = 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

# Create training dataset
data_train = create_train_test(clean_titanic, 0.8, train = TRUE)
# Create test dataset
data_test = create_train_test(clean_titanic, 0.8, train = FALSE)

dim(data_train)
dim(data_test)

# Check yes/no proportion for train dataset
prop.table(table(data_train$survived))
# Check yes/no proportion for test dataset
prop.table(table(data_test$survived))



###########################
# Step 4: Build the model #
###########################

library(rpart)
library(rpart.plot)

# Train the model (Decision tree)
fit = rpart(survived ~ ., data = data_train, method = 'class')

# Plot the model
rpart.plot(fit, extra = 106)



#############################
# Step 5: Make a prediction #
#############################

# Predict the test dataset result from model above
predict_unseen = predict(fit, data_test, type = 'class')

# Show the result of the prediction
table_mat = table(data_test$survived, predict_unseen) %>% print()



###############################
# Step 6: Measure performance #
###############################

# Calculate model accuracy
accuracy_Test = sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))



#####################################
# Step 7: Tune the hyper-parameters #
#####################################

# Implement a function for testing a model, and output the accuracy of the model
accuracy_tune = function(fit) { 
  predict_unseen = predict(fit, data_test, type = 'class') 
  table_mat = table(data_test$survived, predict_unseen) 
  accuracy_Test = sum(diag(table_mat)) / sum(table_mat) 
  accuracy_Test
}

# Tune the model
control = rpart.control(minsplit = 4, minbucket = round(5 / 3), maxdepth = 3, cp = 0)

# Re-train the model with the new hyper-parameters
tune_fit = rpart(survived~., data = data_train, method = 'class', control = control) 

# Show the result
accuracy_tune(tune_fit)
