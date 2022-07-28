library(tidyverse)

set.seed(678)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
head(titanic)
tail(titanic)
dim(titanic)
class(titanic)

titanic.tbl = as.tibble(titanic)
titanic.tbl
class(titanic.tbl)

tail(titanic.tbl)

# Proses pengacakan
set.seed(123)
shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)

titanic <- titanic[shuffle_index, ]
head(titanic)
tail(titanic)

# Clean Dataset
library(dplyr)
library(Hmisc)

describe(titanic)
summary(titanic)
# Drop variables
clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  # Convert to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Midle',
                                                                 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()

clean_titanic$age[clean_titanic$age == "?"] <- NA
clean_titanic$age <- as.numeric(clean_titanic$age)
clean_titanic$sex <- factor(clean_titanic$sex)
glimpse(clean_titanic)

#titanic$age[titanic$age == "?"] <- NA
#titanic$age


clean_titanic$fare[clean_titanic$fare == "?"] <- NA
clean_titanic$fare <- as.numeric(clean_titanic$fare)
glimpse(clean_titanic)

#titanic$fare[titanic$fare == "?"] <- NA
#titanic$fare

# Create Train Data and Test Data
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

prop.table(table(data_train$survived))
prop.table(table(data_test$survived))


# build the model
library(rpart)
library(rpart.plot)
fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

# make a prediction
predict_unseen <- predict(fit, data_test, type = 'class')
table_mat <- table(data_test$survived, predict_unseen)
table_mat


# measure performance
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


#tune the hyper-parameters
#rpart.control(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30)
accuracy_tune <- function(fit) { 
  predict_unseen <- predict(fit, data_test, type = 'class') 
  table_mat <- table(data_test$survived, predict_unseen) 
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat) 
  accuracy_Test 
}
control <- rpart.control(minsplit = 4, 
  minbucket = round(5 / 3), 
  maxdepth = 3, 
  cp = 0) 
tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control) 
accuracy_tune(tune_fit)
