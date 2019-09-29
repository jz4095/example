# load libraries
library(tidyverse)
library(lubridate)
library(class)

# read the data
spam <- read_tsv('spambase.tsv')

# rename the label, make it a factor, and shuffle the data
spam <- spam %>% mutate(label = as.factor(is_spam)) %>% select(-is_spam) %>% slice(sample(1:n()))

# determine index of the middle record for dividing data into train/test sets
split_size = floor(nrow(spam)/2)
#split_size = floor(2*nrow(spam)/3)

# make training set and training labels
train <- spam %>% slice(1:split_size)
train_labels <- train$label
train <- train %>% select(-label)

# make test set and test labels
test <- spam %>% slice(split_size+1:n())
test_labels <- test$label
test <- test %>% select(-label)

# fit KNN model on training set, then compute training set accuracy for 
# values of K going from 99 to 1 (decreasing by 3 each time)
for (K in c(seq(99, 1, -3), 1) ){
  knn_predict <- knn(train, train, train_labels, k = K)
  cat('K = ', K, ': train error = ', 
      1-sum(knn_predict==train_labels)/length(knn_predict), " \n")
}

# fit KNN model on training set, then compute test set accuracy for 
# values of K going from 99 to 1 (decreasing by 3 each time)
for (K in c(seq(99, 1, -3), 1) ){
  knn_predict <- knn(train, test, train_labels, k = K)
  cat('K = ', K, ': test error = ', 
      1-sum(knn_predict==test_labels)/length(knn_predict), " \n")
}
