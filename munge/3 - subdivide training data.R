# further divide training data into a training (60%), test (20%), and cross-validation set (20%)
# - this will be used to test approaches before submitting to kaggle

train_df$set <- sample(c(1,2,3), nrow(train_df), replace = TRUE, prob = c(.6,.2,.2))

subTrain <- train_df[train_df$set == 1, names(train_df)]
subTest <- train_df[train_df$set == 2, names(train_df)]
subCV <- train_df[train_df$set == 3, names(train_df)]