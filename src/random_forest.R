library('ProjectTemplate')
load.project()

# set.seed(1234)
# ind <- sample(2, nrow(dtms_df), replace=TRUE, prob=c(0.80, 0.20))
# training <- dtms_df[ind==1,]
# test <- dtms_df[ind==2,]
# 
# fit_rf <- randomForest(rating ~ ., data = training[,2:ncol(training)], ntree = 500, nodesize = 5, mtry = 5, importance = TRUE)
# rf_predict <- predict(fit_rf,test[,2:ncol(test)-1], type = "response")
# 
# table(predicted = rf_predict, actual = test$rating)
# 
# # accuracy
# rf_acc <- sum(rf_predict == test$rating)/length(rf_predict)

############

# fit_rf <- randomForest(rating ~ ., data = dtms_df, ntree = 500, nodesize = 5, mtry = 5, importance = TRUE)
# rf_predict <- predict(fit_rf, test_df, type = "response")