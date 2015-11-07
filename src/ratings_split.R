library('ProjectTemplate')
load.project()

# temporarily remove the rating column for training
subTrain_temp <- subTrain %>% select(-rating)

# convert the pos_flag to a factor
subTrain_temp$pos_flag <- as.factor(subTrain_temp$pos_flag)

# train decision tree on subTrain_temp, make a prediction on subTest, and inspect confusion matrix
fitControl <- trainControl(method = "repeatedcv", number = 10)

splitrpart <- train(pos_flag ~ ., data = subTrain_temp, method = "rpart", trControl = fitControl)
splitrpart_pred <- predict(splitrpart, subTest)
confusionMatrix(splitrpart_pred, subTest$pos_flag)

subTest$predicted_category <- predict(splitrpart, subTest)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0 189  64
# 1 286 482
# 
# Accuracy : 0.6572          
# 95% CI : (0.6272, 0.6863)
# No Information Rate : 0.5348          
# P-Value [Acc > NIR] : 1.435e-15       
# 
# Kappa : 0.2895          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.3979          
# Specificity : 0.8828          
# Pos Pred Value : 0.7470          
# Neg Pred Value : 0.6276          
# Prevalence : 0.4652          
# Detection Rate : 0.1851          
# Detection Prevalence : 0.2478          
# Balanced Accuracy : 0.6403          
# 
# 'Positive' Class : 0  





subTrain_low <- subTrain %>% filter(as.numeric(rating) == 1 | as.numeric(rating) == 2) %>% select(-pos_flag)
subTest_low <- subTest %>% filter(predicted_category == 0) %>% select(-pos_flag)

subTrain_low$rating <- factor(subTrain_low$rating, levels = c("1.0","2.0"))
subTest_low$rating <- factor(subTest_low$rating, levels = c("1.0","2.0"))

split_low <- train(rating ~ ., data = subTrain_low, method = "rpart", trControl = fitControl)
split_low_pred <- predict(split_low, subTest_low)
confusionMatrix(split_low_pred, subTest_low$rating)

subTest_low$predicted_rating <- predict(split_low, subTest_low)



subTrain_high <- subTrain %>% filter(rating == "4.0" | rating == "5.0") %>% select(-pos_flag)
subTest_high <- subTest %>% filter(predicted_category == 1) %>% select(-pos_flag)

subTrain_high$rating <- factor(subTrain_high$rating, levels = c("4.0","5.0"))
subTest_high$rating <- factor(subTest_high$rating, levels = c("4.0","5.0"))

split_high <- train(rating ~ ., data = subTrain_high, method = "rpart", trControl = fitControl)
split_high_pred <- predict(split_high, subTest_high)
confusionMatrix(split_high_pred, subTest_high$rating)

subTest_high$predicted_rating <- predict(split_high, subTest_high)


subTest_final <- rbind(subTest_low, subTest_high)
