library('ProjectTemplate')
load.project()

# This script uses a boosted decision tree classifier to first predict whether a review is positive (1) or 
# negative (0). Then, a decision tree classifier is used to classify the positives as either 4 or 5,
# and classify the negatives as 1 or 2.

# temporarily remove the rating column for training
subTrain_temp <- subTrain %>% select(-rating)

# convert the pos_flag to a factor
subTrain_temp$pos_flag <- as.factor(subTrain_temp$pos_flag)

# train boosted decision tree on subTrain_temp, make a prediction on subTest, and inspect confusion matrix
fitControl <- trainControl(method = "repeatedcv", number = 10)
Grid <- expand.grid(maxdepth=25,nu=2,iter=100)

splitboost <- train(pos_flag ~ ., data = subTrain_temp, method = "ada", trControl = fitControl, tuneGrid = Grid)
splitboost_pred <- predict(splitboost, subTest)
confusionMatrix(splitboost_pred, subTest$pos_flag)

# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   0   1
#           0 352 186
#           1 187 351
# 
# Accuracy : 0.6533          
# 95% CI : (0.6241, 0.6818)
# No Information Rate : 0.5009          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.3067          
# Mcnemar's Test P-Value : 1               
# 
# Sensitivity : 0.6531          
# Specificity : 0.6536          
# Pos Pred Value : 0.6543          
# Neg Pred Value : 0.6524          
# Prevalence : 0.5009          
# Detection Rate : 0.3271          
# Detection Prevalence : 0.5000          
# Balanced Accuracy : 0.6533          
# 
# 'Positive' Class : 0  

# add the predicted category as a column on subTest
subTest$predicted_category <- predict(splitboost, subTest)

# filter down to just 1's and 2's in the subTrain df, and just the predicted 0's from the subTest df
subTrain_low <- subTrain %>% filter(as.numeric(rating) == 1 | as.numeric(rating) == 2) %>% select(-pos_flag)
subTest_low <- subTest %>% filter(predicted_category == 0) %>% select(-pos_flag)

# reset factor levels because only 1's and 2's are present
subTrain_low$rating <- factor(subTrain_low$rating)
subTest_low$rating <- factor(subTest_low$rating)

# train on 1's and 2's only, make a prediction against predicted 0's from above
split_low <- train(rating ~ ., data = subTrain_low, method = "rpart", trControl = fitControl)
split_low_pred <- predict(split_low, subTest_low)
confusionMatrix(split_low_pred, subTest_low$rating)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction 1.0 2.0 4.0 5.0
# 1.0 155 134  61  73
# 2.0  22  41  37  15
# 4.0   0   0   0   0
# 5.0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.3643          
# 95% CI : (0.3236, 0.4066)
# No Information Rate : 0.329           
# P-Value [Acc > NIR] : 0.04566         
# 
# Kappa : 0.0538          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.8757    0.23429     0.0000     0.0000
# Specificity              0.2576    0.79614     1.0000     1.0000
# Pos Pred Value           0.3664    0.35652        NaN        NaN
# Neg Pred Value           0.8087    0.68322     0.8178     0.8364
# Prevalence               0.3290    0.32528     0.1822     0.1636
# Detection Rate           0.2881    0.07621     0.0000     0.0000
# Detection Prevalence     0.7862    0.21375     0.0000     0.0000
# Balanced Accuracy        0.5667    0.51521     0.5000     0.5000

# filter down to just 4's and 5's in the subTrain df, and just the predicted 1's from the subTest df
subTrain_high <- subTrain %>% filter(rating == "4.0" | rating == "5.0") %>% select(-pos_flag)
subTest_high <- subTest %>% filter(predicted_category == 1) %>% select(-pos_flag)

# reset factor levels because only 4's and 5's are present
subTrain_high$rating <- factor(subTrain_high$rating)
subTest_high$rating <- factor(subTest_high$rating)

# train on 4's and 5's only, make a prediction against predicted 1's from above
split_high <- train(rating ~ ., data = subTrain_high, method = "rpart", trControl = fitControl)
split_high_pred <- predict(split_high, subTest_high)
confusionMatrix(split_high_pred, subTest_high$rating)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction 1.0 2.0 4.0 5.0
# 1.0   0   0   0   0
# 2.0   0   0   0   0
# 4.0  20  51  85  41
# 5.0  70  46  90 135
# 
# Overall Statistics
# 
# Accuracy : 0.4089         
# 95% CI : (0.367, 0.4518)
# No Information Rate : 0.3271         
# P-Value [Acc > NIR] : 4.241e-05      
# 
# Kappa : 0.1224         
# Mcnemar's Test P-Value : NA             
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.0000     0.0000     0.4857     0.7670
# Specificity              1.0000     1.0000     0.6915     0.4309
# Pos Pred Value              NaN        NaN     0.4315     0.3959
# Neg Pred Value           0.8327     0.8197     0.7361     0.7919
# Prevalence               0.1673     0.1803     0.3253     0.3271
# Detection Rate           0.0000     0.0000     0.1580     0.2509
# Detection Prevalence     0.0000     0.0000     0.3662     0.6338
# Balanced Accuracy        0.5000     0.5000     0.5886     0.5990

# add the predicted ratings back on as new column on subTest_low and subTest_high
subTest_low$predicted_rating <- predict(split_low, subTest_low)
subTest_high$predicted_rating <- predict(split_high, subTest_high)

# combine subTest_low and subTest_high, the inspect confusion matrix (rating vs predicted_rating)
subTest_final <- rbind(subTest_low, subTest_high)
confusionMatrix(subTest_final$predicted_rating, subTest_final$rating)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction 1.0 2.0 4.0 5.0
# 1.0 155 134  61  73
# 2.0  22  41  37  15
# 4.0  20  51  85  41
# 5.0  70  46  90 135
# 
# Overall Statistics
# 
# Accuracy : 0.3866          
# 95% CI : (0.3574, 0.4165)
# No Information Rate : 0.2537          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.1835          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.5805     0.1507     0.3114     0.5114
# Specificity              0.6687     0.9080     0.8605     0.7463
# Pos Pred Value           0.3664     0.3565     0.4315     0.3959
# Neg Pred Value           0.8285     0.7596     0.7861     0.8245
# Prevalence               0.2481     0.2528     0.2537     0.2454
# Detection Rate           0.1441     0.0381     0.0790     0.1255
# Detection Prevalence     0.3931     0.1069     0.1831     0.3169
# Balanced Accuracy        0.6246     0.5293     0.5859     0.6288