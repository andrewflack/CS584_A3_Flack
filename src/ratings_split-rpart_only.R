library('ProjectTemplate')
load.project()

# This script uses a decision tree classifier to first predict whether a review is positive (1) or 
# negative (0). Then, a decision tree classifier is used to classify the positives as either 4 or 5,
# and classify the negatives as 1 or 2.


# temporarily remove the rating column for training
subTrain_temp <- subTrain %>% select(-rating)

# convert the pos_flag to a factor
subTrain_temp$pos_flag <- as.factor(subTrain_temp$pos_flag)

# train decision tree on subTrain_temp, make a prediction on subTest, and inspect confusion matrix
fitControl <- trainControl(method = "repeatedcv", number = 10)

splitrpart <- train(pos_flag ~ ., data = subTrain_temp, method = "rpart", trControl = fitControl)
splitrpart_pred <- predict(splitrpart, subTest)
confusionMatrix(splitrpart_pred, subTest$pos_flag)

# add the predicted category as a column on subTest
subTest$predicted_category <- predict(splitrpart, subTest)

# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   0   1
#           0 189  64
#           1 286 482
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
#         Reference
# Prediction 1.0 2.0 4.0 5.0
#       1.0 198 152 103 102
#       2.0  30  59  46  25
#       4.0   0   0   0   0
#       5.0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.3594          
# 95% CI : (0.3242, 0.3958)
# No Information Rate : 0.3189          
# P-Value [Acc > NIR] : 0.01166         
# 
# Kappa : 0.0668          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.8684    0.27962     0.0000     0.0000
# Specificity              0.2669    0.79960     1.0000     1.0000
# Pos Pred Value           0.3568    0.36875        NaN        NaN
# Neg Pred Value           0.8125    0.72613     0.7916     0.8224
# Prevalence               0.3189    0.29510     0.2084     0.1776
# Detection Rate           0.2769    0.08252     0.0000     0.0000
# Detection Prevalence     0.7762    0.22378     0.0000     0.0000
# Balanced Accuracy        0.5677    0.53961     0.5000     0.5000

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
#         Reference
# Prediction 1.0 2.0 4.0 5.0
#       1.0   0   0   0   0
#       2.0   0   0   0   0
#       4.0  10  32  64  38
#       5.0  29  29  60  99
# 
# Overall Statistics
# 
# Accuracy : 0.4515          
# 95% CI : (0.3994, 0.5045)
# No Information Rate : 0.3795          
# P-Value [Acc > NIR] : 0.003057        
# 
# Kappa : 0.1361          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity               0.000      0.000     0.5161     0.7226
# Specificity               1.000      1.000     0.6624     0.4732
# Pos Pred Value              NaN        NaN     0.4444     0.4562
# Neg Pred Value            0.892      0.831     0.7235     0.7361
# Prevalence                0.108      0.169     0.3435     0.3795
# Detection Rate            0.000      0.000     0.1773     0.2742
# Detection Prevalence      0.000      0.000     0.3989     0.6011
# Balanced Accuracy         0.500      0.500     0.5893     0.5979


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
# 1.0 198 152 103 102
# 2.0  30  59  46  25
# 4.0  10  32  64  38
# 5.0  29  29  60  99
# 
# Overall Statistics
# 
# Accuracy : 0.3903          
# 95% CI : (0.3611, 0.4202)
# No Information Rate : 0.2537          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.1882          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.7416    0.21691    0.23443    0.37500
# Specificity              0.5587    0.87438    0.90037    0.85468
# Pos Pred Value           0.3568    0.36875    0.44444    0.45622
# Neg Pred Value           0.8676    0.76747    0.77575    0.80792
# Prevalence               0.2481    0.25279    0.25372    0.24535
# Detection Rate           0.1840    0.05483    0.05948    0.09201
# Detection Prevalence     0.5158    0.14870    0.13383    0.20167
# Balanced Accuracy        0.6501    0.54564    0.56740    0.61484
