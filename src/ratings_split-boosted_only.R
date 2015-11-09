library('ProjectTemplate')
load.project()

# This script uses a boosted decision tree classifier to first predict whether a review is positive (1) or 
# negative (0). Then, a boosted decision tree classifier is used to classify the positives as either 4 or 5,
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
split_low <- train(rating ~ ., data = subTrain_low, method = "ada", trControl = fitControl, tuneGrid = Grid)
split_low_pred <- predict(split_low, subTest_low)
confusionMatrix(split_low_pred, subTest_low$rating)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction 1.0 2.0 4.0 5.0
# 1.0 109  77  43  42
# 2.0  68  98  55  46
# 4.0   0   0   0   0
# 5.0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.3848          
# 95% CI : (0.3434, 0.4273)
# No Information Rate : 0.329           
# P-Value [Acc > NIR] : 0.003709        
# 
# Kappa : 0.0856          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.6158     0.5600     0.0000     0.0000
# Specificity              0.5512     0.5344     1.0000     1.0000
# Pos Pred Value           0.4022     0.3670        NaN        NaN
# Neg Pred Value           0.7453     0.7159     0.8178     0.8364
# Prevalence               0.3290     0.3253     0.1822     0.1636
# Detection Rate           0.2026     0.1822     0.0000     0.0000
# Detection Prevalence     0.5037     0.4963     0.0000     0.0000
# Balanced Accuracy        0.5835     0.5472     0.5000     0.5000

# filter down to just 4's and 5's in the subTrain df, and just the predicted 1's from the subTest df
subTrain_high <- subTrain %>% filter(rating == "4.0" | rating == "5.0") %>% select(-pos_flag)
subTest_high <- subTest %>% filter(predicted_category == 1) %>% select(-pos_flag)

# reset factor levels because only 4's and 5's are present
subTrain_high$rating <- factor(subTrain_high$rating)
subTest_high$rating <- factor(subTest_high$rating)

# train on 4's and 5's only, make a prediction against predicted 1's from above
split_high <- train(rating ~ ., data = subTrain_high, method = "ada", trControl = fitControl, tuneGrid = Grid)
split_high_pred <- predict(split_high, subTest_high)
confusionMatrix(split_high_pred, subTest_high$rating)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction 1.0 2.0 4.0 5.0
# 1.0   0   0   0   0
# 2.0   0   0   0   0
# 4.0  46  57  95  95
# 5.0  44  40  80  81
# 
# Overall Statistics
# 
# Accuracy : 0.3271          
# 95% CI : (0.2876, 0.3686)
# No Information Rate : 0.3271          
# P-Value [Acc > NIR] : 0.5162          
# 
# Kappa : 0.0015          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.0000     0.0000     0.5429     0.4602
# Specificity              1.0000     1.0000     0.4545     0.5470
# Pos Pred Value              NaN        NaN     0.3242     0.3306
# Neg Pred Value           0.8327     0.8197     0.6735     0.6758
# Prevalence               0.1673     0.1803     0.3253     0.3271
# Detection Rate           0.0000     0.0000     0.1766     0.1506
# Detection Prevalence     0.0000     0.0000     0.5446     0.4554
# Balanced Accuracy        0.5000     0.5000     0.4987     0.5036

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
# 1.0 109  77  43  42
# 2.0  68  98  55  46
# 4.0  46  57  95  95
# 5.0  44  40  80  81
# 
# Overall Statistics
# 
# Accuracy : 0.3559          
# 95% CI : (0.3273, 0.3854)
# No Information Rate : 0.2537          
# P-Value [Acc > NIR] : 6.892e-14       
# 
# Kappa : 0.1411          
# Mcnemar's Test P-Value : 0.8744          
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.4082    0.36029    0.34799    0.30682
# Specificity              0.7998    0.78980    0.75342    0.79803
# Pos Pred Value           0.4022    0.36704    0.32423    0.33061
# Neg Pred Value           0.8037    0.78492    0.77267    0.77978
# Prevalence               0.2481    0.25279    0.25372    0.24535
# Detection Rate           0.1013    0.09108    0.08829    0.07528
# Detection Prevalence     0.2519    0.24814    0.27230    0.22770
# Balanced Accuracy        0.6040    0.57505    0.55071    0.55242