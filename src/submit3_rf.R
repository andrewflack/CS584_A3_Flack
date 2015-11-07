library('ProjectTemplate')
load.project()

# train rf on subTrain, make a prediction on subTest, and inspect confusion matrix
fitControl <- trainControl(method = "repeatedcv", number = 10)

sub3rf <- train(rating ~ ., data = subTrain, method = "rf", trControl = fitControl)
sub3rf_pred <- predict(sub3rf, subTest)
confusionMatrix(sub3rf_pred, subTest$rating)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction 1.0 2.0 4.0 5.0
# 1.0 188 113  52  45
# 2.0  44  84  52  25
# 4.0  22  27  82  45
# 5.0  39  26  99 178
# 
# Overall Statistics
# 
# Accuracy : 0.4746         
# 95% CI : (0.445, 0.5043)
# No Information Rate : 0.2614         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.2964         
# Mcnemar's Test P-Value : 2.436e-13      
# 
# Statistics by Class:
# 
#                      Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.6416    0.33600    0.28772     0.6075
# Specificity              0.7464    0.86108    0.88756     0.8019
# Pos Pred Value           0.4724    0.40976    0.46591     0.5205
# Neg Pred Value           0.8548    0.81878    0.78519     0.8524
# Prevalence               0.2614    0.22302    0.25424     0.2614
# Detection Rate           0.1677    0.07493    0.07315     0.1588
# Detection Prevalence     0.3550    0.18287    0.15700     0.3051
# Balanced Accuracy        0.6940    0.59854    0.58764     0.7047



# once satisfied, move on to full training set and produce a submission csv file

# train random forest - only use columns that are common between train and test 
fit_rf <- train(rating ~ ., data = train_df[,c(intersect(names(train_df), names(test_df)), "rating")], method = "rf", trControl = fitControl)
  
# make predictions using trained random forest
rf_predict <- predict(fit_rf, test_df)

# convert rf_predict to dataframe, add id column, and rename rating column
submit3 <- cbind(id = rownames(as.data.frame(rf_predict)), as.data.frame(rf_predict))
colnames(submit3) <- c("id", "category")

# write to csv
write.csv(submit3, "submissions/submit3_rf.csv", row.names = FALSE)

