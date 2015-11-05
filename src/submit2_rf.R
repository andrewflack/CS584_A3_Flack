library('ProjectTemplate')
load.project()

# train rf on subTrain, make a prediction on subTest, and inspect confusion matrix
sub2rf <- train(rating ~ ., data = subTrain, method = "rf")
sub2rf_pred <- predict(sub2rf, subTest)
confusionMatrix(sub2rf_pred, subTest$rating)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction 1.0 2.0 4.0 5.0
# 1.0 173  97  33  49
# 2.0  57  86  46  25
# 4.0  12  26  87  48
# 5.0  42  42 104 163
# 
# Overall Statistics
# 
# Accuracy : 0.467          
# 95% CI : (0.437, 0.4971)
# No Information Rate : 0.2615         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.2866         
# Mcnemar's Test P-Value : 2.665e-09      
# 
# Statistics by Class:
# 
# Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.6092     0.3426    0.32222     0.5719
# Specificity              0.7779     0.8474    0.89512     0.7665
# Pos Pred Value           0.4915     0.4019    0.50289     0.4644
# Neg Pred Value           0.8496     0.8116    0.80044     0.8349
# Prevalence               0.2606     0.2303    0.24771     0.2615
# Detection Rate           0.1587     0.0789    0.07982     0.1495
# Detection Prevalence     0.3229     0.1963    0.15872     0.3220
# Balanced Accuracy        0.6935     0.5950    0.60867     0.6692



# once satisfied, move on to full training set and produce a submission csv file

# train random forest - only use columns that are common between train and test 
fit_rf <- randomForest(rating ~ ., data = train_df[,c(intersect(names(train_df), names(test_df)), "rating")], ntree = 500, nodesize = 5, mtry = 5, importance = TRUE)

# make predictions using trained random forest
rf_predict <- predict(fit_rf, test_df, type = "response")

# convert rf_predict to dataframe, add id column, and rename rating column
submit2 <- cbind(id = rownames(as.data.frame(rf_predict)), as.data.frame(rf_predict))
colnames(submit2) <- c("id", "rating")

# write to csv
write.csv(submit2, "submissions/submit2_rf.csv", row.names = FALSE)

