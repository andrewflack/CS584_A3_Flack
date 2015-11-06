library('ProjectTemplate')
load.project()

# train decision tree on subTrain, make a prediction on subTest, and inspect confusion matrix
fitControl <- trainControl(method = "repeatedcv", number = 10)

sub2rpart <- train(rating ~ ., data = subTrain, method = "rpart", trControl = fitControl)
sub2rpart_pred <- predict(sub2rpart, subTest)
confusionMatrix(sub2rpart_pred, subTest$rating)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction 1.0 2.0 4.0 5.0
# 1.0 171 106  90 121
# 2.0  27  39  10   7
# 4.0  57  75 105  64
# 5.0  38  30  80 101
# 
# Overall Statistics
# 
# Accuracy : 0.3711          
# 95% CI : (0.3427, 0.4001)
# No Information Rate : 0.2614          
# P-Value [Acc > NIR] : 4.973e-16       
# 
# Kappa : 0.154           
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
#                      Class: 1.0 Class: 2.0 Class: 4.0 Class: 5.0
# Sensitivity              0.5836    0.15600    0.36842     0.3447
# Specificity              0.6171    0.94948    0.76555     0.8213
# Pos Pred Value           0.3504    0.46988    0.34884     0.4056
# Neg Pred Value           0.8073    0.79672    0.78049     0.7798
# Prevalence               0.2614    0.22302    0.25424     0.2614
# Detection Rate           0.1525    0.03479    0.09367     0.0901
# Detection Prevalence     0.4353    0.07404    0.26851     0.2221
# Balanced Accuracy        0.6004    0.55274    0.56699     0.5830

# once satisfied, move on to full training set and produce a submission csv file

# train random forest - only use columns that are common between train and test 
fit_rpart <- train(rating ~ ., data = train_df[,c(intersect(names(train_df), names(test_df)), "rating")], method = "rpart", trControl = fitControl)

# make predictions using trained decision tree
rpart_predict <- predict(fit_rpart, test_df)

# convert rpart_predict to dataframe, add id column, and rename rating column
submit2 <- cbind(id = rownames(as.data.frame(rpart_predict)), as.data.frame(rpart_predict))
colnames(submit2) <- c("id", "rating")

# write to csv
write.csv(submit2, "submissions/submit2_rpart.csv", row.names = FALSE)
