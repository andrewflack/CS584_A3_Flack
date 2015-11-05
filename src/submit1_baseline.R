library('ProjectTemplate')
load.project()

summary(train_df$rating)/nrow(train_df)

# 1.0       2.0       4.0       5.0 
# 0.2560154 0.2413859 0.2515881 0.2510106 

# see that a naive classifier that guessed all 1's would have accuracy around .25