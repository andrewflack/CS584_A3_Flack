# from: https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
# and:  http://www.exegetic.biz/blog/2013/09/text-mining-the-complete-works-of-william-shakespeare/

n <- strsplit(train, " #label#:")
df <- as.data.frame(do.call(rbind, n))

colnames(df) <- c("text", "rating")

df$text <- gsub(":\\d", "", df$text)
df$text <- gsub("_", " ", df$text)

# add an ID to keep track of documents
df$id <- seq.int(nrow(df))

# create corpus but maintain IDs (from: http://stackoverflow.com/questions/24501514/keep-document-id-with-r-corpus)
myReader <- readTabular(mapping=list(content="text", id="id"))

train_docs <- VCorpus(DataframeSource(df), readerControl=list(reader=myReader))
train_docs <- tm_map(train_docs, removePunctuation)
train_docs <- tm_map(train_docs, removeNumbers)
train_docs <- tm_map(train_docs, content_transformer(tolower))
train_docs <- tm_map(train_docs, removeWords, stopwords("english"))
train_docs <- tm_map(train_docs, stripWhitespace)
train_docs <- tm_map(train_docs, stemDocument)

train_dtm <- DocumentTermMatrix(train_docs)
train_tdm <- TermDocumentMatrix(train_docs)

train_dtms <- removeSparseTerms(train_dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
train_dtms <- removeSparseTerms(train_dtm, 0.95) # Try 95%, because 10% removed all terms. Down to 46 terms.

# print dtms to csv to inspect
train_m <- as.matrix(train_dtms)   
# write.csv(train_m, file="train_dtms.csv")

# convert train_dtms to df and add class labels back in
train_df <- as.data.frame(train_m)
train_df$id <- seq.int(nrow(train_df))
train_df <- left_join(train_df, df[,c("id","rating")], by = "id")

# add positive/negative flag 
train_df <- train_df %>% 
  mutate(pos_flag = ifelse(as.numeric(rating) > 2, 1, 0))

# check dataframe
summary(train_df)


# #########
# 
# tmp <- readLines("data/train.dat", n = 10)
# tmp <- strsplit(tmp, " ")
# tmp <- strsplit(unlist(tmp), ":")
# df <- as.data.frame(do.call(rbind, tmp))
# colnames(df) <- c("term", "n")
# 
# df %>% 
#   group_by(term) %>% 
#   summarize(freq = sum(n)) %>%
#   arrange(desc(freq)) %>%
#   head(20)
# 
# #########


