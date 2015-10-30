test_df <- as.data.frame(test)
colnames(test_df) <- c("text")

test_df$text <- gsub(":\\d", "", test_df$text)
test_df$text <- gsub("_", " ", test_df$text)

# add an ID to keep track of documents
test_df$id <- seq.int(nrow(test_df))

test_docs <- VCorpus(DataframeSource(test_df), readerControl=list(reader=myReader))
test_docs <- tm_map(test_docs, removePunctuation)
test_docs <- tm_map(test_docs, removeNumbers)
test_docs <- tm_map(test_docs, content_transformer(tolower))
test_docs <- tm_map(test_docs, removeWords, stopwords("english"))
test_docs <- tm_map(test_docs, stripWhitespace)
test_docs <- tm_map(test_docs, stemDocument)

test_dtm <- DocumentTermMatrix(test_docs)
test_tdm <- TermDocumentMatrix(test_docs)

test_dtms <- removeSparseTerms(test_dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
test_dtms <- removeSparseTerms(test_dtm, 0.95) # Try 95%, because 10% removed all terms. Down to 224 terms.

# print dtms to csv to inspect
test_m <- as.matrix(test_dtms)   
# write.csv(test_m, file="test_dtms.csv")

# convert dtms to df and add class labels back in
test_df <- as.data.frame(test_m)
