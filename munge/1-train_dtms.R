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

docs <- VCorpus(DataframeSource(df), readerControl=list(reader=myReader))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
dtms <- removeSparseTerms(dtm, 0.95) # Try 95%, because 10% removed all terms. Down to 46 terms.

# print dtms to csv to inspect
m <- as.matrix(dtms)   
# write.csv(m, file="dtms.csv")

# convert dtms to df and add class labels back in
dtms_df <- as.data.frame(m)
dtms_df$id <- seq.int(nrow(dtms_df))
dtms_df <- left_join(dtms_df, df[,c("id","rating")], by = "id")

# check dataframe
summary(dtms_df)


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


