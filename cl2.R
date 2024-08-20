library(tidyverse)
library(caret)
library(tm)
library(wordcloud)
library(e1071)

data <- read.csv('/Users/shalabhsinghyadav/Desktop/final_project/Sentiment.csv')

# Keeping only the necessary columns
data <- data[, c('text', 'sentiment')]

# Splitting the dataset into train and test set
set.seed(123)
split <- createDataPartition(data$sentiment, p = 0.9, list = FALSE)
train <- data[split, ]
test <- data[-split, ]

# Removing neutral sentiments
train <- train[train$sentiment != "Neutral", ]
train_pos <- train[train$sentiment == 'Positive', 'text']
train_neg <- train[train$sentiment == 'Negative', 'text']

wordcloud_draw <- function(data, color = 'black') {
  words <- paste(data, collapse = ' ')
  cleaned_word <- gsub("http\\S+|@\\S+|#\\S+|RT", "", words)
  cleaned_word <- gsub("\\s+", " ", cleaned_word)
  wordcloud(
    words = cleaned_word,
    stopwords = stopwords("en"),
    background = color,
    width = 2500,
    height = 2000
  )
}

print("Positive words")
wordcloud_draw(train_pos, 'white')

print("Negative words")
wordcloud_draw(train_neg)

tweets <- list()
stopwords_set <- stopwords("en")

for (i in 1:nrow(train)) {
  words_filtered <- tolower(strsplit(train$text[i], "\\s+")[[1]])
  words_filtered <- words_filtered[nchar(words_filtered) >= 3]
  words_cleaned <- words_filtered[!grepl("http|@|#|RT", words_filtered)]
  words_without_stopwords <- words_cleaned[!words_cleaned %in% stopwords_set]
  tweets[[i]] <- list(words_without_stopwords, train$sentiment[i])
}

test_pos <- test[test$sentiment == 'Positive', 'text']
test_neg <- test[test$sentiment == 'Negative', 'text']

# Extracting word features
get_words_in_tweets <- function(tweets) {
  all <- unlist(lapply(tweets, function(x) x[[1]]))
  return(all)
}

get_word_features <- function(wordlist) {
  wordlist <- table(wordlist)
  features <- names(wordlist)
  return(features)
}

w_features <- get_word_features(get_words_in_tweets(tweets))

extract_features <- function(document) {
  document_words <- unique(strsplit(document, "\\s+")[[1]])
  features <- sapply(w_features, function(word) word %in% document_words)
  names(features) <- paste0("contains(", w_features, ")")
  return(features)
}

wordcloud_draw(w_features)
print(colnames(training_set))

# Training the Naive Bayes classifier
training_set <- lapply(tweets, function(x) c(extract_features(paste(x[[1]], collapse = " ")), sentiment = x[[2]]))
training_set <- data.frame(do.call(rbind, training_set), stringsAsFactors = FALSE)
colnames(training_set) <- make.names(colnames(training_set), unique = TRUE)

classifier <- naiveBayes(sentiment ~ ., data = training_set)



neg_cnt <- 0
pos_cnt <- 0

for (obj in test_neg) {
  res <- predict(classifier, newdata = extract_features(obj))
  neg_cnt <- neg_cnt + sum(res == 'Negative')
}

for (obj in test_pos) {
  res <- predict(classifier, newdata = extract_features(obj))
  if (res == 'Positive') {
    pos_cnt <- pos_cnt + 1
  }
}

print(paste0('[Negative]: ', length(test_neg), '/', neg_cnt))
print(paste0('[Positive]: ', length(test_pos), '/', pos_cnt))