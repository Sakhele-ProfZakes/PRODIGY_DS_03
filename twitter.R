install.packages("tm")
install.packages("syuzhet")
install.packages("wordcloud")
install.packages("tidyverse")
install.packages("purrr")
library(tm)
library(syuzhet)
library(wordcloud)
library(tidyverse)
library(purrr)

twitter_data = read.csv("twitter_training.csv")

text_column = "im.getting.on.borderlands.and.i.will.murder.you.all.."

# Cleaning data
clean_data = function(text) {
  text = gsub("http\\S+|www\\S+", "", text) 
  text = gsub("@\\w+", "", text)
  text = gsub("#\\w+", "", text)
  text = gsub("[^A-Za-z\\s]", "", text)
  text = tolower(text)
  return(text)
}

# Apply clean_data function
twitter_data = twitter_data %>% mutate(clean_text = map_chr(.data[[text_column]], clean_data))

# Sentiment Analysis
get_sentiment_label = function(text) {
  sentiment = get_sentiment(text, method = "syuzhet")
  if (sentiment > 0) {
    return("positive")
  } else if (sentiment == 0) {
    return("neutral")
  } else {
    return("negative")
  }
}

# Apply get_sentiment_label function
twitter_data = twitter_data %>% mutate(sentiment = map_chr(clean_text, get_sentiment_label))

# Display first few rows with sentiment
head(twitter_data %>% select(all_of(text_column), clean_text, sentiment))

# Plotting the sentiment distribution
twitter_data %>% 
  count(sentiment) %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Sentiment Distribution") + 
  theme_minimal()

# Function to create and clean a corpus for word cloud
create_wordcloud <- function(text_data) {
  corpus <- Corpus(VectorSource(text_data))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  dtm <- TermDocumentMatrix(corpus)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix), decreasing = TRUE)
  df <- data.frame(word = names(words), freq = words)
  wordcloud(words = df$word, freq = df$freq, max.words = 100, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))
}

# Word Clouds for Sentiments
positive_tweets <- twitter_data %>% 
  filter(sentiment == "positive") %>% 
  pull(clean_text)

negative_tweets <- twitter_data %>% 
  filter(sentiment == "negative") %>% 
  pull(clean_text)

neutral_tweets <- twitter_data %>% 
  filter(sentiment == "neutral") %>% 
  pull(clean_text)

# Create word clouds
create_wordcloud(positive_tweets)
create_wordcloud(negative_tweets)
create_wordcloud(neutral_tweets)
