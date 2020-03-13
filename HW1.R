library('tidyverse')
library(ggplot2)
library('wordcloud2')
library('tm')
library('tidytext')

TwitterData <- read.csv("twitter-airline-sentiment/Tweets.csv")

view(TwitterData)

# adjust date format
TwitterData <- TwitterData %>% mutate(tweet_created = as.Date(tweet_created))

# Breakdown of num tweets by sentiment & airline
summary(TwitterData$airline_sentiment)
summary(TwitterData$airline)

# airline pie chart
ggplot(TwitterData, aes(x = airline_sentiment['negative'], fill = airline)) +
  geom_bar() +
  coord_polar("y", start=0) +
  labs(title = "Proportion of Tweets by Airline")

# stacked sentiment by airline
ggplot(TwitterData, aes(x = airline, fill = airline_sentiment)) +
  geom_bar() + 
  labs(title = "Stacked Sentiment by Airline") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5))

# seperated sentiment by airline
ggplot(TwitterData, aes(x = airline, fill = airline_sentiment)) + 
  geom_bar(position = position_dodge()) +
  labs(title = "Seperated Sentiment by Airline") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5))

TwitterData$text <- as.character(TwitterData$text)
TwitterData_words <- TwitterData %>%
  unnest_tokens(word, text)

# filter out the common words that have no impact on sentiment
blacklist_words <- c("to", "the","i", "a", "you", "for", "on", "and", "is", "are", "am", 
                     "my", "in", "it", "me", "of", "was", "your", "so","with", "at", "just", "this",
                     "http", "t.co", "have", "that", "be", "from", "will", "we", "an", "can")

# Positive Word Analysis
positive_sentiment <- TwitterData_words %>% 
  filter(airline_sentiment == "positive")      

positive_sentiment <- positive_sentiment %>%
  count(word, sort = TRUE) %>%
  filter(!(word %in% blacklist_words)) %>% 
  rename(freq = n)

head(positive_sentiment, 20)

positive_sentiment <- positive_sentiment %>% top_n(20) %>% 
  mutate(word = reorder(word, freq))

wordcloud2(positive_sentiment[1:20,], size = .5, color = rev(brewer.pal(9,'Blues')))

ggplot(positive_sentiment, aes(x = word, y = freq)) + 
  geom_bar(stat="identity", width=.5, fill="blue") + 
  labs(title="Most Frequent Positive Words", 
       subtitle="Top 20 Words") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5))

# Negative analysis
negative_sentiment <- TwitterData_words %>% 
  filter(airline_sentiment == "negative")      

negative_sentiment <- negative_sentiment %>%
  count(word, sort = TRUE) %>%
  filter(!(word %in% blacklist_words)) %>% 
  rename(freq = n)

head(negative_sentiment, 20)

negative_sentiment <- negative_sentiment %>% top_n(20) %>% 
  mutate(word = reorder(word, freq))

wordcloud2(negative_sentiment[1:20,], size = .5, color = rev(brewer.pal(9,'Reds')))

ggplot(negative_sentiment, aes(x = word, y = freq)) + 
  geom_bar(stat="identity", width=.5, fill="red") + 
  labs(title="Most Frequent Negative Words", 
       subtitle="Top 20 Words") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5))

# Neutral word analysis
neutral_sentiment <- TwitterData_words %>% 
  filter(airline_sentiment == "neutral")      

neutral_sentiment <- neutral_sentiment %>%
  count(word, sort = TRUE) %>%
  filter(!(word %in% blacklist_words)) %>% 
  rename(freq = n)

head(neutral_sentiment, 20)

neutral_sentiment <- neutral_sentiment %>% top_n(20) %>% 
  mutate(word = reorder(word, freq))

wordcloud2(negative_sentiment[1:20,], size = .5, color = rev(brewer.pal(9,'Greens')))

ggplot(neutral_sentiment, aes(x = word, y = freq)) + 
  geom_bar(stat="identity", width=.5, fill="green") + 
  labs(title="Most Frequent Neutral Words", 
       subtitle="Top 20 Words") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5))

# Negative Reasons
TwitterData %>%
  filter(negativereason != "") %>%
  ggplot(aes(x = negativereason)) + 
  geom_bar(fill = "red") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Reasons for Negative Sentiment for All")

# by airline facets
TwitterData %>%
  filter(negativereason != "") %>% 
  ggplot(aes(x = negativereason)) + 
  geom_bar(fill = "red") +
  facet_wrap(airline ~ .) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = "Reasons for Negative Sentiment by Airline")

#  sentiment by location (from timezone)
TwitterData %>% 
  filter(user_timezone != "") %>% 
  ggplot(aes(x = user_timezone, fill = airline_sentiment)) +
  geom_bar() + 
  labs(title = "Stacked Sentiment by Timezone") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# sentiment by day
ggplot(TwitterData, aes(x=tweet_created, fill = airline_sentiment)) +
  geom_bar(position = position_dodge()) + 
  facet_wrap(airline ~ .) +
  labs(title = "Sentiment by Date and Airline") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

TwitterData %>% 
  filter(user_timezone == "Central Time (US & Canada)"|
           user_timezone == "Eastern Time (US & Canada)" |
           user_timezone == "Mountain Time (US & Canada)" |
           user_timezone == "Pacific Time (US & Canada)") %>% 
  ggplot(aes(x=tweet_created, fill = airline_sentiment)) +
  geom_bar(position = position_dodge()) + 
  facet_wrap(negativereason ~ .) +
  labs(title = "Sentiment by Date and Timezone") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

TwitterData %>% 
  filter(negativereason != "" &
           negativereason != "Damaged Luggage") %>% 
  ggplot(aes(x=tweet_created, fill = airline)) +
  geom_bar(position = position_dodge()) + 
  facet_wrap(negativereason ~ .) +
  labs(title = "Negative Sentiment by Airline and Date") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

