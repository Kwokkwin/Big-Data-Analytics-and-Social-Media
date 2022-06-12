# Sentiment & Emotion Analysis --------------------------------------------

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(tidytext)
library(textclean)
library(qdapRegex)
library(syuzhet)
library(ggplot2)


# Set up Twitter authentication variables

my_app_name <- "7230ICT_Qin"
my_api_key <- "O2JTa6TiP4RqFTi5AhIjaEO5b"
my_api_secret <- "IIRS6pR8vakdvK7UE8SOCuNa2wW9NBAUdT3kanXYt3J43TCOgq"
my_access_token <- "1501187128038166532-Iwrrqjt1TDszF1MRES9cwNwSTvzN2u"
my_access_token_secret <-"ducOdck8wd3L6UAaKysiMEI3gj8DqFN8YaS7oYVuBZ4Nr"


# Authenticate to Twitter and collect data

twitter_data <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
    Collect(searchTerm ="#Edsheeran",
            searchType = "recent",
            numTweets = 10000,
            lang = "en",
            includeRetweets = TRUE,
            writeToFile = TRUE)


twitter_data_song <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
    Collect(searchTerm = "'the shape of you'",
            searchType = "recent",
            numTweets = 10000,
            lang = "en",
            includeRetweets = FALSE,
            writeToFile = TRUE)



# Clean the tweet text

clean_text <- twitter_data$text  %>% 
    rm_twitter_url() %>% 
    replace_url() %>% 
    replace_hash() %>% 
    replace_tag() %>% 
    replace_internet_slang() %>% 
    replace_emoji() %>% 
    replace_emoticon() %>% 
    replace_non_ascii() %>% 
    replace_contraction() %>% 
    gsub("[[:punct:]]", " ", .) %>% 
    gsub("[[:digit:]]", " ", .) %>% 
    gsub("[[:cntrl:]]", " ", .) %>% 
    gsub("\\s+", " ", .) %>% 
    tolower()


# Clean the song tweet text 

clean_text_song <- twitter_data_song$text  %>% 
    rm_twitter_url() %>% 
    replace_url() %>% 
    replace_hash() %>% 
    replace_tag() %>% 
    replace_internet_slang() %>% 
    replace_emoji() %>% 
    replace_emoticon() %>% 
    replace_non_ascii() %>% 
    replace_contraction() %>% 
    gsub("[[:punct:]]", " ", .) %>% 
    gsub("[[:digit:]]", " ", .) %>% 
    gsub("[[:cntrl:]]", " ", .) %>% 
    gsub("\\s+", " ", .) %>% 
    tolower()

# Assign sentiment scores to tweets

sentiment_scores <- get_sentiment(clean_text, method = "afinn") %>% sign()

sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
View(sentiment_df)

# Assign sentiment scores to tweets song

sentiment_song_scores <- get_sentiment(clean_text_song, method = "afinn") %>% sign()

sentiment_song_df <- data.frame(text = clean_text_song, sentiment = sentiment_song_scores)
View(sentiment_song_df)

# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df)


# Convert sentiment scores song to labels: positive, neutral, negative

sentiment_song_df$sentiment <- factor(sentiment_song_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_song_df)
# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
    geom_bar(aes(fill = sentiment)) +
    scale_fill_brewer(palette = "RdGy") +
    labs(fill = "Sentiment") +
    labs(x = "Sentiment Categories", y = "Number of Tweets") +
    ggtitle("Sentiment Analysis of Tweets")


# Plot song sentiment classification

ggplot(sentiment_song_df, aes(x = sentiment)) +
    geom_bar(aes(fill = sentiment)) +
    scale_fill_brewer(palette = "RdGy") +
    labs(fill = "Sentiment") +
    labs(x = "Sentiment Categories", y = "Number of Tweets") +
    ggtitle("Sentiment Analysis of Tweets:The shape of you")


# Assign emotion scores to tweets

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)

# Assign emotion scores to tweets

emo_scores_song <- get_nrc_sentiment(clean_text_song)[ , 1:8]

emo_scores_song_df <- data.frame(clean_text_song, emo_scores_song)
View(emo_scores_song_df)

# Calculate proportion of emotions across all tweets

emo_sums <- emo_scores_df[,2:9] %>% 
    sign() %>% 
    colSums() %>% 
    sort(decreasing = TRUE) %>% 
    data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)

# Calculate proportion of emotions song across all tweets

emo_song_sums <- emo_scores_song_df[,2:9] %>% 
    sign() %>% 
    colSums() %>% 
    sort(decreasing = TRUE) %>% 
    data.frame() / nrow(emo_scores_song_df) 

names(emo_song_sums)[1] <- "Proportion" 
View(emo_song_sums)


# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
    geom_col() +
    coord_flip()+
    guides(fill = "none") +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "Emotion Categories", y = "Proportion of Tweets") +
    ggtitle("Emotion Analysis of Tweets")



# Plot song emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_song_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_song_sums))) +
    geom_col() +
    coord_flip()+
    guides(fill = "none") +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "Emotion Categories", y = "Proportion of Tweets") +
    ggtitle("Emotion Analysis of song the shape of you")
