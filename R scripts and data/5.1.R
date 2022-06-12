# Part 1: Text pre-processing ----

# Load packages required for this session into library

library(vosonSML)
library(magrittr)
library(tidyr)
library(tidytext)
library(stopwords)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)
library(ggplot2)

# Set up current working directory
#setwd("~/Documents/big data analytics and social media/labs")

# Set up Twitter authentication variables
my_app_name <- "7230ICT_Qin"
my_api_key <- "O2JTa6TiP4RqFTi5AhIjaEO5b"
my_api_secret <- "IIRS6pR8vakdvK7UE8SOCuNa2wW9NBAUdT3kanXYt3J43TCOgq"
my_access_token <- "1501187128038166532-Iwrrqjt1TDszF1MRES9cwNwSTvzN2u"
my_access_token_secret <- "ducOdck8wd3L6UAaKysiMEI3gj8DqFN8YaS7oYVuBZ4Nr"


# Import data when answering 1.5
twitter_data_Ed_retweets   <- ImportData("TwitterData_Retreets.rds", "twitter")

# Create semantic network and graph from the data

twitter_semantic_network <- twitter_data_Ed_retweets %>% Create("semantic")
twitter_semantic_graph <- twitter_semantic_network %>% Graph()


# Run Page Rank algorithm to find important terms/hashtags

rank_twitter_semantic <- sort(page_rank(twitter_semantic_graph)$vector, decreasing = TRUE)
head(rank_twitter_semantic, n = 10)

# Clean the tweet text

clean_text <- twitter_data_Ed_retweets$text %>% 
    rm_twitter_url() %>% 
    replace_url() %>% 
    replace_hash() %>% 
    replace_tag() %>% 
    replace_emoji() %>% 
    replace_emoticon()


# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content



# Perform further pre-processing 

text_corpus <- text_corpus %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeWords, stopwords(kind = "SMART")) %>% 
    tm_map(stemDocument) %>% 
    tm_map(stripWhitespace)

text_corpus[[1]]$content
text_corpus[[5]]$content

# raw text
text_corpus1 <- VCorpus(VectorSource(twitter_data_Ed_retweets$text))
doc_term_matrix1 <- DocumentTermMatrix(text_corpus1)
dtm_df1 <- as.data.frame(as.matrix(doc_term_matrix1))
View(dtm_df)
freq1 <- sort(colSums(dtm_df1), decreasing = TRUE)
head(freq1, n = 10)

# Transform corpus into a Document Term Matrix

doc_term_matrix <- DocumentTermMatrix(text_corpus)

inspect(doc_term_matrix)
# Sort words by total frequency across all documents

dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
View(dtm_df)

freq <- sort(colSums(dtm_df), decreasing = TRUE)

head(freq, n = 10)


# Plot word frequency

word_frequ_df <- data.frame(word = names(freq), freq)
View(word_frequ_df)

ggplot(subset(word_frequ_df, freq > 2926), aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Word Frequency") + 
    xlab("Words") + 
    ylab("Frequency")



