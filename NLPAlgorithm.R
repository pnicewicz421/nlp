# Load libraries
library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)

#Word Manipulators
NumberOfWords <- function(samp) {
    samp <- str_trim(gsub("\\s+", " ", samp))
    sps <- gregexpr(" ", samp)[[1]]
    csps <- length(sps[which(sps>0)]) + 1
    csps
}

GetLastWord <- function(phrase){
    word(phrase, -1)
}

GetLastWords <- function(phrase, n){
    word(phrase, -n)
}

GetWord <- function(phrase, n){
    word(phrase, n)
}

CutFirstWord <- function(phrase){
    word(phrase, 2, NumberOfWords(phrase))
}

RemoveWhiteSpace <- function(phrase){
    gsub("\\s+", " ", phrase)
}


# Load stop words
data(stop_words)

# Get file names
enfileblogs <- "en_US.blogs.txt"
enfilenews <- "en_US.news.txt"
enfiletwitter <- "en_US.twitter.txt"

# Read files
enblogs <- read_lines(enfileblogs)
ennews <- read_lines(enfilenews)
entwitter <- read_lines(enfiletwitter)

# Take samples
samplesize = 0.2

set.seed(2243)
blogsample <- sample(enblogs, samplesize*length(enblogs))
newssample <- sample(ennews, samplesize*length(ennews))
twittersample <- sample(entwitter, samplesize*length(entwitter))

# Convert samples to data frames
blog_df <- data_frame(line = 1:length(blogsample), text=blogsample)
news_df <- data_frame(line = 1:length(newssample), text=newssample)
twitter_df <- data_frame(line = 1:length(twittersample), text=twittersample)

# Convert data frames of samples to tokens
blogtoken <- blog_df %>% unnest_tokens(word, text)
newstoken <- news_df %>% unnest_tokens(word, text)
twittertoken <- twitter_df %>% unnest_tokens(word, text)

sample_df <- rbind(blog_df, news_df, twitter_df)
sampletoken <- sample_df %>% unnest_tokens(word, text)

# Count most common tokens / words
commontokens <- sampletoken %>% count(word, sort = TRUE)
commontokens_nostopwords <- commontokens %>% anti_join(stop_words)

# Visualize most common tokens (with stop words)
commontokens %>% 
    filter(n > 200000) %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    ggtitle("Most Common Tokens (Stop Words Included)") +
    xlab('Number') + 
    coord_flip()

# Visualize most common tokens (without stop words)
commontokens_nostopwords %>% 
    filter(n > 15000) %>% 
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    ggtitle("Most Common Tokens (Without Stop Words)") +
    xlab('Number') + 
    coord_flip()

# Add the tf, idf, and tf-idf terms to the token count 
totalwords <- commontokens_nostopwords %>%
    summarize(total=sum(n))
totalwords <- as.integer(totalwords)
totalwords <- rep(totalwords, dim(commontokens_nostopwords)[1])

commontokens_nostopwords <- cbind(commontokens_nostopwords, totalwords)
commontokens_nostopwords <- commontokens_nostopwords %>% mutate(tf=n/totalwords)
head(commontokens_nostopwords, 25)

# Generate bigrams
bigrams <- sample_df %>%
    unnest_tokens(bigram, text, token="ngrams", n = 2)

# Get the counts with stop words included
bigrams <- bigrams %>% count(bigram, sort=TRUE)

# Seperate the bigram into two words
bigrams_separated <- bigrams %>% 
    separate(bigram, c("word1", "word2"), sep=" ")

# Remove stop words from the bigrams
bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

# Find the total number of bigrams
totalbigrams <- bigrams_separated %>%
    summarize(total=sum(n))
totalbigrams <- as.integer(totalbigrams)
totalbigrams <- rep(totalbigrams, dim(bigrams_separated)[1])

# Find the term frequency
bigrams <- cbind(bigrams_separated, totalbigrams)
bigrams <- bigrams %>% mutate(tf=n/totalbigrams)
head(bigrams)

# Generate an igraph object
# Filter out by the most common bigrams

bigram_graph <- bigrams %>%
    filter (n > 350) %>%
    graph_from_data_frame()

# Generate the network graph using ggraph
set.seed(3226)
arrow <- grid::arrow(type="closed", length=unit(.15, "inches"))
ggraph(bigram_graph, layout="fr") +
    geom_edge_link(aes(edge_alpha=n), show.legend = FALSE, arrow=arrow) +
    geom_node_point(color="blue", size=2) +
    geom_node_text(aes(label=name), vjust=1, hjust=1) +
    theme_void()

trigrams <- sample_df %>%
    unnest_tokens(trigram, text, token="ngrams", n=3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
    count(word1, word2, word3, sort=TRUE)

totaltrigrams <- trigrams %>%
    summarize(total=sum(n))
totaltrigrams <- as.integer(totaltrigrams)
totaltrigrams <- rep(totaltrigrams, dim(trigrams)[1])

# Find the term frequency
trigrams <- cbind(trigrams, totaltrigrams)
trigrams <- trigrams %tri>% mutate(tf=n/totaltrigrams)
head(trigrams)

#Take phrase <-
phrase <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"

#Build the model

# phrase is cleaned up but I think I will keep the stop-words in there. Doesn't Swwift Key do that? In fact, no one on the coursera team ever talked about excluding stop-words.
# They are best included in prediction, best excluded in topic analysis

# start with phrase

trytrigram <- GetLastWords(phrase, 2)
cands <- trigrams[trigrams$word1==GetWord(phrase, 1) & trigrams$word2==GetWord(phrase, 2),]
cands <- Get