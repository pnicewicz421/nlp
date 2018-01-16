# Load libraries
library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)

library(readtext)
library(quanteda)

# Profanity filtering
deprofane <- function(file, badwords="badwords.txt"){
    file <- file[-which(file %in% badwords)]
    file
}

# Read files and put the in one corpus
txts <- readtext("*en_US.*.txt", docvarsfrom="filenames")
encorpus <- corpus(txts)

# Clean up
try <- deprofane(encorpus)

# Tokenize into individual tokens (unigrams)
unigrams <- tokens(encorpus, remove_numbers = FALSE, remove_punct = TRUE,
                   remove_separators = TRUE)
bigrams <- tokens_ngrams(encorpus, n=2, concatenator=" ")
trigrams <- tokens_ngrams(encorpus, n=3, concatenator=" ")
fourgrams <- tokens_ngrams(encorpus, n=4, concatenator=" ")
fivegrams <- tokens_ngrams(encorpus, n=5, concatenator=" ")
    
# Convert samples to data frames
blog_df <- data_frame(line = 1:length(blogsample), text=blogsample)
news_df <- data_frame(line = 1:length(newssample), text=newssample)
twitter_df <- data_frame(line = 1:length(twittersample), text=twittersample)

# Convert data frames of samples to tokens
blogtoken <- blog_df %>% unnest_tokens(word, text)
newstoken <- news_df %>% unnest_tokens(word, text)
twittertoken <- twitter_df %>% unnest_tokens(word, text)