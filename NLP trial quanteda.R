# Load libraries
library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/qdapDictionaries")
data(contractions)

library(readtext)
library(quanteda)
library(stringi)
library(readr)
library(tidytext)
library(data.table)
library(ngram)

# Profanity filtering
badwords <- read_lines("badwords.txt")

# Read files and put the in one corpus


#START OVER
enfileblogs <- "en_US.blogs.txt"
enfilenews <- "en_US.news.txt"
enfiletwitter <- "en_US.twitter.txt"

# Read files
enblogs <- read_lines(enfileblogs)
ennews <- read_lines(enfilenews)
entwitter <- read_lines(enfiletwitter)

# Take samples
samplesize = 0.5

set.seed(2243)
blogsample <- sample(enblogs, samplesize*length(enblogs))
newssample <- sample(ennews, samplesize*length(ennews))
twittersample <- sample(entwitter, samplesize*length(entwitter))

# Convert samples to data frames
blog_df <- data_frame(line = 1:length(blogsample), text=blogsample)
news_df <- data_frame(line = 1:length(newssample), text=newssample)
twitter_df <- data_frame(line = 1:length(twittersample), text=twittersample)

df <- rbind(blog_df, news_df, twitter_df)
encorpus <- corpus(df)

# Clean up
# Deprofane

# Expand contractions

texts(encorpus) <- stri_trans_tolower(texts(encorpus)) 

texts(encorpus) <- stringi::stri_replace_all_fixed(texts(encorpus), 
                                                   contractions$contraction,
                                                   contractions$expanded,
                                                   vectorize_all=FALSE)

texts(encorpus) <- stringi::stri_replace_all_fixed(texts(encorpus), badwords, "",
                                                    vectorize_all=FALSE)


# Tokenize into individual tokens (unigrams)
unigrams <- tokens(texts(encorpus), remove_numbers = FALSE)
bigrams <- tokens_ngrams(unigrams, n=2, concatenator=" ")
trigrams <- tokens_ngrams(unigrams, n=3, concatenator=" ")
fourgrams <- tokens_ngrams(unigrams, n=4, concatenator=" ")

    
# Convert samples to data frames
blog_df <- data_frame(line = 1:length(blogsample), text=blogsample)
news_df <- data_frame(line = 1:length(newssample), text=newssample)
twitter_df <- data_frame(line = 1:length(twittersample), text=twittersample)

# Convert data frames of samples to tokens
blogtoken <- blog_df %>% unnest_tokens(word, text)
newstoken <- news_df %>% unnest_tokens(word, text)
twittertoken <- twitter_df %>% unnest_tokens(word, text)