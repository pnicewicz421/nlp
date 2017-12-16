library(varhandle)
library(stringr)
library(dplyr)

# Markov models
# bigram
# provide word


# preload ngrams



NumberOfWords <- function(samp) {
    samp <- str_trim(gsub("\\s+", " ", samp))
    sps <- gregexpr(" ", samp)[[1]]
    csps <- length(sps[which(sps>0)]) + 1
    csps
}

process_gram <- function(phrase) {
  # Count number of words
  numwords <- NumberOfWords(phrase)    
  # Create n-grams based on number of words
  create_gram <- ngram(phrase, numwords)
  if (length(create_gram) == 0) {
    firstspace <- regexec(" ", phrase)
    phrase <- substr(phrase, firstplace + 1)
    answer <- process_gram(phrase)
  }
  else {
    
  }
}

# PICK THE PHRASE
history <- ""

#check 
#bigrams <- ngram(sample, 2)

srchstr <- paste0("^", history)

#how many words?


answer <- process_gram(history)





pickin <- grep(srchstr, twowords, useBytes=TRUE)



universe <- twowords[pickin]

denominator <- length(universe)

thewords <- substr(universe, nchar(history) + 1, stop=nchar(universe)) 

words <- as.data.frame(sort(table(thewords), decreasing=TRUE))
words <- transform(words, prob=Freq / denominator)

answer <- unfactor(head(words$thewords, 3))