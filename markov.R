library(varhandle)
library(stringr)
library(dplyr)

# Markov models
# bigram
# provide word
NumberOfWords <- function(samp) {
    samp <- str_trim(gsub("\\s+", " ", samp))
    sps <- gregexpr(" ", samp)[[1]]
    csps <- length(sps[which(sps>0)]) + 1
    csps
}

history <- readline(prompt="Enter words (two or one): ")
history <- paste0(str_trim(history), " ")

#check 
#bigrams <- ngram(sample, 2)

srchstr <- paste0("^", history)

#how many words?
numwords <- NumberOfWords(history)

pickin <- grep(srchstr, twowords, useBytes=TRUE)

universe <- twowords[pickin]

denominator <- length(universe)

thewords <- substr(universe, nchar(history) + 1, stop=nchar(universe)) 

words <- as.data.frame(sort(table(thewords), decreasing=TRUE))
words <- transform(words, prob=Freq / denominator)

answer <- unfactor(head(words$thewords, 3))