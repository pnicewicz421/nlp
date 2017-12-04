library(varhandle)
library(dplyr)

# Markov models
# bigram
# provide word
history <- readline(prompt="Enter one word: ")

history <- paste0(str_trim(history), " ")

#check 
#bigrams <- ngram(sample, 2)

srchstr <- paste0("^", history)
pickin <- grep(srchstr, twowords, useBytes=TRUE)
universe <- twowords[pickin]

denominator <- length(universe)

thewords <- substr(universe, nchar(history)+1, stop=nchar(universe)) 

words <- as.data.frame(sort(table(thewords), decreasing=TRUE))
words <- transform(words, prob=Freq / denominator)

answer <- unfactor(head(words$thewords, 3))