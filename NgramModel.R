# Part 2 - Frequencies

# Get frequencies of words in tokens
# 1-gram
onewordtable <- sort(table(tokens), decreasing=T)

UniGram <- as.data.frame(onewordtable)
denominator_UniGram <-sum(UniGram$Freq)
UniGram$Prob <- UniGram$Freq / denominator_UniGram

p <- barplot(head(onewords, n = 50), las=2, space=1, col="green")

# 2-gram
#twogramblog <- ngram(blogsample, 2)
#twogramnews <- ngram(newsample, 2)
#twogramtwitter <- ngram(twittersample, 2)

#twowords <- c(twogramblog, twogramnews, twogramtwitter)
twowords <- ngram(sample, 2)
twowordtable <- sort(table(twowords), decreasing=T)

p <- barplot(head(twowords, n = 50), las=2, space=1, col="green")

BiGram <- as.data.frame(twowordtable)

# Unadjusted Prob
denom <- sum(BiGram$Freq)

BiGram$Prob <- BiGram$Freq / denom

#Add one smoothing
BiGram$AddOne <- BiGram$Freq + 1

# Calculate Prob
PotentialBigrams <- dim(UniGram)[1]^2

# Number of bigrams with zero probability
# i.e., Potential Bigrams - Bigrams with Non-Zero Probability
ZeroProbCount <- PotentialBigrams - dim(BiGram)[1]

# Adjusted Denominator
denominator_BiGram <- sum(BiGram$AddOne) + ZeroProbCount

# Adjusted Probab
BiGram$Adj_Prob <- log(BiGram$AddOne / denominator_BiGram)

# Prob for each Zero Probability entry
Adjusted_Zero_Prob <- log(1 / denominator_BiGram)


# Use backoff model for bigrams
# Get second word from bigrams

twowords <- twowords[which(nchar(twowords)>0)]

srchstr <- paste0("[[:blank:]]")
twowords <- twowords[grep(srchstr, twowords)]

pickin <- regexpr(srchstr, twowords, useBytes=TRUE, perl=TRUE)
pickin <- pickin[pickin > 0]

secondwords <- substr(twowords, pickin + 1, stop=nchar(twowords)) 

# Number to shoot for
NumZeroProbs <- length(unique(tokens)) - length(unique(secondwords))
ZeroProbTokens <- unique(tokens)[!(unique(tokens) %in% unique(secondwords))]

ZeroProbTokens <- as.data.frame(ZeroProbTokens)

ZeroProbTokens$Freq <- UniGram[match(ZeroProbTokens$ZeroProbTokens,
                                     UniGram$tokens), 2]

# 3-gram
threegramblog <- ngram(blogsample, 3)
threegramnews <- ngram(newsample, 3)
threegramtwitter <- ngram(twittersample, 3)

threewords <- c(threegramblog, threegramnews, threegramtwitter)
threewordtable <- sort(table(threewords), decreasing=T)

p <- barplot(head(threewords, n = 50), las=2, space=1, col="green")twogramtwitter <- ngram(twittersample, 2)