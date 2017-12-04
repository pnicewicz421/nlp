# Part 2 - Frequencies

# Get frequencies of words in tokens
# 1-gram
tokens <- str_trim(tokens)
onewordtable <- sort(table(tokens), decreasing=T)

 
p <- barplot(head(onewords, n = 50), las=2, space=1, col="green")

# 2-gram
twogramblog <- ngram(blogsample, 2)
twogramnews <- ngram(newsample, 2)
twogramtwitter <- ngram(twittersample, 2)

twowords <- c(twogramblog, twogramnews, twogramtwitter)
twowordtable <- sort(table(twowords), decreasing=T)
p <- barplot(head(twowords, n = 50), las=2, space=1, col="green")

# 3-gram
threegramblog <- ngram(blogsample, 3)
threegramnews <- ngram(newsample, 3)
threegramtwitter <- ngram(twittersample, 3)

threewords <- c(threegramblog, threegramnews, threegramtwitter)
threewordtable <- sort(table(threewords), decreasing=T)

p <- barplot(head(threewords, n = 50), las=2, space=1, col="green")