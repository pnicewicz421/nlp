# Evaluate the coverage of words in English and foreign words

# How many unique words do you need in a frequency sorted dictionary to cover 
# 50% of all word instances in the language? 90%?
enwords <- read_lines("englishwords.txt")
enwords <- tolower(enwords)
lowertokens <- tolower(tokens)
unitokens <- unique(lowertokens)
inenglish <- unitokens %in% enwords
enlength <-  length(inenglish[which(inenglish==TRUE)])
outenlength <- length(inenglish[which(inenglish==FALSE)])
perc <- enlength / length(enwords)

enratio <- enlength/(outenlength+enlength)

# How do you evaluate how many of the words come from foreign languages?
outenglish <- unitokens[-which(unitokens %in% enwords)]
outenglish <- outenglish[-grep("^[[:punct:]]*$", outenglish)]

# Can you think of a way to increase the coverage -- 
# identifying words that may not be in the corpora or using a smaller number of 
# words in the dictionary to cover the same number of phrases?
# lowercase, break down "/"...