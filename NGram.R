

PredictWordsWithContext <- function(phrase, threshold=5) {
    
    # Get the last two words to use as ngram
    #ngram <- GetLastWords(phrase, 2)
    
    srchstr <- paste0(phrase, "[[:space:]][[:alnum:]]+")
    
    poss <- grep(srchstr, sample, useBytes=TRUE, value=TRUE)
    possl <- str_match(poss, srchstr)
    
    #poss
    
    if (length(possl) > threshold) {
        # look at the context
        srchstr <- gsub(" ", "|", phrase)
        conts <- gregexpr(srchstr, sample, useBytes = TRUE)
        NumberOfWords(conts)
        
        #which(conts[1]>0)
        
        freq <- as.data.frame(sort(table(possl), decreasing=TRUE))
    }  else if (length(possl) == 1){
        freq <- row.names(as.data.frame(sort(table(possl), decreasing=TRUE)))
    } else if (length(possl) == 0) {
        phrase <- CutFirstWord(phrase)
        if (NumberOfWords(phrase) > 0) {
            freq <- GetAllWords(phrase)
        } else {
            freq <- NULL
        }
    }
    freq
}

phrase <- "case of"
a <- GetAllWords(phrase)

phrase <- "mean the"
b <- GetAllWords(phrase)

phrase <- "me the"
c <- GetAllWords(phrase)

phrase <- "but the"
d <- GetAllWords(phrase)

phrase <- "at the"
e <- GetAllWords(phrase)

phrase <- "on my"
f <- GetAllWords(phrase)

phrase <- "quite some"
g <- GetAllWords(phrase)

phrase <- "his little"
h <- GetAllWords(phrase)

phrase <- "during the"
i <- GetAllWords(phrase)

phrase <- "must be"
j <- GetAllWords(phrase)


phrase <- "Very early observations on the Bills game: Offense still struggling but the"
#phrase <- deprofane(phrase, badwords)
phrase <- splice(phrase)
phrase <- RemoveWhiteSpace(phrase)
#phrase <- tolower(phrase)
process_gram_no_preloading(phrase)


