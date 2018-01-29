# Load libraries
library(readr)
library(stringr)
library(stringi)
library(varhandle)
library(stringr)
library(dplyr)

# Profanity filtering
deprofane <- function(phrase, badwords){
    removeWords(phrase, badwords)
}

NumberOfWords <- function(samp) {
    samp <- str_trim(gsub("\\s+", " ", samp))
    sps <- gregexpr(" ", samp)[[1]]
    csps <- length(sps[which(sps>0)]) + 1
    csps
}

GetLastWord <- function(phrase){
    word(phrase, -1)
}

GetLastWords <- function(phrase, number){
    word(phrase, -number, -1)
}

CutFirstWord <- function(phrase){
    word(phrase, 2, NumberOfWords(phrase))
}

RemoveWhiteSpace <- function(phrase){
    gsub("\\s+", " ", phrase)
}

LowerCase <- function(phrase){
    phrase <- stri_trans_tolower(phrase)
}

# Load with no pre-loading
process_gram <- function(phrase, sample) {
    # Count number of words
    if (NumberOfWords(phrase) > 5) {
        phrase <- GetLastWords(phrase, 5)
    }
    srchstr <- paste0(phrase, "[[:space:]][[:alnum:]]+")
    poss <- grep(srchstr, sample, useBytes=TRUE, value=TRUE)
    possl <- str_match(poss, srchstr)
    
    if (length(possl) > 1) {
        # passedl
        freq <- as.data.frame(sort(table(possl), decreasing=TRUE))[1,1]
        result <- GetLastWord(freq)
    }  else if (length(possl) == 1){
        freq <- row.names(as.data.frame(sort(table(possl), decreasing=TRUE)))[1]
        result <- GetLastWord(freq)
    }
    else if (length(possl) == 0 & NumberOfWords(phrase)>1) {
        phrase <- CutFirstWord(phrase)
        if (NumberOfWords(phrase) > 0) {
            result <- process_gram(phrase)
        } else {
            result <- "the"
        }}
    else if (length(possl) == 0 & NumberOfWords(phrase)==1){
        result <- "the"
    }
    result
}

preprocess_phrase <- function(phrase) {
    contractions$contraction <- LowerCase(contractions$contraction)
    contractions$expanded <- LowerCase(contractions$expanded)
    phrase <- LowerCase(phrase)
    phrase <- stringi::stri_replace_all_regex(phrase, 
                                              contractions$contraction,
                                              contractions$expanded,
                                              vectorize_all=FALSE)
    phrase <- deprofane(phrase, badwords)
    phrase <- RemovePunctuation(phrase)
    phrase <- RemoveNumbers(phrase)
    phrase <- RemoveTwitter(phrase)
    phrase <- RemoveWhiteSpace(phrase)
    phrase
}

badwords <- read_lines("badwords.txt")
sample <- read_lines("SamplePreProcessed.txt")