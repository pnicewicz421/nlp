# Load libraries
library(data.table)
library(stringr)
library(stringi)

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

RemoveWhiteSpace <- function(phrase) {
    gsub("\\s+", " ", phrase)
}

LowerCase <- function(phrase){
    phrase <- stri_trans_tolower(phrase)
}

# Load with no pre-loading
process_gram <- function(phrase) {
    # Count number of words
    phrase <- RemoveWhiteSpace(phrase)
    phrase <- str_trim(phrase)
    numwords <- NumberOfWords(phrase)
    if (numwords > 5) {
        phrase <- GetLastWords(phrase, 4)
        numwords <- 4
    }
    phrase <- LowerCase(phrase)
    
    result <- datafile[firstwords==phrase]
    
    if (dim(result)[1]==0) {
        if (numwords > 1) {
            result <- process_gram(GetLastWords(phrase, numwords - 1))
        } else if (numwords<=1) {
            result <- "the"
        }
    }
    else if (dim(result)[1]>=1) {
        result <- result[1, lastword]
    } 
    
    result
 
}

datafile <- data.table(readRDS("ProcessedData.RDS"))
setkey(datafile, firstwords)