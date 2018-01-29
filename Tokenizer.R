# Load libraries
library(readr)
library(stringr)
library(stringi)
library(varhandle)
library(stringr)
library(dplyr)

# Splice contractions and punctuation
splice <- function (samp) {
    conts <- "n[’']t|[’']s|[’']d|[’']ve|[’']re|[’']m|[’']ll"
    puncts <- "^[[:punct:]]|[[:punct:]]{2,}|[[:punct:]][[:blank:]]|[[:blank:]][[:punct:]]|[\"]|[[:punct:]]$"
    fixed <- gsub(paste0("(", conts, "|", puncts, ")"), " \\1 ", samp)
    fixed <- stri_enc_toutf8(fixed, TRUE, TRUE)
    fixed <- str_trim(fixed)
}

# Divide up into tokens (i.e., 1-grams)
tokenize <- function(samp) {
    tokens <- strsplit(samp, split=" ")
    tokens <- unlist(tokens)
    # Remove empty elements
    tokens <- tokens[which(nchar(tokens)>0)]
    return (tokens)
}

# Divide up into n-grams 
ngram_helper <- function (samp, n) {
    #samp <- stri_enc_toutf8(samp, TRUE, TRUE)
    #samp <- str_trim(gsub("\\s+", " ", samp))
    sps <- gregexpr(" ", samp)[[1]]
    csps <- length(sps[which(sps>0)]) + 1
    if (csps >= n & n >=2) {
        
        reptext <- "[[:space:]]+[[:alnum:][:punct:]]+"
        addt <- paste(replicate(n-1, reptext), collapse = "")
        sp <- paste0("([[:alnum:][:punct:]]+", 
                     addt, ")")
        sps <- gsub(sp, "[\\1]", samp)
        
        ngrams <- unlist(str_extract_all(sps, "\\[.*\\]"))
        ngrams <- unlist(strsplit(ngrams, split="\\[|\\]"))
        ngrams <- ngrams[-which(ngrams==""|ngrams==" ")]
        ngrams <- gsub("^\\s+|\\s+$", "", ngrams)
    } else if (n==1) {
        return (tokenize(samp))
    } else {
        return (NULL)    
    }
}

ngram <- function (samp, n){
    for (i in 1:n) {
        if (i > 1) {
            offset <- regexpr("[[:space:]]", samp)[1]
            samp <- substr(samp, start=offset+1, stop=nchar(samp))
            result <- c(result, ngram_helper(samp, n))
        } else {
            result <- ngram_helper(samp, n)
        }
    }
    result
}

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
process_gram <- function(phrase) {
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
    else if (length(possl) == 0) {
        phrase <- CutFirstWord(phrase)
        if (NumberOfWords(phrase) > 0) {
            result <- process_gram_no_preloading(phrase)
        } else {
            result <- NULL
        }
    }
    result
}

preprocess_phrase <- function(phrase) {
    phrase <- splice(phrase)
    phrase <- deprofane(phrase, badwords)
    phrase <- RemoveWhiteSpace(phrase)
    phrase <- LowerCase(phrase)
}

# Load files
enfileblogs <- "en_US.blogs.txt"
enfilenews <- "en_US.news.txt"
enfiletwitter <- "en_US.twitter.txt"

enblogs <- read_lines(enfileblogs)
ennews <- read_lines(enfilenews)
entwitter <- read_lines(enfiletwitter)

# Take samples
samplesize = 1

set.seed(2243)
enblogsample <- sample(enblogs, samplesize*length(enblogs))
ennewssample <- sample(ennews, samplesize*length(ennews))
entwittersample <- sample(entwitter, samplesize*length(entwitter))
badwords <- read_lines("badwords.txt")

# Splice and Process
blogsample <- splice(enblogsample)
twittersample <- splice(entwittersample)
newsample <- splice(ennewssample)
sample <- c(blogsample, newsample, twittersample)
sample <- deprofane(sample, badwords)
sample <- RemoveWhiteSpace(sample)
sample <- LowerCase(sample)


