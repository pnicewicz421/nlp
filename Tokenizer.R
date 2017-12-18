# Load libraries
library(readr)
library(stringr)
library(stringi)

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
deprofane <- function(file, badwords){
    file <- file[-which(file %in% badwords)]
    file
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
#sample <- to(lowersample)

#
#megasample <- list()
## Pre-load lists 
#for (i in 1:25) {
 #   ngram_sample <- ngram(sample, i)
 #   megasample[[i]] <- ngram_sample
#}
