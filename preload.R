# Load libraries
library(readr)
library(stringr)
library(stringi)
library(varhandle)
library(stringr)
library(dplyr)
library(tm)

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/qdapDictionaries")
data(contractions)

# Splice contractions and punctuation
splice <- function (samp) {
    #conts <- "n[’']t|[’']s|[’']d|[’']ve|[’']re|[’']m|[’']ll"
    puncts <- "^[[:punct:]]|[[:punct:]]{2,}|[[:punct:]][[:blank:]]|[[:blank:]][[:punct:]]|[\"]|[[:punct:]]$"
    fixed <- gsub(paste0("(", puncts, ")"), " \\1 ", samp)
    fixed <- stri_enc_toutf8(fixed, TRUE, TRUE)
    fixed <- str_trim(fixed)
}

# Profanity filtering
deprofane <- function(phrase, badwords){
    removeWords(phrase, badwords)
}

RemovePunctuation <- function(phrase) {
    phrase <- gsub("(?!')[[:punct:]]+", "", phrase, perl=TRUE)
}

RemoveNumbers <- function(phrase){
    phrase <- gsub('[[:digit:]]+', '', phrase)
}

RemoveTwitter <- function(phrase){
    phrase <- gsub("&amp", "", phrase)
    phrase <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", phrase)
    phrase <- gsub("@\\w+", "", phrase)
    phrase <- gsub("#\\w+", "", phrase)
    gsub('http\\S+\\s*', '', phrase)
}

RemoveWhiteSpace <- function(phrase){
    phrase <- gsub("^\\s+|\\s+$", "", phrase) 
    phrase <- gsub("\\s{2,}", " ", phrase)
}

LowerCase <- function(phrase){
    phrase <- stri_trans_tolower(phrase)
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
sample <- c(enblogsample, ennewssample, entwittersample)

badwords <- read_lines("badwords.txt")

# Splice and Process
#sample <- splice(sample)
contractions$contraction <- LowerCase(contractions$contraction)
contractions$expanded <- LowerCase(contractions$expanded)
sample <- LowerCase(sample)
sample <- stringi::stri_replace_all_regex(sample, 
                                    contractions$contraction,
                                    contractions$expanded,
                                    vectorize_all=FALSE)
sample <- deprofane(sample, badwords)
sample <- RemovePunctuation(sample)
sample <- RemoveNumbers(sample)
sample <- RemoveTwitter(sample)
sample <- RemoveWhiteSpace(sample)

write(sample, file="SamplePreProcessed.txt")