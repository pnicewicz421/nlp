# Load libraries
library(readr)
library(stringr)
library(stringi)
library(varhandle)
library(stringr)
library(dplyr)

source('preload.R')

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

create_frequencies <- function(ngram) {
    freq <- sort(table(ngram), decreasing=T)
}



ngram1 <- ngram(sample, 1)
freq1 <- create_frequencies(ngram1)
write.csv(freq1, "ngram1.csv", row.names=FALSE)
rm(list = setdiff(ls(), c("sample", lsf.str())))

ngram2 <- ngram(sample, 2)
freq2 <- create_frequencies(ngram2)
write.csv(freq2, "ngram2.csv", row.names=FALSE)
rm(list = setdiff(ls(), c("sample", lsf.str())))

ngram3 <- ngram(sample, 3)
freq3 <- create_frequencies(ngram3)
write.csv(freq3, "ngram3.csv", row.names=FALSE)
rm(list = setdiff(ls(), c("sample", lsf.str())))

ngram4 <- ngram(sample, 4)
freq4 <- create_frequencies(ngram4)
write.csv(freq4, "ngram4.csv", row.names=FALSE)
rm(list = setdiff(ls(), c("sample", lsf.str())))

ngram5 <- ngram(sample, 5)
freq5 <- create_frequencies(ngram5)
write.csv(freq5, "ngram5.csv", row.names=FALSE)
rm(list = setdiff(ls(), c("sample", lsf.str())))