# Stemmer using Morpheus of the Perseus Hopper

setwd("~/OneDrive/GithubProjects/LatParser")

corpus_words <- readLines("godfrey-wordlist.txt")

corpus_words <- gsub("[[:punct:]]", " ", corpus_words)  # replace punctuation with space
corpus_words <- gsub("[[:cntrl:]]", " ", corpus_words)  # replace control characters with space
corpus_words <- gsub("^[[:space:]]+", "", corpus_words) # remove whitespace at beginning of documents
corpus_words <- gsub("[[:space:]]+$", "", corpus_words) # remove whitespace at end of documents
corpus_words <- gsub("[0-9]", "", corpus_words) #remove numbers

corpus_words <- gsub("^[[:space:]]+", "", corpus_words) # remove whitespace at beginning of documents

corpus_words <- unique(unlist(corpus_words))
corpus_words <- sort(corpus_words)
corpus_words <- tolower(corpus_words)

parsing2 <- function(x){
  URL <- paste("https://services.perseids.org/bsp/morphologyservice/analysis/word?word=", x, "&lang=lat&engine=morpheuslat", sep = "")
  message("Accessing ", URL)
  
  XMLpassage <-function(xdata){
    miner <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
    result <- xmlParse(xdata)
    temp.df <- as.data.frame(t(xpathSApply(result, "//*/hdwd", miner)), stringsAsFactors = FALSE)
    as.vector(temp.df[['text']])}
  
  URLcontent <- tryCatch({
    getURLContent(URL)}, 
    error = function(err)
    {message(x, " -query caused server error. Return original value.")
     content <- "ServerError"
     return(content)})
  if (URLcontent == "ServerError") {lemma <- "ServerError"
                                    return(lemma)}
  
  lemma <- if (is.null(XMLpassage(URLcontent)) == TRUE) {
    lemma <- "NotFound2"
    return(lemma)}
  else {tryCatch({XMLpassage(URLcontent)},
                 error = function(err) {
                   message(x, " not found. Return original value.")
                   lemma <- "NotFound1"
                   return(lemma)})}
  
  lemma <- gsub("[0-9]", "", lemma)
  lemma <- tolower(lemma)
  lemma <- unique(lemma)
  lemma <- paste(lemma, sep="", collapse="_")
  if (nchar(lemma) == 0) lemma <- x
  message(x, " is ", lemma)
  return(lemma)}

parsing <- function(x){
  URL <- paste("http://www.perseus.tufts.edu/hopper/xmlmorph?lang=lat&lookup=", x, sep = "")
  message("Accessing ", URL)
  
  XMLpassage <-function(xdata){
    miner <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
    result <- xmlParse(xdata)
    temp.df <- as.data.frame(t(xpathSApply(result, "//*/lemma", miner)), stringsAsFactors = FALSE)
    as.vector(temp.df[['text']])}
  
  URLcontent <- tryCatch({
    getURLContent(URL)}, 
    error = function(err)
    {message(x, " -query caused server error. Return original value.")
     content <- "ServerError"
     return(content)})
  if (URLcontent == "ServerError") {lemma <- parsing2(x)
                                    return(lemma)}
  
  lemma <- if (is.null(XMLpassage(URLcontent)) == TRUE) {
    lemma <- parsing2(x)
    return(lemma)}
  else {tryCatch({XMLpassage(URLcontent)},
                    error = function(err) {
                      message(x, " not found. Return original value.")
                      lemma <- "NotFound1"
                      return(lemma)})}
  
  lemma <- gsub("[0-9]", "", lemma)
  lemma <- tolower(lemma)
  lemma <- unique(lemma)
  lemma <- paste(lemma, sep="", collapse="_")
  if (nchar(lemma) == 0) lemma <- x
  message(x, " is ", lemma)
  return(lemma)}

#stemming

t1 <- Sys.time()

stem_dictionary <- sapply(corpus_words, parsing)

t2 <- Sys.time()
parsing_time <- t2 - t1

# Evaluate result

stem_dictionary.df <- as.data.frame(stem_dictionary)
frequency <- as.data.frame(table(stem_dictionary))
frequency <- arrange(frequency,desc(Freq))

duplicates <- rownames(stem_dictionary.df) == stem_dictionary.df[,1]
duplicate_log <- which(duplicates == TRUE)
duplicates.df <- as.data.frame(stem_dictionary[duplicate_log])
duplicates_number <- length(rownames(duplicates.df))

notfound1.df <- as.data.frame(stem_dictionary[which(stem_dictionary.df[,1] == "NotFound1")])
notfound1_number <- length(rownames(notfound1.df))

notfound2.df <- as.data.frame(stem_dictionary[which(stem_dictionary.df[,1] == "NotFound2")])
notfound2_number <- length(rownames(notfound2.df))

servererror.df <- as.data.frame(stem_dictionary[which(stem_dictionary.df[,1] == "ServerError")])
servererror_number <- length(rownames(servererror.df))

total_notfound <- notfound1_number + notfound2_number

percent_parsed <-  1 - total_notfound/length(corpus_words)
