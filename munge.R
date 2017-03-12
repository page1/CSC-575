library(dplyr)
library(tm)
library(SnowballC)

munge_na <- function(data){
  na_maker <- function(x){
    x[x == 99] <- NA # Dataset has 99's in place of NA's
    return(x)
  }
  
  data %>%
    mutate_each(funs(na_maker))
}

make_tf_idf <- function(jokes){
  jokeCorp <- Corpus(VectorSource(jokes$joke[order(jokes$joke_number)]))
  jokeCorp <- tm_map(jokeCorp, removePunctuation)
  jokeCorp <- tm_map(jokeCorp, removeNumbers)
  jokeCorp <- tm_map(jokeCorp, tolower)
  jokeCorp <- tm_map(jokeCorp, removeWords, stopwords("english"))
  jokeCorp <- tm_map(jokeCorp, stripWhitespace)
  jokeCorp <- tm_map(jokeCorp, stemDocument)
  
  dtm <- DocumentTermMatrix(jokeCorp, control = list(weighting = weightTfIdf))
  dtm_less_sparce <- removeSparseTerms(dtm, 0.98) # Just so we don't go too wide lets cut down to more common words
  
  doc_term_matrix_idf <- as.matrix(dtm_less_sparce)
  
  return(doc_term_matrix_idf)
}