library(stringr)

get_ratings <- function(){
  rbind(read.csv("data/ratings/jester-data-1.csv"),
        read.csv("data/ratings/jester-data-2.csv"),
        read.csv("data/ratings/jester-data-3.csv"))
}

get_joke_file_names <- function(){
  list.files("data/jokes", full.names = T)
}

get_jokes <- function(){
  joke_html_files <- get_joke_file_names()
  jokes <- lapply(joke_html_files, function(file_name){
    joke <- readLines(file_name)
    joke <- sapply(joke, str_trim)
    joke <- joke[-c(1:which(joke == "<!--begin of joke -->"), which(joke == "<!--end of joke -->"):length(joke))]
    joke <- paste(joke, collapse = " ")
    joke <- tolower(joke)
    joke <- gsub("<.*?>", "", joke)
    joke <- gsub('\"', "", joke)
    
    joke_number <- gsub("data/jokes/init", "", file_name)
    joke_number <- gsub(".html", "", joke_number)
    data.frame(joke_number = as.integer(joke_number), joke = joke)
  }) %>%
    do.call("rbind", .)
  
  return(jokes)
}