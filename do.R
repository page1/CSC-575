source("get.R")
source("munge.R")
source("analyze.R")
source("demo.R")
library(dplyr)

if(!file.exists("model.Rdata")){ #don't waste time doing this if we can avoid it
  ratings <- get_ratings()
  ratings <- select(ratings, -jokes_rated)
  ratings <- munge_na(ratings)
  
  jokes <- get_jokes()
  joke_tif_idf <- make_tf_idf(jokes)

  matrix_fit <- fit_matrix_factorize(ratings)
  save.image(file = "model.Rdata")
} else {
  load("model.Rdata")
}

run_demo(ratings, joke_tif_idf, matrix_fit)
