tempDir <- tempdir()

display_joke_and_get_rating <- function(joke_file_name){
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "test.html")
  writeLines(readLines(joke_file_name), htmlFile)
  viewer <- getOption("viewer")
  viewer(htmlFile)
  
  rating <- NA
  
  while(is.na(rating) | rating < -10 | rating > 10){
    rating <- readline("How Would You Rate This Joke [-10:10] (or 'exit'):")
    if(rating == 'exit'){
      break
    }
    
    rating <- as.integer(rating)
  }
  
  return(rating)
}

run_demo <- function(ratings, joke_tif_idf, matrix_fit){
  joke_file_names <- get_joke_file_names()
  init_jokes <- c(5, 7, 8, 13, 15, 16, 17, 18, 19, 20) # These are the jokes the dataset has in highest density
  exit <- F
  
  user_ratings <- matrix(numeric(), nrow = 1, ncol = length(joke_file_names)) # Init empty user ratings
  
  print("Welcome to the Joke Recommendation Service")
  print("To Start Lets Rate 10 Initial Jokes")
  print("Jokes will show in the RStudio Viewer")
  print("If you type 'exit' you will stop rating the initial set and move onto the recommendations")
  
  for(init_joke in init_jokes){ # Get Ratings to initial jokes
    rating <- display_joke_and_get_rating(joke_file_names[init_joke])
    
    if(rating == 'exit'){
      exit <- T
      break
    }
    
    user_ratings[1,init_joke] <- rating
  }
  
  print("Prepping Personalization")
  
  predictions_matrix_fac <- predict_knn_ratings(matrix_fit, user_ratings, k = 5)
  predictions_tfidf_rocchio <- predict_knn_tfidf_rocchio(ratings, user_ratings, joke_tif_idf, k = 1)
  predictions_knn <- predict_knn_ratings(ratings, user_ratings, k = 6)

  predictions <- data.frame(joke_number = 1:length(user_ratings),
                            predictions_matrix_fac = as.numeric(predictions_matrix_fac),
                            predictions_tfidf_rocchio = as.numeric(predictions_tfidf_rocchio),
                            predictions_knn = as.numeric(predictions_knn))
  
  ordered_unseen_jokes_by_prediction <- predictions %>%
    filter(joke_number %in% which(is.na(user_ratings))) %>%
    arrange(predictions_matrix_fac)
  
  ######### Demo best jokes based on initial joke ratings via different mechanisms
  
  print("Now Lets See Your Favorite Joke Based On Matrix Factorization")
  rating <- display_joke_and_get_rating(joke_file_names[ordered_unseen_jokes_by_prediction$joke_number[[which(ordered_unseen_jokes_by_prediction$predictions_matrix_fac == max(ordered_unseen_jokes_by_prediction$predictions_matrix_fac))]]])
  print(paste("Expected Rating =", ordered_unseen_jokes_by_prediction$predictions_matrix_fac[[which(ordered_unseen_jokes_by_prediction$predictions_matrix_fac == max(ordered_unseen_jokes_by_prediction$predictions_matrix_fac))]]))
  
  print("Now Lets See Your Favorite Joke Based On tfidf Rocchio")
  rating <- display_joke_and_get_rating(joke_file_names[ordered_unseen_jokes_by_prediction$joke_number[[which(ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio == max(ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio))]]])
  print(paste("Expected Rating =", ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio[[which(ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio == max(ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio))]]))
  
  print("Now Lets See Your Favorite Joke Based On KNN")
  rating <- display_joke_and_get_rating(joke_file_names[ordered_unseen_jokes_by_prediction$joke_number[[which(ordered_unseen_jokes_by_prediction$predictions_knn == max(ordered_unseen_jokes_by_prediction$predictions_knn))]]])
  print(paste("Expected Rating =", ordered_unseen_jokes_by_prediction$predictions_knn[[which(ordered_unseen_jokes_by_prediction$predictions_knn == max(ordered_unseen_jokes_by_prediction$predictions_knn))]]))
  
  ############# Demo worst jokes based on initial joke ratings via different mechanisms
  
  print("Now Lets See Your Least Favorite Joke Based On Matrix Factorization")
  rating <- display_joke_and_get_rating(joke_file_names[ordered_unseen_jokes_by_prediction$joke_number[[which(ordered_unseen_jokes_by_prediction$predictions_matrix_fac == min(ordered_unseen_jokes_by_prediction$predictions_matrix_fac))]]])
  print(paste("Expected Rating =", ordered_unseen_jokes_by_prediction$predictions_matrix_fac[[which(ordered_unseen_jokes_by_prediction$predictions_matrix_fac == min(ordered_unseen_jokes_by_prediction$predictions_matrix_fac))]]))
  
  print("Now Lets See Your Least Favorite Joke Based On tfidf Rocchio")
  rating <- display_joke_and_get_rating(joke_file_names[ordered_unseen_jokes_by_prediction$joke_number[[which(ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio == min(ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio))]]])
  print(paste("Expected Rating =", ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio[[which(ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio == min(ordered_unseen_jokes_by_prediction$predictions_tfidf_rocchio))]]))
  
  print("Now Lets See Your Least Favorite Joke Based On KNN")
  rating <- display_joke_and_get_rating(joke_file_names[ordered_unseen_jokes_by_prediction$joke_number[[which(ordered_unseen_jokes_by_prediction$predictions_knn == min(ordered_unseen_jokes_by_prediction$predictions_knn))]]])
  print(paste("Expected Rating =", ordered_unseen_jokes_by_prediction$predictions_knn[[which(ordered_unseen_jokes_by_prediction$predictions_knn == min(ordered_unseen_jokes_by_prediction$predictions_knn))]]))
}