library(NNLM)
library(parallel)
library(ggplot2)

knn_predict <- function(test, train, sim_matrix, k = 6){
  train_means <- rowMeans(train, na.rm = T) # Used to make mean correction between test example and training example
  
  predictions <- mclapply(1:nrow(test), function(row_number){
    test_example <- as.matrix(test[row_number,])
    sim <- sim_matrix[row_number,] 
    
    na_to_fill <- which(is.na(test_example)) # We only want to fill in these values based on knn
    example_mean <- mean(test_example, na.rm = T) # Used to correct for mean diff between train and test
    
    sapply(1:length(test_example), function(colnum){
      if(colnum %in% na_to_fill){ 
        neighbors <- head(order(-sim * ifelse(is.na(train[[colnum]]), NA, 1)), k) # Find k most similar users that have rated the unrated test joke
        sum((train[neighbors, colnum] + (example_mean - train_means[neighbors])) * sim[neighbors]) / sum(sim[neighbors]) # Predict the unrated joke will recieve the mean corrected cosine similarity weighted vote
      } else {
        test_example[[colnum]] # Jokes that were already weighted should maintain the original weight
      }
    })
  }, mc.cores = 8, mc.allow.recursive = F) %>%
    do.call("rbind", .)
  
  return(predictions)
}

compute_cos_sim <- function(train_clean, test_clean){
  test_to_train_dot_product <- as.matrix(test_clean) %*% t(as.matrix(train_clean))
  test_sq_sums <- rowSums(test_clean^2)
  train_sq_sums <- rowSums(train_clean^2)
  sqrt_sum_sq <- sqrt(test_sq_sums %o% train_sq_sums) 
  
  cos_sim <- test_to_train_dot_product / sqrt_sum_sq
  
  return(cos_sim)
}

predict_knn_ratings <- function(train, test, k = 6){
  train_clean <- train
  train_clean[is.na(train_clean)] <- 0 # Fill with zero since it won't impact cos similarity
  
  test_clean <- test
  test_clean[is.na(test_clean)] <- 0 # Fill with zero since it won't impact cos similarity
  
  cos_sim <- compute_cos_sim(train_clean, test_clean)
  
  predictions <- knn_predict(test, train, cos_sim, k)
  
  return(predictions)
}

train_test_knn_ratings <- function(ratings, k = 6){
  train_id <- sample(1:nrow(ratings), nrow(ratings) * .998)
  train <- head(ratings[train_id,], 10000)
  validation <- ratings[-train_id,]
  test <- make_random_nas(validation)
  
  predictions <- predict_knn_ratings(train, test, k)
  
  unknown_values <- validation
  unknown_values[!is.na(test)] <- NA
  unknown_diff <- mean(as.matrix(abs(predictions - unknown_values)), na.rm = T)
  
  return(unknown_diff)
}

find_best_k_for_knn_ratings <- function(ratings){
  result <- sapply(1:10, function(k){
    err <- train_test_knn_ratings(ratings, k = k)
    print(paste("K =", k, 'Error =', err))
    return(err)
  })
  
  plot(result,
       main = "KNN Error By K Over Ratings",
       ylab = "MAE",
       xlab = "K")
  
  return(which(result == min(result)))
}

predict_knn_tfidf_rocchio <- function(train, test, joke_tif_idf, k = 1){
  train_clean <- train
  train_clean[is.na(train_clean)] <- 0 # Fill with zero since it won't impact cos similarity
  train_clean[train_clean < 0] <- train_clean[train_clean < 0] * .25 # rocchio negative weights are not full weight
  
  test_clean <- test
  test_clean[is.na(test_clean)] <- 0 # Fill with zero since it won't impact cos similarity
  test_clean[test_clean < 0] <- test_clean[test_clean < 0] * .25 # rocchio negative weights are not full weight
  
  rocchio_user_term_scores_train <- as.matrix(train_clean) %*% joke_tif_idf
  rocchio_user_term_scores_test <- as.matrix(test_clean) %*% joke_tif_idf
  
  cos_sim <- compute_cos_sim(rocchio_user_term_scores_train, rocchio_user_term_scores_test)
  
  predictions <- knn_predict(test, train, cos_sim, k)
  
  return(predictions)
}

train_test_knn_tfidf_rocchio <- function(ratings, joke_tif_idf, k = 1){
  train_id <- sample(1:nrow(ratings), nrow(ratings) * .998)
  train <- head(ratings[train_id,], 10000)
  validation <- ratings[-train_id,]
  test <- make_random_nas(validation)
  
  predictions <- predict_knn_tfidf_rocchio(test, train, joke_tif_idf, k)
  
  unknown_values <- validation
  unknown_values[!is.na(test)] <- NA
  unknown_diff <- mean(as.matrix(abs(predictions - unknown_values)), na.rm = T)
  
  return(unknown_diff)
}

find_best_k_for_knn_tfidf_rocchio <- function(ratings){
  result <- sapply(1:10, function(k){
    err <- train_test_knn_tfidf_rocchio(ratings, joke_tif_idf, k = k)
    print(paste("K =", k, 'Error =', err))
    return(err)
  })
  
  plot(result,
       main = "KNN Error By K Over tf*idf Rocchio Ratings",
       ylab = "MAE",
       xlab = "K")
  
  return(which(result == min(result)))
}

fit_matrix_factorize <- function(train, rank = 5, alpha = c(0,0,1e-03), beta = c(0,0,1e-03)) {
  shift <- 10
  train_shift <- train + shift # Shift so we can perform "non negative" matrix factorization
  model <- nnmf(as.matrix(train_shift), rank, 
                alpha = alpha,
                beta = beta,
                max.iter = 2000, inner.max.iter = 100, 
                check.k = FALSE, n.thread = 8)
  
  return(predict(model) - shift)
}

train_test_matrix_factorize <- function(ratings, rank = 5, k = 5, alpha = c(0,0,1e-03), beta = c(0,0,1e-03)){
  train_id <- sample(1:nrow(ratings), nrow(ratings) * .998)
  train <- head(ratings[train_id,], 10000)
  validation <- ratings[-train_id,]
  test <- make_random_nas(validation)

  matrix_fit <- fit_matrix_factorize(train, rank = rank, alpha = c(0,0,1e-03), beta = c(0,0,1e-03))
  predictions <- predict_knn_ratings(matrix_fit, test, k = k)
  
  unknown_values <- validation
  unknown_values[!is.na(test)] <- NA
  unknown_diff <- mean(as.matrix(abs(predictions - unknown_values)), na.rm = T)
  
  return(unknown_diff)
}

find_best_k_for_matrix_factorization <- function(ratings){
  result <- mclapply(1:8, function(k){
    trial_result <- train_test_matrix_factorize(ratings, rank = 5, k = k)
    print(paste("K=", k, "MAE=", trial_result))
    data.frame(unknown_mae = trial_result,
               k = k)
  }, mc.cores = 8) %>%
    do.call("rbind", .)
  
  plot(result$k, result$unknown_mae)
  
  return(result$k[result$unknown_mae == min(result$unknown_mae)])
}

find_best_regularization_for_matrix_factorization <- function(ratings){
  result <- mclapply(c(0, .00001, .0001, .001), function(l1reg){
      mclapply(c(0, .00001, .0001, .001), function(l2reg){
        trial_result <- train_test_matrix_factorize(ratings, rank = 5, k = 3,
                                                    alpha = c(l2reg, 0, l1reg),
                                                    beta = c(l2reg, 0, l1reg))
        print(paste("l1=", l1reg, "l2=", l2reg, "MAE=", trial_result))
        data.frame(unknown_mae = trial_result,
                   l1 = l1reg,
                   l2 = l2reg)
      }, mc.cores = 8) %>%
        do.call("rbind", .)
    }, mc.cores = 8) %>%
      do.call("rbind", .)
  
  result_summary_l1 <- result %>%
    group_by(l1) %>%
    summarize(unknown_mae_std = sd(unknown_mae),
              unknown_mae_max = max(unknown_mae),
              unknown_mae_min = min(unknown_mae),
              unknown_mae_90_percentile = quantile(unknown_mae, .9),
              unknown_mae_10_percentile = quantile(unknown_mae, .1),
              unknown_mae_mean = mean(unknown_mae))
  
  result_summary_l2 <- result %>%
    group_by(l2) %>%
    summarize(unknown_mae_std = sd(unknown_mae),
              unknown_mae_max = max(unknown_mae),
              unknown_mae_min = min(unknown_mae),
              unknown_mae_90_percentile = quantile(unknown_mae, .9),
              unknown_mae_10_percentile = quantile(unknown_mae, .1),
              unknown_mae_mean = mean(unknown_mae))
  
  plot(result_summary_l1$l1, result_summary_l1$unknown_mae_mean)
  plot(result_summary_l1$l1, result_summary_l1$unknown_mae_90_percentile)
  plot(result_summary_l1$l1, result_summary_l1$unknown_mae_10_percentile)
  plot(result_summary_l1$l1, result_summary_l1$unknown_mae_std)
  
  plot(result_summary_l2$l2, result_summary_l2$unknown_mae_mean)
  plot(result_summary_l2$l2, result_summary_l2$unknown_mae_90_percentile)
  plot(result_summary_l2$l2, result_summary_l2$unknown_mae_10_percentile)
  plot(result_summary_l2$l2, result_summary_l2$unknown_mae_std)
  
  return(result[result$unknown_mae == min(result$unknown_mae),])
}

compute_errors_by_model <- function(ratings){
  mat_fac_mae <- mclapply(1:8,function(x){
    train_test_matrix_factorize(ratings)
  }, mc.cores = 8)
  
  tfidf_rocchio_mae <- mclapply(1:8,function(x){
    train_test_knn_tfidf_rocchio(ratings, joke_tif_idf)
  }, mc.cores = 8)
  
  knn_mae <- mclapply(1:8,function(x){
    train_test_knn_ratings(ratings)
  }, mc.cores = 8)
    
  results <- data.frame(model = ordered(c("Matrix Factorization", "KNN", "tfidf Rocchio"), levels = c("Matrix Factorization", "KNN", "tfidf Rocchio")),
             mean = c(mean(unlist(mat_fac_mae)), mean(unlist(knn_mae)), mean(unlist(tfidf_rocchio_mae))),
             max = c(max(unlist(mat_fac_mae)), max(unlist(knn_mae)), max(unlist(tfidf_rocchio_mae))),
             min = c(min(unlist(mat_fac_mae)), min(unlist(knn_mae)), min(unlist(tfidf_rocchio_mae))))
   
  err_plot <- ggplot(results, aes(x=model, y=mean, fill = model)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=min, ymax=max),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ylab("MAE") +
    xlab("Model") +
    ggtitle("MAE By Model Type")
  
  ggsave("Model Errors.png", err_plot)
}





