library(kknn)

# Weighted logistic regression
logistic_weight <- function(X_train, y_train, minor_num, LR_vars){
  
  df_train <- data.frame(X_train, y_train)
  N <- dim(df_train)[1]
  counts <- table(y_train)
  cat_minor <- levels(y_train)[minor_num]
  N_minor <- counts[names(counts) == cat_minor]
  N_major <- counts[names(counts) != cat_minor]
  weights <- sapply(y_train, function(x) 
    ifelse(x==cat_minor, N/(2*N_minor), N/(2*N_major)))
  
  model_log <- glm(formula = y_train ~ ., 
                   family = binomial(link = "logit"),
                   data = df_train[, LR_vars], weights = weights)
  model_log
  
}


# kNN weighted voting
knn_voting <- function(X_train, y_train, X_test, y_test, k){
  
  df_train <- data.frame(X_train, y_train)
  df_test <- data.frame(X_test, y_test)
  cats <- levels(y_test)
  N_cats <- length(cats)
  N_test <- dim(df_test)[1]
  N_col <- dim(df_test)[2]
  
  wknn <- kknn(y_train ~., train = df_train, 
               test = X_test, k = k)
  probs <- wknn$prob
  

  max_cat <- apply(probs, 1, which.max)
  
  df_test["predict"] <- cats[max_cat]
  df_test
  
}
