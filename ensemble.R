library(ebmc)

# AdaBoost
ada_boost <- function(X, y, minor_num){
  df <- data.frame(X,y)
  cats <- levels(y)
  df$y <- factor(df$y, 
                 levels = c(cats[cats != cats[minor_num]], cats[cats == cats[minor_num]]),
                 labels = c("0", "1"))
  model_ada <- adam2(y ~ ., data = df, size = 10,
                     alg = "cart")
}


# SmoteBoost
smote_boost <- function(X, y, minor_num){
  df <- data.frame(X,y)
  cats <- levels(y)
  df$y <- factor(df$y, 
                 levels = c(cats[cats != cats[minor_num]], cats[cats == cats[minor_num]]),
                 labels = c("0", "1"))
  model_smb <- sbo(y ~ ., data = df, size = 10,
      alg = "cart", over = 100, smote.k = 5)
}


# RUSBoost
rus_boost <- function(X, y, minor_num, imb_ratio){
  df <- data.frame(X,y)
  cats <- levels(y)
  df$y <- factor(df$y, 
                 levels = c(cats[cats != cats[minor_num]], cats[cats == cats[minor_num]]),
                 labels = c("0", "1"))
  model_rus <- rus(y ~ ., data = df, size = 10,
                   ir = imb_ratio, alg = "cart")
}


# SMOTEBagging
smote_bagg <- function(X, y, minor_num){
  df <- data.frame(X,y)
  cats <- levels(y)
  df$y <- factor(df$y, 
                 levels = c(cats[cats != cats[minor_num]], cats[cats == cats[minor_num]]),
                 labels = c("0", "1"))
  model_sbag <- sbag(y ~ ., data = df, size = 10,
                   alg = "cart", smote.k = 5)
}


# UnderBagging
under_bagg <- function(X, y, minor_num, imb_ratio){
  df <- data.frame(X,y)
  cats <- levels(y)
  df$y <- factor(df$y, 
                 levels = c(cats[cats != cats[minor_num]], cats[cats == cats[minor_num]]),
                 labels = c("0", "1"))
  model_ub <- ub(y ~ ., data = df, size = 10,
                   ir = imb_ratio, alg = "cart")
}


# Random Forest
rf <- function(X, y, minor_num){
  df <- data.frame(X,y)
  cats <- levels(y)
  df$y <- factor(df$y, 
                 levels = c(cats[cats != cats[minor_num]], cats[cats == cats[minor_num]]),
                 labels = c("0", "1"))
  model_rf <- randomForest(y~., data=df)
}


# Balanced Random forest
rf_balanced <- function(X, y, minor_num){
  df <- data.frame(X,y)
  cats <- levels(y)
  df$y <- factor(df$y, 
                 levels = c(cats[cats != cats[minor_num]], cats[cats == cats[minor_num]]),
                 labels = c("0", "1"))
  minor_size = unname(table(y)[cats[minor_num]])
  model_rfb <- randomForest(y~., data=df, strata=df$y,
                           sampsize=c(minor_size,minor_size))
  
}
