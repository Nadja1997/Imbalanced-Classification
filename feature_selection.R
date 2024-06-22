library(corrplot)
library(infotheo)
library(pdfCluster)
library(RWeka)
library(openxlsx)
library(caret)
library(mlbench)
library(dbscan)


# Fisher score
f_score <- function(X, y, cluster=FALSE, minor_num = 2){
  
  cols <- colnames(X)
  scores <- data.frame(matrix(ncol = 2, nrow = length(cols)))
  colnames(scores) <- c("column", "score")
  if(cluster){
    cl <-pdfCluster(X[which(!(y %in% levels(y)[minor_num])), ],
                    bwtype="adaptive")
    levels(y) = c(levels(y), cl@clusters)
    y[!(y %in% levels(y)[minor_num])] = cl@clusters
    y <- droplevels(y)
  }
  cat_size <- length(levels(y))
  
  for (i in 1:length(cols)){
    col <- X[cols[i]][[1]]
    s_between <- 0
    s_within <- 0
    mean_total <- mean(col)
    
    for (j in 1:cat_size){
      n_k <- length(y[y %in% levels(y)[j]])
      col_tmp <- col[which(y %in% levels(y)[j])]
      mean_cat <- mean(col_tmp)
      
      s_between <- s_between + n_k*(mean_cat - mean_total)^2
      s_within <- s_within + sum((col_tmp - mean_cat)^2)
    }
    
    scores[i, "column"] <- cols[i]
    scores[i, "score"] <- s_between / s_within
  }
  scores <- scores[order(scores$score, decreasing = TRUE),]
  if(cluster){return(list(scores, cl@clusters))}
  else{return(scores)}
  
}


# Mutual information
multiinf <- function(X, y){
  
  df <- data.frame(X,y)
  X_disc <- Discretize(y ~., data = df)
  X_disc <- X_disc[,!names(X_disc) %in% c("y")]
  
  cols <- colnames(X)
  mu_inf <- data.frame(matrix(ncol = 2, nrow = length(cols)))
  colnames(mu_inf) <- c("column", "mu_inf")
  
  for (i in 1:length(cols)){
    mu_inf[i, "column"] <- cols[i]
    mu_inf[i, "mu_inf"] <- mutinformation(X_disc[i], y)
  }
  mu_inf <- mu_inf[order(mu_inf$mu_inf, decreasing = TRUE),]
  mu_inf
  
}


# Helinger distance
h_dist <- function(X, y){
  
  cols <- colnames(X)
  distances <- data.frame(matrix(ncol = 2, nrow = length(cols)))
  colnames(distances) <- c("column", "dist")
  f_p <- sum(y == "positive")
  f_n <- sum(y == "negative")
  
  for (i in 1:length(cols)){
    col <- X[cols[i]][[1]]
    cat_size <- length(levels(col))
    sum <- 0
    
    for (j in 1:cat_size){
      y_tmp <- y[which(col == levels(col)[j])]
      f_p_cat <- sum(y_tmp == "positive")
      f_n_cat <- sum(y_tmp == "negative")
      
      sum <- sum + (sqrt(f_p_cat/f_p) - sqrt(f_n_cat/f_n))^2
    }
    
    distances[i, "column"] <- cols[i]
    distances[i, "dist"] <- sqrt(sum)
  }
  distances
  
}


# Build Hellinger distance tree
hd_tree <- function(X, y, C, labels=unique(y)) {
  
  if(is.null(labels) || length(labels)==0) labels <- unique(y)  
  
  node <- list() # when called for first time, this will be the root
  node$C <- C
  node$labels <- labels
  
  if(length(unique(y))==1 || length(y) < C) {
    # calculate counts and frequencies
    node$count <- sort(table(y), decreasing=TRUE)
    node$freq  <- node$count/sum(node$count)
    # get the label of this leaf node
    node$label <- names(node$count)[1]
    return(node)
  }
  else { # recursion
    # get Hellinger distance and their max
    HD <- list()
    for(i in 1:ncol(X)){
      
      if(class(X[,i]) == "factor"){
        results_tmp <- data.frame(matrix(nrow = length(levels(X[,i])),
                                         ncol = 3))
        colnames(results_tmp) <- c("value", "hdist", "type")
        
        for(j in 1:length(levels(X[,i]))){
          X_tmp <- data.frame(x = (X[,i] == levels(X[,i])[j]))
          X_tmp$x <- factor(X_tmp$x)
          results_tmp[j, "value"] <- levels(X[,i])[j]
          results_tmp[j, "hdist"] <- h_dist(X_tmp,y)$dist
          results_tmp[j, "type"] <- class(X[,i])
        }
      }
      else{
        results_tmp <- data.frame(matrix(nrow = length(unique(X[,i])),
                                         ncol = 3))
        colnames(results_tmp) <- c("value", "hdist", "type")
        
        for(j in 1:length(unique(X[,i]))){
          X_tmp <- data.frame(x = (X[,i] <= unique(X[,i])[j]))
          X_tmp$x <- factor(X_tmp$x)
          results_tmp[j, "value"] <- unique(X[,i])[j]
          results_tmp[j, "hdist"] <- h_dist(X_tmp,y)$dist
          results_tmp[j, "type"] <- class(X[,i])
        }
      }
    
      HD[[i]] <- results_tmp[which.max(results_tmp$hdist)[1],]    
    }
    
    hd <- sapply(HD, function(x) {return(x$hdist)})
    i  <- which(hd==max(hd))[1] # just taking the first 
    
    # save node attributes
    node$i    <- i
    node$v    <- HD[[i]]$value
    node$type <- HD[[i]]$type
    node$d    <- HD[[i]]$hdist
    
    if(node$type=="factor") {
      j <- X[,i]==node$v
      node$childLeft  <- hd_tree(X[j,], y[j], C, labels)
      node$childRight <- hd_tree(X[!j,], y[!j], C, labels)
    }
    else{
      j <- X[,i]<=node$v
      node$childLeft  <- hd_tree(X[j,], y[j], C, labels)
      node$childRight <- hd_tree(X[!j,], y[!j], C, labels)      
    }
  }
  
  return(node) # returns root node
  
}


# Predict by hd_tree
hddt_predict <- function(root, X) {
  
  y <- rep(NA, nrow(X))
  for(i in 1:nrow(X)) {
    # traverse the tree until we find a leaf node
    node <- root
    while(!is.null(node$v)) {
      if(node$type=="factor") {
        if(X[i,node$i]==node$v) node <- node$childLeft
        else node <- node$childRight
      }
      else{
        if(X[i,node$i]<=node$v) node <- node$childLeft
        else node <- node$childRight
      }
    }

    stopifnot(!is.null(node$label))
    y[i] <- node$label
  }
  
  return(y)
  
}


## Feature selection per each dataset

## Default of credit cards clients
UCI_Credit_Card <- read.xlsx("UCI_Credit_Card.xlsx")
UCI_Credit_Card["Class"] = UCI_Credit_Card$default.payment.next.month
df <- UCI_Credit_Card[, !names(UCI_Credit_Card) %in% 
                        c("default.payment.next.month", "ID")]
df<-as.data.frame(df)

# Dealing with categorical attributes
df <- df[df$EDUCATION !="0",]
df[df$EDUCATION %in% c("5", "6"), "EDUCATION"] = "4"
df <- df[df$MARRIAGE !="0",]

df$SEX <- as.factor(df$SEX)
df$EDUCATION <- as.factor(df$EDUCATION)
df$MARRIAGE <- as.factor(df$MARRIAGE)
df$Class <- as.factor(df$Class)

# Class Imbalance cca 3:1
table(df$Class)
levels(df$Class) = c("negative", "positive")
minor_num <- 2

# Train/test split
y = df$Class
trainIndex <- createDataPartition(y, p = .8, 
                                  list = FALSE, 
                                  times = 1)
df_train <- df[trainIndex,]
df_train$Class = as.factor(df_train$Class)
X_train = df_train[, !names(df_train) %in% c("Class")]
y_train = df_train[, "Class"]

df_test <- df[-trainIndex,]
y_test = df_test[, "Class"]
X_test = df_test[, !names(df_test) %in% c("Class")]
y_test = df_test[, "Class"]

# Numerical data
X_train_num <- X_train[, !names(X_train) %in%
                         c("SEX", "EDUCATION", "MARRIAGE")]

# high correlation between consecutive PAY_n and between BILL_AMTn
correlations <- cor(X_train_num)
corrplot(correlations)

correlations <- cor(X_train_num[, !names(X_train_num) %in%
                              c("PAY_2", "PAY_4", "PAY_6",
                                "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", 
                                "BILL_AMT5", "BILL_AMT6")])
corrplot(correlations)


# Logistic regression model excluding above variables
df_train <- df_train[, !names(df_train) %in%
                       c("PAY_2", "PAY_4", "PAY_6",
                         "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", 
                         "BILL_AMT5", "BILL_AMT6")]
X_train <- X_train[, !names(X_train) %in%
                     c("PAY_2", "PAY_4", "PAY_6",
                       "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", 
                       "BILL_AMT5", "BILL_AMT6")]
X_test <- X_test[, !names(X_test) %in%
                     c("PAY_2", "PAY_4", "PAY_6",
                       "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", 
                       "BILL_AMT5", "BILL_AMT6")]
X_train_num <- X_train[, !names(X_train) %in%
                         c("SEX", "EDUCATION", "MARRIAGE")]
X_test_num <- X_test[, !names(X_test) %in%
                       c("SEX", "EDUCATION", "MARRIAGE")]

# Scale train and test data
X_train_num_scaled <- as.data.frame(scale(X_train_num))
X_test_num_scaled <- as.data.frame(scale(X_test_num, 
                                         center=attr(scale(X_train_num), 
                                                     "scaled:center"),
                                         scale=attr(scale(X_train_num), 
                                                    "scaled:scale")))                                    

X_train <- cbind(X_train_num_scaled, 
                 X_train[, c("SEX", "EDUCATION", "MARRIAGE")])
X_test <- cbind(X_test_num_scaled, 
                 X_test[, c("SEX", "EDUCATION", "MARRIAGE")])

X_train_num <- X_train[, !names(X_train) %in%
                         c("SEX", "EDUCATION", "MARRIAGE")]
X_test_num <- X_test[, !names(X_test) %in%
                       c("SEX", "EDUCATION", "MARRIAGE")]

# Determine predictors for logistic regression
model_lr <- glm(formula = y_train ~ . , 
             family = binomial(link = "logit"), 
             data = X_train)

# high p-value of Wald test for 
# PAY_AMT3, PAY_AMT4, PAY_AMT5 and PAY_AMT6
summary(model_lr)

# reduced model without the above variables
model_red <- glm(formula = y_train ~ .  - PAY_AMT3 
                 - PAY_AMT4 -PAY_AMT5 - PAY_AMT6 , 
                 family = binomial(link = "logit"), 
                 data = X_train)

# likelihood ratio test, large p-value indicates we
# should keep reduced model 
anova(model_red, model_lr, test = "Chisq")

LR_summ <- summary(model_red)
LR_vars_default <- c("LIMIT_BAL" , "SEX", "EDUCATION", "MARRIAGE",
                     "PAY_0", "PAY_3", "PAY_5", "BILL_AMT1", 
                     "PAY_AMT1", "PAY_AMT2" )

# Fisher score
f_score(X_train_num, y_train)

# Mutual information
multiinf(X_train, y_train)

# Hellinger distance

df_disc <- Discretize(Class ~., data = df_train)
X_disc <- df_disc[,!names(df_disc) %in% c("Class")]
h_distances<- h_dist(X_disc, y_train)
h_distances[order(h_distances$dist, decreasing = TRUE),]

# All three methods rank PAY_0, PAY_3, PAY_5, LIMIT_BAL, PAY_AMT1
# and PAY_AMT2 as top variables, we will build more complex models
# using them 
predictors_default = c("PAY_0", "PAY_3", "PAY_5", "LIMIT_BAL", "PAY_AMT1",
                       "PAY_AMT2")

# Determine eps for DBSCAN (k = minPts-1; minPts = 2*dim(X_train))
kNNdistplot(X_train_num, k = 2*dim(X_train_num)[2] - 1, minPts = 2*dim(X_train_num)[2])
abline(h=4)
eps_def <- 4

kNNdistplot(X_train_num[y_train == "positive",], k = 2*dim(X_train_num)[2] - 1,
            minPts = 2*dim(X_train_num)[2])
abline(h=3)
eps_min_def <- 3
kNNdistplot(X_train_num[y_train == "negative",], k = 2*dim(X_train_num)[2] - 1,
            minPts = 2*dim(X_train_num)[2])
abline(h=4)
eps_maj_def <- 4

# Save the data
default_df <- df_train
default_train <- X_train
default_y_train <- y_train
default_test <- X_test
default_y_test <- y_test
default_train_num <- X_train_num


## Glass type dataset
data(Glass)
df<- Glass

# Classes 3, 5, 6 and 7 are minor, 6 is the smallest one
table(df$Type)
minor_nums <- c(3, 4, 5, 6)

# Rename target to Class to be consistent
names(df) <- c(names(df)[-ncol(df)], "Class")

y = df$Class
trainIndex <- createDataPartition(y, p = .8, 
                                  list = FALSE, 
                                  times = 1)
df_train <- df[trainIndex,]
df_train$Class = as.factor(df_train$Class)
X_train = df_train[, !names(df_train) %in% c("Class")]
y_train = df_train[, "Class"]

df_test <- df[-trainIndex,]
y_test = df_test[, "Class"]
X_test = df_test[, !names(df_test) %in% c("Class")]
y_test = df_test[, "Class"]

# Moderate correlation between RI and Ca
correlations <- cor(X_train)
corrplot(correlations)

# Scale train and test data
X_train <- as.data.frame(scale(X_train))
X_test <- as.data.frame(scale(X_test, 
                              center=attr(scale(X_train), "scaled:center"),
                              scale=attr(scale(X_train), "scaled:scale")))                                    

# Mutual information
multiinf(X_train, y_train)

# Fisher score
f_score(X_train, y_train)

# Mg and Al most significant per both criteria, other variables'
# scores differ. We could try using all of them in our models.
predictors_glass = names(X_train)

# Determine eps for DBSCAN (k = minPts-1; minPts = 2*dim(X_train))
# 1. eps for downsampling with clustering
kNNdistplot(X_train[y_train %in% c("1", "6"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=2)
eps_glass16 <- 2

kNNdistplot(X_train[y_train %in% c("2", "6"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=3)
eps_glass26 <- 3

kNNdistplot(X_train[y_train %in% c("3", "6"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=3)
eps_glass36 <- 3

kNNdistplot(X_train[y_train %in% c("5", "6"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=4)
eps_glass56 <- 4

kNNdistplot(X_train[y_train %in% c("7", "6"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=2)
eps_glass76 <- 2

eps_glass_down <-c(eps_glass16, eps_glass26, eps_glass36, eps_glass56, eps_glass76)
names(eps_glass_down) <- c("1", "2", "3", "5", "7")

# 2. eps for oversampling with clustering
kNNdistplot(X_train[y_train %in% c("1"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=1.5)
eps_glass1 <- 1.5

kNNdistplot(X_train[y_train %in% c("2"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=2)
eps_glass2 <- 2

kNNdistplot(X_train[y_train %in% c("3"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=2)
eps_glass3 <- 2

kNNdistplot(X_train[y_train %in% c("5"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=6)
eps_glass5 <- 6

kNNdistplot(X_train[y_train %in% c("6"), ], 
            k = 7, minPts = dim(X_train)[2])
abline(h=6)
eps_glass6 <- 6

kNNdistplot(X_train[y_train %in% c("7"), ], 
            k = dim(X_train)[2] - 1, minPts = dim(X_train)[2])
abline(h=2.5)
eps_glass7 <- 2.5

eps_glass_up <- c(eps_glass1, eps_glass2, eps_glass3, eps_glass5, eps_glass6, eps_glass7)
names(eps_glass_up) <- c("1", "2", "3", "5", "6", "7")

# Save the data
glass_df <- df_train
glass_train <- X_train
glass_y_train <- y_train
glass_test <- X_test
glass_y_test <- y_test
glass_train_num <- X_train

