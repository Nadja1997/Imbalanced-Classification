library(dbscan)
library(cluster)

d_eucl <- function(x,y){sqrt(sum((x-y)^2))}

## DownSanple 

# Downsampling with clustering
dbscan_clust <- function(X, y, eps, minPt, minor_num = 2){
  
  dtypes <- sapply(names(X), function(x) class(X[,x]))
  X_num <- X[, !names(X_train) %in%
                     names(X)[dtypes == "factor"]]
  dbs_cluster <- dbscan(X_num, eps, MinPts=minPt) 
  # -1 for noise points
  clusters <- dbs_cluster$cluster
  clusters_unique <- unique(dbs_cluster$cluster)
  cluster_num <- length(clusters_unique) - 1
  
  df <- data.frame(X,Class = y)
  result <- data.frame()
  pos <- sum(y==levels(y)[minor_num])
  ratio_i <- c()
  
  for(i in 1:cluster_num){
    df_tmp = df[which(clusters==i),]
    y_i <- df_tmp$Class
    pos_i <- sum(y_i==levels(y_i)[minor_num])
    neg_i <- sum(y_i!=levels(y_i)[minor_num])
    if(pos_i!=0){
      ratio_i <- c(ratio_i, neg_i/pos_i)
    }else{
      # pos_i = 1
      ratio_i <- c(ratio_i, neg_i)
    }
  }
  
  size_i <- floor(pos*ratio_i/sum(ratio_i))
  
  for(i in 1:cluster_num){
    df_tmp = df[which(clusters==i), ]
    y_i <- df_tmp$Class
    negative_i <- rownames(df_tmp[which(y_i!=levels(y_i)[minor_num]),])
    negative_sample <- sample(negative_i, min(size_i[i], length(negative_i)))
    result <- rbind(result, df_tmp[negative_sample,])
  }
  
  result_fin <- rbind(result, df[which(y==levels(y)[minor_num]),])
  result_fin
  
}


dbscan_multi <- function(X, y, eps, minPt){
  
  df <- data.frame(X,Class = y)
  
  minor <- which.min(table(y))
  result <- df[y == levels(y)[minor], ]
  
  for(i in 1:length(eps)){
    e = eps[i]
    X_tmp <- X[y %in% c(names(e), levels(y)[minor]),] 
    y_tmp <- y[y %in% c(names(e), levels(y)[minor])] 
    y_tmp <- droplevels(y_tmp)
    result_tmp <- dbscan_clust(X_tmp, y_tmp, eps = 5, 
                               minPt = dim(X_tmp)[2], 
                               minor_num = which.min(table(y_tmp)))
    result <- rbind(result, result_tmp[result_tmp$Class == names(e),])
  }
  result
  
}

## UpSample 

# SMOTE in multiclass
SMOTE_multi <- function(X, y){
  
  df <- data.frame(X,Class = y)
  
  major <- which.max(table(y))
  result <- df[y == levels(y)[major], ]
  other_cats <- levels(y)[levels(y) != names(major)]
  
  for(i in 1:length(other_cats)){
    X_tmp <- X[y %in% c(other_cats[i], levels(y)[major]),] 
    y_tmp <- y[y %in% c(other_cats[i], levels(y)[major])] 
    y_tmp <- droplevels(y_tmp)
    
    imb_ratio <- floor(table(y_tmp)[which.max(table(y_tmp))]/
                         table(y_tmp)[which.min(table(y_tmp))])
    
    result_tmp <- SMOTE(X_tmp, y_tmp, K = imb_ratio, 
                        dup_size = imb_ratio - 1)$data
    names(result_tmp)[names(result_tmp) == 'class'] <- 'Class'
    result_tmp$Class <- factor(result_tmp$Class)
    
    result <- rbind(result, result_tmp[result_tmp$Class == other_cats[i],])
  }
  result
  
}


# Oversampling with clustering
clust_oversample <- function(X, y, eps_min, eps_maj, 
                             minPt, minor_num = 2){
  
  dtypes <- sapply(names(X), function(x) class(X[,x]))
  X_num <- X[, !names(X_train) %in%
               names(X)[dtypes == "factor"]]
  
  df <- data.frame(X, Class = y)
  S_minor <- df[which(y==levels(y)[minor_num]), ]
  
  X_minor <- X_num[rownames(S_minor), ]
  S_major <- df[which(y!=levels(y)[minor_num]), ]
  X_major <- X_num[rownames(S_major), ]
  
  dbsc_min <- dbscan(X_minor, eps_min, MinPts=minPt)
  clus_min <- dbsc_min$cluster
  clus_min_unique <- unique(dbsc_min$cluster)
  clus_min_num <- length(clus_min_unique) - 1
  
  dbsc_maj <- dbscan(X_major, eps_maj, MinPts=minPt)
  clus_maj <- dbsc_maj$cluster
  clus_maj_unique <- unique(dbsc_maj$cluster)
  clus_maj_num <- length(clus_maj_unique) - 1
  
  sizes <- c()
  for(i in 1:clus_min_num){
    sizes <- c(sizes, sum(clus_min == i))
  }
  for(i in 1:clus_maj_num){
    sizes <- c(sizes, sum(clus_maj == i))
  }
  size_max <- max(sizes)
  
  result <- data.frame()
  for(i in 1:clus_min_num){
    df_tmp <- S_minor[which(clus_min==i), ]
    ind <- rownames(df_tmp)
    ind_sampled <- sample(ind, size_max, replace = TRUE)
    result <- rbind(result, df_tmp[ind_sampled, ])
  }
  for(i in 1:clus_maj_num){
    df_tmp <- S_major[which(clus_maj==i), ]
    ind <- rownames(df_tmp)
    ind_sampled <- sample(ind, size_max, replace = TRUE)
    result <- rbind(result, df_tmp[ind_sampled, ])
  }
  
  result
  
}

cluster_overs_multi <- function(X, y, eps, minPt){
  
  df <- data.frame(X, Class = y)
  cats <- levels(y)
  clusters <- list()
  cats_data <- list()
  sizes <- c()
  
  for(i in 1:length(cats)){
    
    e = eps[i]
    cats_data[[i]] <- df[y == names(e), ]
    dbsc <- dbscan(X[y == names(e), ], e, 
                   MinPts = min(nrow(cats_data[[i]]), minPt))
    
    clus <- dbsc$cluster
    clus_unique <- unique(clus)
    clus_num <- length(unique(clus[clus>0])) 
    clusters[[i]] <- clus
      
    for(j in 1:clus_num){
      sizes <- c(sizes, sum(clus == j))
    }
  }
  
  size_max <- max(sizes)
  result <- data.frame()
  
  for(i in 1:length(cats)){
    
    clus <- clusters[[i]]
    clus_num <- length(unique(clus[clus>0])) 
    for(j in 1:clus_num){
      
      df_tmp <- cats_data[[i]][which(clus==j), ]
      ind <- rownames(df_tmp)
      ind_sampled <- sample(ind, size_max, replace = TRUE)
      result <- rbind(result, df_tmp[ind_sampled, ])
    }
  }
  result
  
}
