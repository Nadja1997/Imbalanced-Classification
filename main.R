source("resampling.R")
source("cost_sensitive.R")
source("feature_selection.R")
source("ensemble.R")

library(rpart)
library(pROC)
library(randomForest)
library(e1071)
library(smotefamily)
library(nnet)
library(adabag)


## Evaluation metrics
evaluate_binary <- function(X_test, y_test, model, type, model_name, 
                     df_name, minor_num = 2, Hellinger=FALSE,
                     SVM=FALSE, wkNN = FALSE, wknn_pred = NULL,
                     tr = 0.5){
  
  cats <- levels(y_test)
  
  if(Hellinger){
    y_pred <- as.factor(hddt_predict(h_tree, X_test))
  }
  else if(SVM){
    y_pred <- predict(model, X_test, type = type)
  }
  else if(wkNN){
    y_pred <- factor(wknn_pred)
  }else{
    probs <- predict(model, X_test, type = type)
    if (class(probs)[1] == "matrix") probs = probs[, minor_num]
  
    y_pred <- factor(ifelse(probs > tr, 
                            cats[cats == cats[minor_num]],
                            cats[cats != cats[minor_num]]))
  }
  metrics <- confusionMatrix(y_pred, y_test, positive = cats[minor_num])
  
  if(!Hellinger & !SVM & !wkNN){
    auc_value <- auc(y_test, probs)
  }else{
    auc_value <- NA
  }
  
  metrics_df <- data.frame(Accuracy = metrics$overall["Accuracy"],
                           AUC = auc_value,
                           F1 = metrics$byClass["F1"],
                           Precision = metrics$byClass["Precision"],
                           Sensitivity = metrics$byClass["Sensitivity"],
                           Specificity = metrics$byClass["Specificity"]
                           )
  rownames(metrics_df) = model_name
  
  # ROC curve
  if(!Hellinger & !SVM & !wkNN){
    plot(roc(y_test, probs), legacy.axes = TRUE, main=model_name)
  }
  
  return(metrics_df)
  
}


evaluate_multi <- function(X_test, y_test, model, type, model_name, 
                           df_name, minor_num = 2, Hellinger=FALSE,
                           SVM=FALSE, wkNN = FALSE, wknn_pred = NULL,
                           tr = 0.5){

  cats <- levels(y_test)  
  if(!wkNN){
    y_pred <- predict(model, X_test, type = type)
    if(class(y_pred) == "list"){
      y_pred <- factor(y_pred$class, levels = levels(y_test))
    }
  }else{
    y_pred <- factor(y_pred, levels = levels(y_test))
  }
  metrics <- confusionMatrix(y_pred, y_test)
  eval <- metrics$byClass[, c("Sensitivity", "Precision", "F1")]
  
  metrics_ls <- sapply(1:length(cats), function(i){
    tmp <- data.frame(
      eval[i,]["F1"], 
      eval[i,]["Precision"],
      eval[i,]["Sensitivity"])
    names(tmp) <- c(paste(rownames(eval)[i], "_F1"),
                    paste(rownames(eval)[i], "_Precision"),
                    paste(rownames(eval)[i], "_Sensitivity"))
    tmp
  }, simplify = FALSE)
  metrics_df <- metrics_ls[[1]]
  for (i in 2:length(cats)) {
    metrics_df <- cbind(metrics_df, metrics_ls[[i]])
  }
  
  rownames(metrics_df) = model_name
  
  return(metrics_df)
  
}



# Build and evaluate main models
main_models <- function(resample="None", vars, LR_vars, eps = NULL, 
                        eps_min = NULL, eps_maj = NULL, epsDown = NULL,
                        epsUp = NULL, resample_fun=NULL, imb_ratio = 3, 
                        C = 5){
  
  # Resampling
  if(resample != "None"){
    if(resample %in% c("downSample", "upSample")){
      df_train <- resample_fun(X_train, y_train, list = FALSE, 
                               yname = "Class")
    }
    else if(resample == "downCluster"){
      minPts <- 2*ncol(X_train)
      df_train <- resample_fun(X_train, y_train, eps, minPts)
    }
    else if(resample == "downClusterMulti"){
      # Glass dataset is not large, we will use smaller minPts
      minPts <- ncol(X_train)
      df_train <- resample_fun(X_train, y_train, epsDown, minPts)
    }
    else if(resample == "SMOTE"){
      X_train_num <- X_train[, !names(X_train) %in%
                               c("SEX", "EDUCATION", "MARRIAGE")]
      data = resample_fun(X_train_num, y_train, K = imb_ratio, 
                   dup_size = imb_ratio - 1)
      df_train = data$data
      names(df_train)[names(df_train) == 'class'] <- 'Class'
      df_train$Class <- factor(df_train$Class)
    }
    else if(resample == "upCluster"){
      minPts <- 2*ncol(X_train)
      df_train <- resample_fun(X_train, y_train,
                               eps_min = eps_min, 
                               eps_maj = eps_maj, 
                               minPt = minPts)
    }
    else if(resample == "upClusterMulti"){
      minPts <- ncol(X_train)
      df_train <- resample_fun(X_train, y_train, epsUp, minPt = minPts)
    }
    else{
      df_train <- resample_fun(X_train, y_train)
    }
    
    X_train = df_train[, !names(df_train) %in% c("Class")]
    y_train = df_train[, "Class"]
  }
  
  if(length(levels(y_train)) == 2){
    evaluate <- evaluate_binary
    binary <- TRUE
  }else{
    evaluate <- evaluate_multi
    binary = FALSE
  }
  
  # Logistic regression
  if(binary){
    if(resample == "SMOTE"){
      model_lr <- glm(formula = y_train ~ ., 
                      family = binomial(link = "logit"), 
                      data = X_train[, vars])
    }
    else{
      model_lr <- glm(formula = y_train ~ ., 
                      family = binomial(link = "logit"), 
                    data = X_train[, LR_vars])
    }
   
  }else{
    model_lr <- multinom(y_train ~ ., data = X_train[, LR_vars])
  }
  
  eval_lr <- evaluate(X_test, y_test, model_lr, 
                      type = ifelse(binary, "response", "class"), 
                      model_name = "LR", df_name)
  
  # kNN
  model_knn <- knn3(X_train[, vars], y_train, k=5)
  eval_knn5 <- evaluate(X_test[, vars], y_test, model_knn, 
                        type = ifelse(binary, "prob", "class"), 
                        model_name = "kNN(k = 5)", df_name)
  
  model_knn <- knn3(X_train[, vars], y_train, k=10)
  eval_knn10 <- evaluate(X_test[, vars], y_test, model_knn, 
                         type = ifelse(binary, "prob", "class"), 
                         model_name = "kNN(k = 10)", df_name)
  
  model_knn <- knn3(X_train[, vars], y_train, k=20)
  eval_knn20 <- evaluate(X_test[, vars], y_test, model_knn,
                         type = ifelse(binary, "prob", "class"), 
                         model_name = "kNN(k = 20)", df_name)
  if(binary){
  model_knn <- knn3(X_train[, vars], y_train, k=50)
  eval_knn50 <- evaluate(X_test[, vars], y_test, model_knn, 
                         type = ifelse(binary, "prob", "class"),  
                         model_name = "kNN(k = 50)", df_name)
  }
  
  # SVM: linear, radial, sigmoid and polynomial kernel
  svm_tuned <- tune(svm, Class~., 
                    data = df_train[, c(vars, "Class")], 
                    kernel = "linear", 
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

  svm_lin <- evaluate(X_test[, vars], y_test, svm_tuned$best.model, 
                      type = ifelse(binary, "response", "class"), 
                      model_name = "SVM_linear", df_name, SVM = TRUE)
  
  svm_tuned <- tune(svm, Class~., 
                    data = df_train[, c(vars, "Class")], 
                    kernel = "radial", 
                    ranges = list(cost = c(0.1,1,10,100,1000),
                                  gamma = c(0.5,1,2,3,4)))
  svm_rad <- evaluate(X_test[, vars], y_test, svm_tuned$best.model, 
                      type = ifelse(binary, "response", "class"),  
                      model_name = "SVM_radial", df_name, SVM = TRUE)
  
  svm_tuned <- tune(svm, Class~., 
                    data = df_train[, c(vars, "Class")], 
                    kernel = "sigmoid", 
                    ranges = list(cost = c(0.1,1,10,100,1000),
                                  gamma = c(0.5,1,2,3,4),
                                  coef0 = c(0.5,1,2,3,4)))
  svm_sig <- evaluate(X_test[, vars], y_test, svm_tuned$best.model,
                      type = ifelse(binary, "response", "class"), 
                      model_name = "SVM_sigmoid", df_name, SVM = TRUE)
  
  svm_tuned <- tune(svm, Class~., 
                    data = df_train[, c(vars, "Class")], 
                    kernel = "polynomial", 
                    ranges = list(cost = c(0.1,10,100),
                                  gamma = c(0.5,1,2),
                                  coef0 = c(0.5,1,2),
                                  degree = c(1,2,3)))
  svm_poly <- evaluate(X_test[, vars], y_test, svm_tuned$best.model, 
                       type = ifelse(binary, "response", "class"),  
                       model_name = "SVM_polynomial", df_name, SVM = TRUE)
  
  # Decision trees: CART, C4.5, Hellinger tree
  cart_tree <- rpart(Class~., data = df_train, method = "class")
  eval_cart <- evaluate(X_test, y_test, cart_tree, 
                        type = ifelse(binary, "prob", "class"),  
                        model_name = "CART", df_name)
  
  c45_tree <- J48(Class~., data=df_train)
  eval_c45 <- evaluate(X_test, y_test, c45_tree, 
                       type = ifelse(binary, "prob", "class"), 
                       model_name = "C4.5", df_name)
  if(binary){
    h_tree <- hd_tree(X_train, y_train, C = C)
    eval_h <- evaluate(X_test, y_test, h_tree, model_name = "Hellinger",
                       df_name=df_name, Hellinger=TRUE)
    eval_all <- rbind(eval_lr, eval_knn5, eval_knn10, eval_knn20, 
                      eval_knn50, svm_lin, svm_rad, svm_sig, svm_poly,
                      eval_cart, eval_c45, eval_h)
  }else{
    eval_all <- rbind(eval_lr, eval_knn5, eval_knn10, eval_knn20, 
                      svm_lin, svm_rad, svm_sig, svm_poly,
                      eval_cart, eval_c45)
  }
  
  
  if(resample != "None"){
    rownames(eval_all) = paste(resample, rownames(eval_all), sep = "_")
  }
  
  return(eval_all)
}


# Main function
main <- function(X_train, y_train, X_test, y_test, vars, LR_vars, eps=NULL,
                 eps_maj = NULL, eps_min = NULL, epsDown = NULL,
                 epsUp = NULL, df_name, minor_num = 2, 
                 imb_ratio = 3){
  
  # Evaluate main models without resampling
  eval_main <- main_models(vars = vars, LR_vars=LR_vars, C=5)
  
  # Downsampling: random downsampling and
  # downsampling with clustering
  eval_downs <- main_models(resample = "downSample",
                            vars = vars,
                            LR_vars = LR_vars,
                            resample_fun = downSample
                            )
  
  if(binary){
    eval_downclus <- main_models(resample = "downCluster",
                                vars = vars,
                                LR_vars = LR_vars,
                                eps = eps,
                                resample_fun = dbscan_clust,
                                epse = eps)
  }else{
    eval_downclus <- main_models(resample = "downClusterMulti",
                               vars = vars,
                               LR_vars = LR_vars,
                               epsDown = epsDown,
                               resample_fun = dbscan_multi
                              ) 
  }
  
  # Oversampling: random oversampling, SMOTE and oversampling
  # with clustering
  eval_ups <- main_models(resample = "upSample",
                          vars = vars,
                          LR_vars = LR_vars,
                          resample_fun = upSample,
                          C = 100)
  
  if(binary){
    eval_smote <- main_models(resample = "SMOTE", 
                              vars = vars,
                              LR_vars = LR_vars,
                              resample_fun = SMOTE,
                              imb_ratio = imb_ratio,
                              C = 100)
  }else{
    eval_smote <- main_models(resample = "SMOTEMulti", 
                              vars = vars,
                              LR_vars = LR_vars,
                              resample_fun = SMOTE_multi,
                              )
  }
  if(binary){
    eval_upclus <- main_models(resample = "upCluster",
                             vars = vars,
                             LR_vars = LR_vars,
                             eps_min = eps_min,
                             eps_maj = eps_maj,
                             resample_fun = clust_oversample,
                             C = 100
                             )
  }else{
    eval_upclus <- main_models(resample = "upClusterMulti",
                               vars = vars,
                               LR_vars = LR_vars,
                               epsUp = epsUp,
                               resample_fun = cluster_overs_multi
                              )  
  }
  
  
  # Cost-sensitive methods: weighted logistic regression, weighted SVM,
  # weighted voting in kNN 
  if(binary){
    model_wlr <- logistic_weight(X_train, y_train, minor_num, LR_vars)
  }else{
    model_wlr <- multinom(y_train ~., data = X_train[, LR_vars],
                          weights = sapply(y_train, function(x)
                            1/table(y_train)[x]))
  }
  eval_wlr <- evaluate(X_test, y_test, model_wlr, 
                       type = ifelse(binary, "response", "class"), 
                       model_name = "Weighted_LR", df_name)
  
  wsvm_tuned <- tune(svm, Class~., 
                    data = df_train[, c(vars, "Class")], 
                    kernel = "linear", 
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)),
                    class.weights = 1/table(y_train))
  wsvm_lin <- evaluate(X_test, y_test, wsvm_tuned$best.model, 
                       type = ifelse(binary, "response", "class"),
                       model_name = "Weighted_SVM_linear", df_name, SVM = TRUE)
  
  wsvm_tuned <- tune(svm, Class~., 
                    data = df_train[, c(vars, "Class")], 
                    kernel = "radial", 
                    ranges = list(cost = c(0.1,1,10,100,1000),
                                  gamma = c(0.5,1,2,3,4)),
                    class.weights = 1/table(y_train))
  wsvm_rad <- evaluate(X_test, y_test, wsvm_tuned$best.model, 
                       type = ifelse(binary, "response", "class"),
                       model_name = "Weighted_SVM_radial", df_name, SVM = TRUE)
  
  wsvm_tuned <- tune(svm, Class~., 
                    data = df_train[, c(vars, "Class")], 
                    kernel = "sigmoid", 
                    ranges = list(cost = c(0.1,1,10,100,1000),
                                  gamma = c(0.5,1,2,3,4),
                                  coef0 = c(0.5,1,2,3,4)),
                    class.weights = 1/table(y_train))
  wsvm_sig <- evaluate(X_test, y_test, wsvm_tuned$best.model, 
                       type = ifelse(binary, "response", "class"),
                      model_name = "Weighted_SVM_sigmoid", df_name, SVM = TRUE)
  
  wsvm_tuned <- tune(svm, Class~., 
                    data = df_train[, c(vars, "Class")], 
                    kernel = "polynomial", 
                    ranges = list(cost = c(0.1,1,10,100,1000),
                                  gamma = c(0.5,1,2,3,4),
                                  coef0 = c(0.5,1,2,3,4),
                                  degree = c(1,2,3,4)),
                    class.weights = 1/table(y_train))
  wsvm_poly <- evaluate(X_test, y_test, wsvm_tuned$best.model, 
                        type = ifelse(binary, "response", "class"),
                        model_name = "Weighted_SVM_polynomial", df_name, 
                        SVM = TRUE)
  
  wknn <- knn_voting(X_train[, vars], y_train, X_test[, vars], y_test, 
                     k=5)
  eval_wknn5 <- evaluate(X_test[, vars], y_test, wknn, 
                         type = ifelse(binary, "response", "class"), 
                         model_name = "Weighted_kNN(k = 5)", df_name,
                         wkNN = TRUE, wknn_pred = wknn$predict)
  
  wknn <- knn_voting(X_train[, vars], y_train, X_test[, vars], y_test, 
                     k=10)
  eval_wknn10 <- evaluate(X_test[, vars], y_test, wknn, 
                          type = ifelse(binary, "response", "class"), 
                         model_name = "Weighted_kNN(k = 10)", df_name,
                         wkNN = TRUE, wknn_pred = wknn$predict)
  
  wknn <- knn_voting(X_train[, vars], y_train, X_test[, vars], y_test,
                     k=20)
  eval_wknn20 <- evaluate(X_test[, vars], y_test, wknn, 
                          type = ifelse(binary, "response", "class"),
                         model_name = "Weighted_kNN(k = 20)", df_name,
                         wkNN = TRUE, wknn_pred = wknn$predict)
  if(binary){
  wknn <- knn_voting(X_train[, vars], y_train, X_test[, vars], y_test,
                     k=50)
  eval_wknn50 <- evaluate(X_test[, vars], y_test, wknn, type = "prob", 
                         model_name = "Weighted_kNN(k = 50)", df_name,
                         wkNN = TRUE, wknn_pred = wknn$predict)
  }
  # Ensembles: Random forest, Balanced random forest, AdaBoost, 
  # SmoteBoost, RUSBoost, SMOTEBagging, UnderBagging
  if(binary){
    model_rf <- rf(X_train, y_train, minor_num)
  }else{
    model_rf <- randomForest(X_train, y_train)
  }
  eval_rf <- evaluate(X_test, y_test, model_rf, 
                      type = ifelse(binary, "prob", "class") ,
                      model_name = "RF", df_name)
  
  if(binary){
    balanced_rf <- rf_balanced(X_train, y_train, minor_num)
  }else{
    minor_size <- table(y_train)[which.min(table(y_train))]
    balanced_rf <- randomForest(X_train, y_train, strata = y_train,
                             sampsize = rep(unname(minor_size),
                                            length(levels(y_train))))
  }
  eval_brf <- evaluate(X_test, y_test, balanced_rf, 
                       type = ifelse(binary, "prob", "class"),
                      model_name = "Balanced RF", df_name)
  
  if(binary){
    model_ada <- ada_boost(X_train, y_train, minor_num)
  }else{
    model_ada <- boosting(Class~., df_train)
  }
  eval_ada <- evaluate(X_test, y_test, model_ada, 
                       type = ifelse(binary, "prob", "class"), 
                       model_name = "AdaBoost", df_name)
  
  if(binary){
    model_smotebo <- smote_boost(X_train_num, y_train, minor_num)
    eval_smbo <- evaluate(X_test, y_test, model_smotebo, type = "prob", 
                         model_name = "SmoteBoost", df_name)
    
    model_rusb <- rus_boost(X_train, y_train, minor_num, imb_ratio)
    eval_rusb <- evaluate(X_test, y_test, model_rusb, type = "prob", 
                         model_name = "RUSBoost", df_name)
    
    model_smoteba <- smote_bagg(X_train_num, y_train, minor_num)
    eval_smba <- evaluate(X_test, y_test, model_smoteba, type = "prob", 
                         model_name = "SMOTEBagging", df_name)
    
    model_undb <- under_bagg(X_train, y_train, minor_num, imb_ratio)
    eval_undb <- evaluate(X_test, y_test, model_undb, type = "prob", 
                         model_name = "UnderBagging", df_name)
    
    eval_df <- rbind(eval_main, eval_downs, eval_downclus, eval_ups, 
                     eval_smote, eval_upclus,
                     eval_wlr, wsvm_lin, wsvm_rad, wsvm_sig, wsvm_poly,
                     eval_wknn5, eval_wknn10, eval_wknn20, eval_wknn50,
                     eval_rf, eval_brf, eval_ada, eval_smbo, eval_rusb,
                     eval_smba, eval_undb)
  }else{
    eval_df <- rbind(eval_main, eval_downs, eval_downclus, eval_ups, 
                     eval_smote, eval_upclus,
                     eval_wlr, wsvm_lin, wsvm_rad, wsvm_sig, wsvm_poly,
                     eval_wknn5, eval_wknn10, eval_wknn20, 
                     eval_rf, eval_brf, eval_ada)
  }
  write.xlsx(eval_df, paste(df_name, "_eval.xlsx", sep = ""), 
             rowNames=TRUE)
}

# Example 1. Default of credit cards clients
df_train <- default_df
X_train <- default_train
y_train <- default_y_train
X_test <- default_test
y_test <- default_y_test
X_train_num <- default_train_num

main(X_train, y_train, X_test, y_test, vars = predictors_default, 
     LR_vars = LR_vars_default, 
     eps = eps_def, eps_maj = eps_maj_def, 
     eps_min = eps_min_def, df_name = "Default")


# Example 2. Glass type
df_train <- glass_df
X_train <- glass_train
y_train <- glass_y_train
X_test <- glass_test
y_test <- glass_y_test
X_train_num <- glass_train_num

main(X_train, y_train, X_test, y_test, epsDown = eps_glass_down,
     epsUp = eps_glass_up, vars = predictors_glass, 
     LR_vars = predictors_glass, df_name = "Glass")