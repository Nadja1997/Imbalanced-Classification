library(MASS)
library(ggplot2)
library(caret)
library(openxlsx)
library(tidyverse)
library(plotly)

# Logistic vs weighted logistic regression
S_min <- matrix(c(0.8, 0.5,
              0.5, 0.5), 
            nrow = 2, ncol = 2, byrow = TRUE)
data_minor <- mvrnorm(100, mu=c(-0.2,0), Sigma = S_min)
data_minor <- cbind(as.data.frame(data_minor, colnames=c("X1", "X2")),
                    Class = rep(1,100))

S_maj<- matrix(c(1, 0,
              0, 0.5), 
            nrow = 2, ncol = 2, byrow = TRUE)
data_major <- mvrnorm(1000, mu=c(-2,0), Sigma = S_maj)
data_major <- cbind(as.data.frame(data_major, colnames=c("X1", "X2")),
                    Class = rep(0,1000))
data1 <- rbind(data_major, data_minor)
data1$Class <- factor(data1$Class)

g <- ggplot(data1, aes(x=V1, y=V2, color = Class)) + 
  geom_point(show.legend = F) +
  scale_color_manual(values = c("0" = "steelblue",
                                "1" = "orange")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()
        )
g

model_lr <- glm(formula = Class ~ ., 
                family = binomial(link = "logit"), 
                data = data1)
coeff <- model_lr$coefficients
intercept <- -1*coeff[1]/coeff[3]
slope <- -1 * coeff[2]/coeff[3]

g <- g + geom_abline(intercept = intercept, slope = slope, 
                     color="black", size = 0.5)
g

y <- data1$Class
N_minor <- sum(y == 1)
N_major <- sum(y == 0)
N <- N_minor + N_major
weights <- sapply(y, function(x) 
  ifelse(x==1, N/(2*N_minor), N/(2*N_major)))

model_wlr <- glm(formula = Class ~ ., 
                family = binomial(link = "logit"), 
                data = data1,
                weights = weights)
coeffw <- model_wlr$coefficients
interceptw <- -1*coeffw[1]/coeffw[3]
slopew <- -1 * coeffw[2]/coeffw[3]

g <- g + geom_abline(intercept = interceptw, slope = slopew, 
                     color="black", linetype  = "dashed", size = 0.6)

g + ggtitle("Logistic regression \nvs weighted logistic regression") +
  theme(plot.title = element_text(hjust = 0.5, size = 9))


# different shapes of imbalanced data

S_maj<- matrix(c(0.5, 0,
                 0, 0.5), 
               nrow = 2, ncol = 2, byrow = TRUE)
data_major <- mvrnorm(2000, mu=c(-2,0), Sigma = S_maj)
data_major <- cbind(as.data.frame(data_major, colnames=c("X1", "X2")),
                    Class = rep(0,2000))

S_min1 <- matrix(c(0.8, 0.5,
                  0.5, 0.5), 
                nrow = 2, ncol = 2, byrow = TRUE)
data_minor1 <- mvrnorm(100, mu=c(0.2,-0.7), Sigma = S_min1)
data_minor1 <- cbind(as.data.frame(data_minor1, colnames=c("X1", "X2")),
                    Class = rep(1,100))

S_min2 <- matrix(c(0.01, 0,
                   0, 0.01), 
                 nrow = 2, ncol = 2, byrow = TRUE)
data_minor2 <- mvrnorm(10, mu=c(-2,0), Sigma = S_min2)
data_minor2 <- cbind(as.data.frame(data_minor2, colnames=c("X1", "X2")),
                     Class = rep(2,10))

data_minor3 <- data.frame(V1 = c(-3, -1, -3),
                          V2 = c(-0.5, 0.5, 1),
                          Class = factor(rep(3, 3)))

data2 <- rbind(data_major, data_minor1, data_minor2, data_minor3)
data2$Class <- factor(data2$Class)
g <- ggplot(data2, aes(x=V1, y=V2, color = Class)) + 
  geom_point(show.legend = F) +
  scale_color_manual(values = c("0" = "steelblue",
                                "1" = "orange",
                                "2" = "plum3",
                                "3" = "cyan")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )
g

# AUC sensitivity
trainIndex <- createDataPartition(y, p = .8, 
                                  list = FALSE, 
                                  times = 1)
df_train <- data1[trainIndex,]
X_train = df_train[, !names(df_train) %in% c("Class")]
y_train = df_train[, "Class"]

df_test <- data1[-trainIndex,]
X_test = df_test[, !names(df_train) %in% c("Class")]
y_test = df_test[, "Class"]

model_lr <- glm(formula = y_train ~ ., 
                family = binomial(link = "logit"), 
                data = X_train)

probs <- predict(model_lr, X_test, type = "response")
y_pred <- factor(ifelse(probs > 0.5, 
                        "1",
                        "0"))
metrics <- confusionMatrix(y_pred, y_test, positive = "1")
auc(y_test, probs)
eval_lr <- evaluate(X_test, y_test, model_lr, type = "response", 
         model_name = "LR", df_name, tr = 0.5)

eval_lr0 <- evaluate(X_test, y_test, model_lr, type = "response", 
                    model_name = "LR", df_name, tr = 0)

eval_lr1 <- evaluate(X_test, y_test, model_lr, type = "response", 
                    model_name = "LR", df_name, tr = 1)

points(eval_lr$Specificity, eval_lr$Sensitivity)
text(eval_lr$Specificity, eval_lr$Sensitivity, "t = 0.5")
points(eval_lr0$Specificity, eval_lr0$Sensitivity)
text(eval_lr0$Specificity, eval_lr0$Sensitivity, "t = 0")
points(eval_lr1$Specificity, eval_lr1$Sensitivity)
text(eval_lr1$Specificity, eval_lr1$Sensitivity, "t = 1")

g <- ggroc(roc(y_test, probs), legacy.axes = TRUE)
g
g<- g + geom_point(aes(x = 1-eval_lr$Specificity, 
                       y = eval_lr$Sensitivity)) +
  annotate(geom="text", x=(1-eval_lr$Specificity + 0.1), 
           y=eval_lr$Sensitivity, label="t = 0.5", size=3) +
  annotate(geom="text", x=(1-eval_lr$Specificity + 0.15), 
           y=(eval_lr$Sensitivity-0.1), 
           label=paste("TPR = ",eval_lr$Sensitivity), size=3) +
  annotate(geom="text", x=(1-eval_lr$Specificity + 0.15), 
           y=(eval_lr$Sensitivity - 0.2), 
           label=paste("TNR = ",eval_lr$Specificity), size=3) +
  geom_point(aes(x = 1-eval_lr0$Specificity, 
                 y = eval_lr0$Sensitivity)) +
  annotate(geom="text", (x=1-eval_lr0$Specificity-0.025), 
           y=(eval_lr0$Sensitivity-0.05), label="t = 0", size=3) +
  geom_point(aes(x = 1-eval_lr1$Specificity, 
                 y = eval_lr1$Sensitivity)) +
  annotate(geom="text", x=(1-eval_lr1$Specificity + 0.08), 
           y=eval_lr1$Sensitivity, label="t = 1", size=3) +
  annotate(geom="text", x=0.7, 
           y=0.6, label=paste("AUC = ", eval_lr$AUC), size=4) +
  ggtitle("ROC curve for logistic regression\nimbalance ratio = 10:1") +
  theme(plot.title = element_text(hjust = 0.5, size = 9))
g

# Variable selection methods
var <- c(rnorm(200,1,), rnorm(200,7,1), rnorm(40, 10, 1))
y = factor(c(rep("0", 400), rep("1", 40)))
levels(y) <- c("neg", "pos")
df <- data.frame(V1 = var,
                 Class = factor(y))

fs <- f_score(as.data.frame(var), factor(y))
fs
X_disc <- Discretize(Class ~., data = df)
mu_inf <- mutinformation(X_disc$V1, y)
mu_inf
g <- ggplot(df, aes(x=V1, color=Class)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") +
  scale_color_manual(values = c("aquamarine3", "orange")) +
  ggtitle(paste("F-score =", round(fs$score,3), 
                "\nMutual inf. =", round(mu_inf,3) )) +
  theme(plot.title = element_text(hjust = 0.5, size = 9))
g

fs1 <- f_score(as.data.frame(var), factor(y), cluster=TRUE)[1]
fs1
clusters <- f_score(as.data.frame(var), factor(y), cluster=TRUE)[2]

mu_inf1 <- mutinformation(X_disc$V1, c(factor(clusters[[1]]), 
                            droplevels(y[y=="pos"])))
mu_inf1

df_clustered <- data.frame(V1 = var, 
                           Class = c(factor(clusters[[1]]), 
                                     droplevels(y[y=="pos"])))
g1 <- ggplot(df_clustered, aes(x=V1, color=Class)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") +
  scale_color_manual(values = c("aquamarine3", "lightpink", "orange")) +
  ggtitle(paste("F-score =", round(fs1[1][[1]]$score,3),
                "\nMutual inf. =", round(mu_inf1,3))) +
  theme(plot.title = element_text(hjust = 0.5, size = 9))
g1

# SVM illustration
S_maj<- matrix(c(0.5, 0,
                 0, 0.5), 
               nrow = 2, ncol = 2, byrow = TRUE)
data3a <- mvrnorm(200, mu=c(-2,0), Sigma = S_maj)
data3a <- cbind(as.data.frame(data3a, colnames=c("X1", "X2")),
                    Class = rep(0,200))

S_maj<- matrix(c(0.5, 0,
                 0, 0.5), 
               nrow = 2, ncol = 2, byrow = TRUE)
data3b <- mvrnorm(200, mu=c(2,0), Sigma = S_maj)
data3b <- cbind(as.data.frame(data3b, colnames=c("X1", "X2")),
                Class = rep(1,200))
data3 <- rbind(data3a, data3b)
data3$Class <- factor(data3$Class)
g <- ggplot(data3, aes(x=V1, y=V2, color = Class)) + 
  geom_point(show.legend = F) +
  scale_color_manual(values = c("0" = "steelblue",
                                "1" = "orange")) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )
g


# Default dataset vizualization
df <- read.xlsx("Default_eval.xlsx")
metrics <- df$X1
LR_metrics <- metrics[grepl("LR", metrics)]

knn5_metrics <- metrics[grepl("k = 5)", metrics)]
knn10_metrics <- metrics[grepl("k = 10)", metrics)]
knn20_metrics <- metrics[grepl("k = 20)", metrics)]
knn50_metrics <- metrics[grepl("k = 50)", metrics)]

SVMlin_metrics <- metrics[grepl("SVM_linear", metrics)]
SVMrad_metrics <- metrics[grepl("SVM_radial", metrics)]
SVMsig_metrics <- metrics[grepl("SVM_sigmoid", metrics)]
SVMpoly_metrics <- metrics[grepl("SVM_polynomial", metrics)]

C45_metrics <- metrics[grepl("C4.5", metrics)]
CART_metrics <- metrics[grepl("CART", metrics)]
Hellinger_metrics <- metrics[grepl("Hellinger", metrics)]

bagging_metrics <- c("RF", "Balanced RF", "UnderBagging", "SMOTEBagging")
boosting_metrics <- c("AdaBoost", "SmoteBoost", "RUSBoost")

df[df$X1 %in% LR_metrics,][,c("X1","Precision","Sensitivity")]


# Bar plot of precision and sensitivity per model
barchart <- function(models){
  
  df_bar <- df[df$X1 %in% models,][,c("X1","Precision","Sensitivity")]
  n <- nrow(df_bar)
  
  values <- sapply(1:n, function(i) apply(df_bar[i,c("Precision","Sensitivity")],
                                          1, function(x) x))
  df_grouped <- data.frame(Model = factor(as.vector(sapply(df_bar$X1, 
                                                           function(x) c(x,x))),
                                          levels <- df_bar$X1),
                          Metric = rep(c("Precision","Sensitivity"), n),
                          Val = as.vector(values))
  
  ggplot(data=df_grouped, aes(x=Model, y=Val, fill=Metric)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=round(Val,2)), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5) + 
    scale_fill_brewer(palette="Paired") +
    ylim(0,0.8)+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "top",
          legend.title=element_blank()
    )
}

barchart(LR_metrics)
barchart(knn5_metrics)
barchart(knn10_metrics)
barchart(knn20_metrics)
barchart(knn50_metrics)
barchart(SVMlin_metrics)
barchart(SVMrad_metrics)
barchart(SVMpoly_metrics)
barchart(C45_metrics)
barchart(CART_metrics)
barchart(Hellinger_metrics)
barchart(bagging_metrics)
barchart(boosting_metrics)
barchart(c(bagging_metrics,boosting_metrics))


# Bar plot of top 20 Precision, Sensitivity and F1
bar_chart_max <- function(metric){
  
  df_metric <- df[,c("X1", metric)]
  colnames(df_metric) <- c("Model", "Metric")
  df_metric <- drop_na(df_metric)
  df_top20 <- head(df_metric[order(df_metric$Metric, decreasing = T),], 20)
  df_top20$Model<- factor(df_top20$Model, levels = df_top20$Model)
  
  ggplot(data=df_top20, aes(x=Model, y=Metric)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=round(Metric,2)), vjust=1.6, color="white", size=3.5) +
    ylim(0,0.8) +
    ggtitle(paste("Top 20 models with the highest", metric)) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title = element_text(hjust = 0.5, size = 11)
          
    )
}

bar_chart_max("F1")
bar_chart_max("Precision")
bar_chart_max("Sensitivity")
