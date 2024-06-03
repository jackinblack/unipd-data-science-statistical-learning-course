library(splitTools)
library(pROC) # good
library(ROSE)
library(ggplot2)
library(cowplot)
library(dplyr)
library(Rmisc)
library(corrplot) # good
library(MASS) # good
library(caret) # confusionMatrix
library(regclass)
library(fastDummies)
library(class) # good
library(glmnet) # good
library(outliers)
library(mlr3measures)
library(pracma)
library(FactoMineR)
library(e1071) # good

set.seed(1) 

# https://archive.ics.uci.edu/ml/machine-learning-databases/00222/
# http://archive.ics.uci.edu/ml/datasets/Bank+Marketing#
# data <- read.csv('data/bank-full.csv', sep=';', na.strings="unknown")
data <- read.csv('../data/bank-additional-full.csv', sep=';', 
                 na.strings=c("unknown"))

#group features
bin_feats <- c("default", "housing", "loan", "contact")
ordinal_feats <- c("education")
nominal_feats <- c("job", "marital",
                   "month", "day_of_week", "poutcome")
continuous_feats <- c("age", "duration", "campaign", "pdays", "previous",
                      "emp.var.rate", "cons.price.idx", "cons.conf.idx",
                      "euribor3m", "nr.employed")

# convert cat feats to factor 
data$y <- as.factor(data$y)
data$marital <- factor(data$marital, levels = c("single", "divorced", 
                                                "married"), ordered = FALSE)
data$education <- factor(data$education, levels = c('illiterate',
                                                    'basic.4y','basic.6y',
                                                    'basic.9y','high.school',
                                                    'professional.course',
                                                    'university.degree'), 
                         ordered = TRUE)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$contact <- as.factor(data$contact)
data$day_of_week <- factor(data$day_of_week, 
                           levels = c("mon", "tue", "wed", "thu", "fri"), 
                           ordered = FALSE)
data$poutcome <- factor(data$poutcome, levels = c("nonexistent", "failure", 
                                                  "success"), ordered = FALSE)
data$job <- factor(data$job, levels = c('admin.','blue-collar','entrepreneur',
                                        'housemaid', 'management','retired',
                                        'self-employed','services',
                                        'student','technician','unemployed'), 
                   ordered = FALSE)
data$month <- factor(data$month, levels = c("jan", "feb", "mar", "apr", "may", 
                                            "jun", "jul", "aug", "sep", "oct", 
                                            "nov", "dec"), ordered = TRUE)
# adding year
data$year <- NA
year = 2008
for (i in 1:(nrow(data)-1)) {
  data[i, 'year'] <- year
  if (data[i, 'month'] > data[i+1, 'month']) {
    year <- year + 1
  } 
}
data[i+1, 'year'] <- year

# adding date
data$date <- paste(as.character(data$year), "01", 
                   as.character(as.numeric(data$month)), 
                   sep = "-")
data$date <- as.Date(data$date)

# remove leak feats
data <- subset(data, select = -c(duration))

# remove useless feats
# save default values for feature needs
default <- data$default
default[is.na(default)] <- "no"

# remove useless feats
data <- subset(data, select = -c(default))

# remove feats which can't be used for prediction
data <- subset(data, select = -c(month, date))

# update groups of feats
continuous_feats <- continuous_feats[! continuous_feats %in% c('duration')]
continuous_feats <- c(continuous_feats, "year")
nominal_feats <- nominal_feats[! nominal_feats %in% c('month')]
bin_feats <- bin_feats[! bin_feats %in% c('default')]

# time series train test split
# leave 20% of examples for a test set
cut_idx = round(nrow(data) * 0.75)
train <- data[1:cut_idx, ]
test <- data[(cut_idx+1):nrow(data), ]

# check target distribution in the datasets
table(train$y) / nrow(train) * 100
table(test$y) / nrow(test) * 100

# function for calcilating mode
calc_mode <- function(x){
  
  vals <- unique(x)
  counts <- tabulate(match(x, vals))
  mode <- vals[which.max(counts)]
  return(mode)
}

# function for filling train and test sets
fill_na_tr_tst <- function(tr, tst, feats) {
  tr_filled <- tr
  tst_filled <- tst
  for (feat in feats) {
    mode_val <- calc_mode(tr[, feat])
    tr_filled[is.na(tr_filled[, feat]), feat] <- mode_val
    tst_filled[is.na(tst_filled[, feat]), feat] <- mode_val
  }
  return(list(train = tr_filled, test = tst_filled))
}

# fill NA with mode
missing_feats <- c("job", "education", "marital", "housing", "loan")
filled_dfs <-fill_na_tr_tst(train, test, missing_feats)
train <- filled_dfs$train
test <- filled_dfs$test

# remove outliers from train set
train <- train[default[0:cut_idx] != "yes", ]
max_campaign <- quantile(train$campaign)[4] + 1.5*IQR(train$campaign)
train <- train[train$campaign <= max_campaign, ]

# create a validation set
val_cut_idx = round(nrow(train) * 0.85)
val_train <- train[0:val_cut_idx, ]
val_val <- train[(val_cut_idx+1):nrow(train), ]

# additional data preprocessing for Ridge, Lasso, KNN
# create dummies
data_for_dummies <- rbind(train, test)
data_dummies <- dummy_cols(data_for_dummies, 
                           select_columns=c(bin_feats, ordinal_feats, 
                                            nominal_feats),
                           remove_selected_columns=TRUE)
# replace "-" with "_" for avoiding further errors
colnames(data_dummies)[colnames(data_dummies) == 
                         "job_blue-collar"] <- "job_blue_collar"
colnames(data_dummies)[colnames(data_dummies) == 
                         "job_self-employed"] <- "job_self_employed"
# again, train test split
train_dummies <- data_dummies[1:nrow(train), ]
test_dummies <- data_dummies[(nrow(train_dummies)+1):nrow(data_dummies), ]

# min max scaling
mins <- sapply(subset(train_dummies, select = -c(y)), min)
maxs <- sapply(subset(train_dummies, select = -c(y)), max)
maxs_m_mins <- maxs - mins

# function for min max scaling
min_max_scaling <- function(df, mins, maxs_m_mins) {
  df_sc <- sweep(subset(df, select = -c(y)),
                 2,
                 mins)
  df_sc <- sweep(df_sc,
                 2,
                 maxs_m_mins, FUN='/')
  df_sc$y <- df$y
  return(df_sc)
}

train_dummies_sc <- min_max_scaling(train_dummies, mins, maxs_m_mins)
test_dummies_sc <- min_max_scaling(test_dummies, mins, maxs_m_mins)

# leave some samples for validation set
val_train_dummies_sc <- train_dummies_sc[0:val_cut_idx, ]
val_val_dummies_sc <- train_dummies_sc[(val_cut_idx+1):nrow(train_dummies), ]

# Logistic regression
# set default threshold for computing binary predictions
threshold <- 0.5
# modeling
model_lr <- glm(y ~., data=train, family = binomial)
summary(model_lr)

# prediction
probabilities_lr <- predict(model_lr, test, type="response")
preds_lr <- ifelse(probabilities_lr > threshold, "yes", "no")
preds_lr <- factor(preds_lr, levels=c("no", "yes"))

# evaluation
roc_score_lr <- roc(test$y, probabilities_lr)
plot(roc_score_lr ,main =sprintf("ROC curve for Logistic Regression. AUC = %s", 
                              round(roc_score_lr$auc, 2)))
confusionMatrix(data=preds_lr, reference = test$y)

# VIF
VIF(model_lr)

# PCA + removing pdays and previous
first_g_correlated_feats <- c("year", "nr.employed", "euribor3m", 
                              "emp.var.rate", "cons.price.idx")
pca <- PCA(train[,first_g_correlated_feats], graph = FALSE)
pca_train <- subset(train, select = -c(year, nr.employed, euribor3m, 
                                       emp.var.rate, cons.price.idx))
pca_test <- subset(test, select = -c(year, nr.employed, euribor3m, 
                                     emp.var.rate, cons.price.idx))
pca_comps_train <- predict(pca, train[,first_g_correlated_feats])$coord
pca_comps_test <- predict(pca, test[,first_g_correlated_feats])$coord
pca_train <- cbind(pca_train, pca_comps_train)
pca_test <- cbind(pca_test, pca_comps_test)

model_pca_lr <- glm(y ~.-pdays-previous, data=pca_train, family = binomial)
# now we have reduced multicollinearity
VIF(model_pca_lr)
summary(model_pca_lr)

# prediction
probabilities_pca_lr <- predict(model_pca_lr, pca_test, type="response")
preds_pca_lr <- ifelse(probabilities_pca_lr > threshold, "yes", "no")
preds_pca_lr <- as.factor(preds_pca_lr)

# evaluation
roc_score_pca_lr <- roc(test$y, probabilities_pca_lr)
plot(roc_score_pca_lr, 
     main =sprintf("ROC curve for Logistic Regression. AUC = %s", 
                              round(roc_score_pca_lr$auc, 2)))
confusionMatrix(data=preds_pca_lr, reference = test$y)

# oversampling
pca_over_train <- ovun.sample(y~., data = pca_train, method = "over")$data

# modeling
preds_pca_over_lr <- glm(y ~.-pdays-previous, data=pca_over_train, 
                         family = binomial)
summary(preds_pca_over_lr)

# model diagnostics
par(mfrow=c(2,2))
plot(preds_pca_over_lr)
par(mfrow=c(1,1))

# prediction
probabilities_pca_over_lr <- predict(preds_pca_over_lr, pca_test, 
                                     type="response")
preds_pca_over_lr <- ifelse(probabilities_pca_over_lr > threshold, "yes", "no")
preds_pca_over_lr <- factor(preds_pca_over_lr, levels=c("no", "yes"))

# evaluation
roc_score_pca_over_lr <- roc(test$y, probabilities_pca_over_lr)
plot(roc_score_pca_over_lr, 
     main =sprintf("ROC curve for Logistic Regression. AUC = %s", 
                              round(roc_score_pca_over_lr$auc, 2)))
confusionMatrix(data=preds_pca_over_lr, reference = test$y)

# Backward Stepwise Selection
model_pca_lr <- glm(y ~.-pdays-previous, data=pca_train, family = binomial)
b_step <- step(model_pca_lr, direction= "backward", 
               scope=formula(model_pca_lr), trace=0)
# Removed features
b_step$anova

model_pca_fsel_lr <- glm(y ~.-pdays-previous-housing-campaign-loan-Dim.4-age, 
                         data=pca_train, family = binomial)
summary(model_pca_fsel_lr)

# prediction
probabilities_pca_fsel_lr <- predict(model_pca_fsel_lr, pca_test, 
                                     type="response")
preds_pca_fsel_lr <- ifelse(probabilities_pca_fsel_lr > threshold, "yes", "no")
preds_pca_fsel_lr <- factor(preds_pca_fsel_lr, levels=c("no", "yes"))

# evaluation
roc_score_pca_fsel_lr <- roc(test$y, probabilities_pca_fsel_lr)
plot(roc_score_pca_fsel_lr, 
     main =sprintf("ROC curve for Logistic Regression. AUC = %s", 
                              round(roc_score_pca_fsel_lr$auc, 2)))
confusionMatrix(data=preds_pca_fsel_lr, reference = test$y)

# Forward Stepwise Selection
model_intercept <- glm(y ~ 1, data=pca_train, family = binomial)
f_step <- step(model_intercept, direction= "forward", 
               scope=formula(model_pca_lr), trace=0)
# Removed features
f_step$anova
# The same results

# Ridge
# convert the data to required format
X_val_train_mat <- as.matrix(subset(val_train_dummies_sc, select = -c(y)))
y_val_train_mat <- as.matrix(as.numeric(val_train_dummies_sc$y)-1)
X_val_val_mat <- as.matrix(subset(val_val_dummies_sc, select = -c(y)))
y_val_val_mat <- as.matrix(as.numeric(val_val_dummies_sc$y)-1)
X_train_mat <- as.matrix(subset(train_dummies_sc, select = -c(y)))
y_train_mat <- as.matrix(as.numeric(train_dummies_sc$y)-1)
X_test_mat <- as.matrix(subset(test_dummies_sc, select = -c(y)))
y_test_mat <- as.matrix(as.numeric(test_dummies_sc$y)-1)

# selecting lambda
model_ridge <- glmnet(x=X_val_train_mat, y=y_val_train_mat, 
                      family = binomial, alpha = 0)
probabilities_ridge <- predict(model_ridge, X_val_val_mat, type="response")
roc_auc_scores <- sapply(as.data.frame(probabilities_ridge), 
                         function(x) roc(val_val_dummies_sc$y, x)$auc)
lambdas <- model_ridge$lambda
plot(lambdas, roc_auc_scores, type="l", main ="Validation curve for Ridge")
lambda = lambdas[roc_auc_scores == max(roc_auc_scores)]

# modeling
model_ridge <- glmnet(x=X_train_mat, y=y_train_mat, 
                      family = binomial, alpha = 0, lambda = lambda)

# prediction
probabilities_ridge <- predict(model_ridge, X_test_mat, 
                               type="response")
probabilities_ridge <- as.vector(probabilities_ridge)
preds_ridge <- ifelse(probabilities_ridge > threshold, "yes", "no")
preds_ridge <- as.factor(preds_ridge)

# evaluation
roc_score_ridge <- roc(test$y, probabilities_ridge)
plot(roc_score_ridge ,main =sprintf("ROC curve for Ridge. AUC = %s", 
                              round(roc_score_ridge$auc, 2)))
confusionMatrix(data=preds_ridge, reference = test$y)

# oversampling
val_train_dummies_sc_over <- ovun.sample(y~., data = val_train_dummies_sc, 
                                         method = "over")$data
train_dummies_sc_over <- ovun.sample(y~., data = train_dummies_sc, 
                                         method = "over")$data

X_val_train_over_mat <- as.matrix(subset(val_train_dummies_sc_over, 
                                         select = -c(y)))
y_val_train_over_mat <- as.matrix(as.numeric(val_train_dummies_sc_over$y)-1)
X_train_over_mat <- as.matrix(subset(train_dummies_sc_over, select = -c(y)))
y_train_over_mat <- as.matrix(as.numeric(train_dummies_sc_over$y)-1)

# selecting lambda
model_ridge_over <- glmnet(x=X_val_train_over_mat, y=y_val_train_over_mat, 
                      family = binomial, alpha = 0)
probabilities_ridge_over <- predict(model_ridge_over, X_val_val_mat, 
                                    type="response")
roc_auc_scores <- sapply(as.data.frame(probabilities_ridge_over), 
                         function(x) roc(val_val_dummies_sc$y, x)$auc)
lambdas <- model_ridge_over$lambda
plot(lambdas, roc_auc_scores, type="l", main ="Validation curve for Ridge")
lambda = lambdas[roc_auc_scores == max(roc_auc_scores)][1]

# modeling
model_ridge_over <- glmnet(x=X_train_over_mat, y=y_train_over_mat, 
                      family = binomial, alpha = 0, lambda = lambda)

# prediction
probabilities_ridge_over <- predict(model_ridge_over, X_test_mat, 
                                    type="response")
probabilities_ridge_over <- as.vector(probabilities_ridge_over)
preds_ridge_over <- ifelse(probabilities_ridge_over > threshold, "yes", "no")
preds_ridge_over <- as.factor(preds_ridge_over)

# evaluation
roc_score_ridge_over <- roc(test$y, probabilities_ridge_over)
plot(roc_score_ridge_over ,main =sprintf("ROC curve for Ridge. AUC = %s", 
                              round(roc_score_ridge_over$auc, 2)))
confusionMatrix(data=preds_ridge_over, reference = test$y)

# Lasso
# selecting lambda
model_lasso <- glmnet(x=X_val_train_mat, y=y_val_train_mat, 
                      family = binomial, alpha = 1)
probabilities_lasso <- predict(model_lasso, X_val_val_mat, type="response")
roc_auc_scores <- sapply(as.data.frame(probabilities_lasso), 
                         function(x) roc(val_val_dummies_sc$y, x)$auc)
lambdas <- model_lasso$lambda
plot(lambdas, roc_auc_scores, type="l", main ="Validation curve for Lasso")
lambda = lambdas[roc_auc_scores == max(roc_auc_scores)][1]

# modeling
model_lasso <- glmnet(x=X_train_mat, y=y_train_mat, 
                      family = binomial, alpha = 1, lambda = lambda)

# prediction
probabilities_lasso <- predict(model_lasso, X_test_mat, 
                               type="response")
probabilities_lasso <- as.vector(probabilities_lasso)
preds_lasso <- ifelse(probabilities_lasso > threshold, "yes", "no")
preds_lasso <- as.factor(preds_lasso)

# evaluation
roc_score_lasso <- roc(test$y, probabilities_lasso)
plot(roc_score_lasso ,main =sprintf("ROC curve for Lasso. AUC = %s", 
                              round(roc_score_lasso$auc, 2)))
confusionMatrix(data=preds_lasso, reference = test$y)

# oversampling
# selecting lambda
model_lasso_over <- glmnet(x=X_val_train_over_mat, y=y_val_train_over_mat, 
                           family = binomial, alpha = 1)
probabilities_lasso_over <- predict(model_lasso_over, X_val_val_mat, 
                                    type="response")
roc_auc_scores <- sapply(as.data.frame(probabilities_lasso_over), 
                         function(x) roc(val_val_dummies_sc$y, x)$auc)
lambdas <- model_lasso_over$lambda
plot(lambdas, roc_auc_scores, type="l", main ="Validation curve for Lasso")
lambda = lambdas[roc_auc_scores == max(roc_auc_scores)]

# modeling
model_lasso_over <- glmnet(x=X_train_over_mat, y=y_train_over_mat, 
                           family = binomial, alpha = 1, lambda = lambda)

# prediction
probabilities_lasso_over <- predict(model_lasso_over, X_test_mat, 
                                    type="response")
probabilities_lasso_over <- as.vector(probabilities_lasso_over)
preds_lasso_over <- ifelse(probabilities_lasso_over > threshold, "yes", "no")
preds_lasso_over <- as.factor(preds_lasso_over)

# evaluation
roc_score_lasso_over <- roc(test$y, probabilities_lasso_over)
plot(roc_score_lasso_over ,main =sprintf("ROC curve for Lasso. AUC = %s", 
                              round(roc_score_lasso_over$auc, 2)))
confusionMatrix(data=preds_lasso_over, reference = test$y)

# LDA
# Checking normality assumption
continuous_feats_pca <- c("age", "campaign",
                          "cons.conf.idx", "Dim.1", "Dim.2", "Dim.3", 
                          "Dim.4", "Dim.5")
limit <- 5000
par(mfrow=c(4, 4))
for (feat in continuous_feats_pca) {
  for (cl_lab in c("yes", "no")) {
    init_arr <- pca_train[pca_train$y == cl_lab, feat]
    size <- min(limit, length(init_arr))
    arr <- sample(init_arr, size)
    res <- shapiro.test(arr)
    p_val <- res[2]$p.value
    p_val <- round(p_val, 2)
    qqnorm(arr, pch = 1, frame = FALSE,
           main=sprintf("%s. y = %s. Sh.-W. p-value = %s", 
                        feat, cl_lab, p_val), cex.lab=2, cex.main=2)
    qqline(arr, col = "steelblue", lwd = 2)
  }
}
par(mfrow=c(1, 1))

# modeling
model_pca_lda <- lda(y ~.-pdays-previous, data=pca_train)
model_pca_lda

# distribution of discriminant function values
plot(model_pca_lda)

# prediction
probabilities_pca_lda <- predict(model_pca_lda, pca_test, type="response")
probabilities_pca_lda <- probabilities_pca_lda$posterior[,2]
preds_pca_lda <- ifelse(probabilities_pca_lda > threshold, "yes", "no")
preds_pca_lda <- as.factor(preds_pca_lda)

# evaluation
roc_score_pca_lda <- roc(test$y, probabilities_pca_lda)
plot(roc_score_pca_lda ,main =sprintf("ROC curve for LDA. AUC = %s", 
                              round(roc_score_pca_lda$auc, 2)))
confusionMatrix(data=preds_pca_lda, reference = test$y)

# oversampling
# modeling
model_pca_over_lda <- lda(y ~.-pdays-previous, data=pca_over_train)
model_pca_over_lda

# distribution of discriminant function values
plot(model_pca_over_lda)

# prediction
probabilities_pca_over_lda <- predict(model_pca_over_lda, pca_test, 
                                      type="response")
probabilities_pca_over_lda <- probabilities_pca_over_lda$posterior[,2]
preds_pca_over_lda <- ifelse(probabilities_pca_over_lda > threshold, 
                             "yes", "no")
preds_pca_over_lda <- as.factor(preds_pca_over_lda)

# evaluation
roc_score_pca_over_lda <- roc(test$y, probabilities_pca_over_lda)
plot(roc_score_pca_over_lda ,main =sprintf("ROC curve for LDA. AUC = %s", 
                              round(roc_score_pca_over_lda$auc, 2)))
confusionMatrix(data=preds_pca_over_lda, reference = test$y)

# QDA
# modeling
model_pca_qda <- qda(y ~.-pdays-previous, data=pca_train)
model_pca_qda

# prediction
probabilities_pca_qda <- predict(model_pca_qda, pca_test, type="response")
probabilities_pca_qda <- probabilities_pca_qda$posterior[,2]
preds_pca_qda <- ifelse(probabilities_pca_qda > threshold, "yes", "no")
preds_pca_qda <- as.factor(preds_pca_qda)

# evaluation
roc_score_pca_qda <- roc(test$y, probabilities_pca_qda)
plot(roc_score_pca_qda ,main =sprintf("ROC curve for QDA. AUC = %s", 
                              round(roc_score_pca_qda$auc, 2)))
confusionMatrix(data=preds_pca_qda, reference = test$y)

# oversampling

# modeling
model_pca_over_qda <- qda(y ~.-pdays-previous, data=pca_over_train)
model_pca_over_qda

# prediction
probabilities_pca_over_qda <- predict(model_pca_over_qda, pca_test, 
                                      type="response")
probabilities_pca_over_qda <- probabilities_pca_over_qda$posterior[,2]
preds_pca_over_qda <- ifelse(probabilities_pca_over_qda > threshold, 
                             "yes", "no")
preds_pca_over_qda <- as.factor(preds_pca_over_qda)

# evaluation
roc_score_over_qda <- roc(test$y, probabilities_pca_over_qda)
plot(roc_score_over_qda ,main =sprintf("ROC curve for QDA. AUC = %s", 
                              round(roc_score_over_qda$auc, 2)))
confusionMatrix(data=preds_pca_over_qda, reference = test$y)

# Naive Bayes
# modeling
pca_nb <- naiveBayes(y ~.-pdays-previous, data=pca_train)
pca_nb

# prediction
probabilities_pca_nb <- predict(pca_nb, pca_test, type = "raw")[,2]
preds_pca_nb <- predict(pca_nb, pca_test)

# evaluation
roc_score_pca_nb <- roc(test$y, probabilities_pca_nb)
plot(roc_score_pca_nb ,main =sprintf("ROC curve for Naive Bayes. AUC = %s", 
                              round(roc_score_pca_nb$auc, 2)))
confusionMatrix(data=preds_pca_nb, reference = test$y)

# oversampling
# modeling
pca_over_nb <- naiveBayes(y ~.-pdays-previous, data=pca_over_train)
pca_over_nb

# prediction
probabilities_pca_over_nb <- predict(pca_over_nb, pca_test, type = "raw")[,2]
preds_pca_over_nb <- predict(pca_over_nb, pca_test)

# evaluation
roc_score_pca_over_nb <- roc(test$y, probabilities_pca_over_nb)
plot(roc_score_pca_over_nb ,main =sprintf("ROC curve for Naive Bayes. AUC = %s", 
                              round(roc_score_pca_over_nb$auc, 2)))
confusionMatrix(data=preds_pca_over_nb, reference = test$y)

# KNN
# selecting k
scores <- c()
thresholds <- c()
ks <- seq(from = 5, to = 200, by = 20)
for (k in ks) {
  preds <- class::knn(train=X_val_train_mat, 
               test=X_val_val_mat, 
               cl = val_train_dummies_sc$y, k = k,
               prob = TRUE)
  probabilities <- attributes(preds)$prob
  score  <- roc(val_val_dummies_sc$y, probabilities)$auc
  scores <- c(scores, score)
}

k <- ks[scores == max(scores)][1]
plot(ks, scores, type='l')

# modeling
out.knn <- class::knn(train=X_train_mat, 
                     test=X_test_mat, 
                     cl = train_dummies_sc$y, k = k,
                     prob = TRUE)

# prediction
probabilities_knn <- attributes(out.knn)$prob
preds_knn <- ifelse(probabilities_knn > threshold, "yes", "no")
preds_knn <- factor(preds_knn, levels=c("no", "yes"))

# evaluation
roc_score_knn <- roc(test$y, probabilities_knn)
plot(roc_score_knn ,main =sprintf("ROC curve for K-NN. AUC = %s", 
                              round(roc_score_knn$auc, 2)))
confusionMatrix(data=preds_knn, reference = test$y)

# oversampling

# selecting k
scores <- c()
thresholds <- c()
ks <- seq(from = 5, to = 200, by = 20)
for (k in ks) {
  preds <- class::knn(train=X_val_train_over_mat, 
                      test=X_val_val_mat, 
                      cl = val_train_dummies_sc_over$y, k = k,
                      prob = TRUE)
  probabilities <- attributes(preds)$prob
  score  <- roc(val_val_dummies_sc$y, probabilities)$auc
  scores <- c(scores, score)
}

k <- ks[scores == max(scores)][1]
plot(ks, scores, type='l')

# modeling
out.knn_over <- class::knn(train=X_train_over_mat, 
                      test=X_test_mat, 
                      cl = train_dummies_sc_over$y, k = k,
                      prob = TRUE)

# prediction
probabilities_knn_over <- attributes(out.knn_over)$prob
preds_knn_over <- ifelse(probabilities_knn_over > threshold, "yes", "no")
preds_knn_over <- factor(preds_knn_over, levels=c("no", "yes"))

# evaluation
roc_score_knn_over <- roc(test$y, probabilities_knn_over)
plot(roc_score_knn_over ,main =sprintf("ROC curve for K-NN. AUC = %s", 
                              round(roc_score_knn_over$auc, 2)))
confusionMatrix(data=preds_knn_over, reference = test$y)

# Gather all results
roc_curve <- plot(roc_score_pca_fsel_lr, col = "blue")
roc_curve <- plot(roc_score_ridge, col = "red", add = TRUE)
roc_curve <- plot(roc_score_lasso, col = "green", add = TRUE)
roc_curve <- plot(roc_score_pca_lda, col = "cyan", add = TRUE)
roc_curve <- plot(roc_score_pca_qda, col = "black", add = TRUE)
roc_curve <- plot(roc_score_pca_nb, col = "yellow", add = TRUE)
roc_curve <- plot(roc_score_knn, col = "orange", add = TRUE)

col_legend <- c(sprintf("Logistic Regression. AUC = %s", 
                        round(roc_score_pca_fsel_lr$auc, 2)),
                sprintf("Ridge classifier. AUC = %s", 
                        round(roc_score_ridge$auc, 2)),
                sprintf("Lasso classifier. AUC = %s", 
                        round(roc_score_lasso$auc, 2)),
                sprintf("LDA. AUC = %s", 
                        round(roc_score_pca_lda$auc, 2)),
                sprintf("QDA. AUC = %s", 
                        round(roc_score_pca_qda$auc, 2)),
                sprintf("Naive Bayes. AUC = %s", 
                        round(roc_score_pca_nb$auc, 2)),
                sprintf("K-NN. AUC = %s", round(roc_score_knn$auc, 2)))

legend(x = "right",inset = 0,
       legend = col_legend, 
       col=c("blue", "red", "green", 
             "cyan", "black", "yellow",
             "orange"), lwd=7, cex=.7, horiz = FALSE)
