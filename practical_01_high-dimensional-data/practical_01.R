# practical 01 script
# by: Ward B. Eiling
# date: 14-11-2024

# load libraries
library(tidyverse)
library(glmnet)
library(caret)

##### Take Home Exercises #####

### 1. ------------------------------

# read data
gene_exp <- read_rds("practical_01_high-dimensional-data/data/gene_expressions.rds")
gene_exp <- as.data.frame(gene_exp)

# What are the dimensions of the data?
dim(gene_exp)[1] # rows
dim(gene_exp)[2] # columns
# thus, P > N

# What is the sample size?
length(unique(gene_exp$sample))

### 2. ------------------------------

# create histograms of the first six variables
firstsix <- gene_exp[,2:7]
coln <- colnames(firstsix)

par(mfrow = c(2,3))
for (i in 1:6) {
  hist(firstsix[,i], main = coln[i], xlab = "Values", col = "skyblue")
}

# Describe what you notice
# 1007_s_at/1053_at/121_at/1294_at: approximately normal distribution, a bit skewed
# 1255_g_at: log normal distribution or weibull distribution or poisson distribution or other.

### 3. ------------------------------

# load phenotypes.rds
phenotypes <- read_rds("practical_01_high-dimensional-data/data/phenotypes.rds") %>%
  select("sample", "disease")

# merge gene_exp and phenotypes
gene_exp_dis <- right_join(gene_exp, phenotypes, by = "sample")

### 4. ------------------------------

# Does this dataset suffer from class imbalance?
table(gene_exp_dis$disease)

# the dataset is quite balanced when it comes to normal/tumor samples
# However, the dataset may be imbalanced when it comes to the distributions
# on the different variables when split by disease status

### 5. ------------------------------

# Split the data into a training (80%) and a test set (20%). 
# We will use the training set for model development in the next section.

# recode disease into dummy variable where normal = 0 and tumor = 1
gene_exp_dis$disease_dum <- NA
gene_exp_dis$disease_dum[gene_exp_dis$disease == "normal"] <- 1
gene_exp_dis$disease_dum[gene_exp_dis$disease == "tumor"] <- 2
gene_exp_dis$disease_dum <- as.numeric(gene_exp_dis$disease_dum)

# split data
set.seed(123)
train_index <- sample(1:nrow(gene_exp_dis), 0.8*nrow(gene_exp_dis))
train <- gene_exp_dis[train_index,]
test <- gene_exp_dis[-train_index,]

##### Lab Exercises #####

### 6. ------------------------------

# use a correlation filter to find the IDs of the 
# 10 genes most related to disease status

# calculate correlation
genes_train <- train %>% select(-sample, -disease, -disease_dum)
cor_fil <- cor(genes_train, train$disease_dum)
cor_abs <- abs(cor_fil)

# retrieve the names of the greatest correlations
cor_greatest <- cor_abs %>% as.data.frame() %>% arrange(desc(cor_abs)) %>% head(10)
sel_genes <- rownames(cor_greatest)

### 7. ------------------------------

# perform logistic regression using the selected gens
train_sel_gens <- train %>% select(sel_genes, disease, disease)
train_sel_gens$disease <- as.factor(train_sel_gens$disease) # tumor becomes 2, normal 1
fit_lr <- glm(disease ~ ., data = train_sel_gens, family = "binomial")
summary(fit_lr)

# str(train_sel_gens)

### 8. ------------------------------

# create confusion matrix for predictions of the model
test_sel_gens <- test %>% select(sel_genes, disease, sample)
predictions_prob_test <- predict(fit_lr, newdata = test_sel_gens, type = "response")
predictions_df <- data.frame(sample = test_sel_gens$sample, observed = test_sel_gens$disease, test_pred_prob = predictions_prob_test)
predictions_df$test_pred <- NA
predictions_df$test_pred[predictions_df$test_pred_prob > 0.5] <- "tumor"
predictions_df$test_pred[predictions_df$test_pred_prob < 0.5] <- "normal"
predictions_df$test_pred <- as.factor(predictions_df$test_pred)
predictions_df$observed <- as.factor(predictions_df$observed)

# create confusion matrix
cor_log_reg_confusion <- confusionMatrix(predictions_df$test_pred, predictions_df$observed)

### 9. ------------------------------

x_train <- as.matrix(train %>%
                      select(-sample, -disease_dum, -disease))
y_train <- as.matrix(train %>%
                       select(disease) %>%
                       mutate(disease = as.factor(disease)))
x_test <- as.matrix(test %>%
                      select(-sample, -disease_dum, -disease))
y_test <- as.matrix(test %>%
                      select(disease) %>%
                      mutate(disease = as.factor(disease)))

### 10. ------------------------------

lasso <- glmnet(x_train, y_train, family = "binomial")
par(mfrow = c(1,1))
plot(lasso)
savePlot("p_lasso")

# We see the parameters associated with the different genes

### 11. ------------------------------

cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial")
plot(cv_lasso)

# the plot shows the binomial deviance, which is a predictive accuracy measure
# relative to the log of the hyperparameter lambda.
# we can see that the lowest deviance (so best predictive accuracy) 
# is found near a log(lambda) of about -4.9

### 12. ------------------------------

# inspect the non-zero coefficients of the model with lowest OOS deviance
cv_coef_SE <- as.data.frame(as.matrix(coef(cv_lasso, s = "lambda.1se"))) %>%
  filter(s1 != 0)
cv_coef_min <- as.data.frame(as.matrix(coef(cv_lasso, s = "lambda.min"))) %>%
  filter(s1 != 0)

### 13. ------------------------------

predicted_cv_lasso <- predict(
  object = cv_lasso,
  newx = x_test,
  s = "lambda.min",
  type = "response"
)

predicted <- ifelse(predicted_cv_lasso[,"lambda.min"] > 0.5, "tumor", "normal")
lasso_cv_confusion <- confusionMatrix(as.factor(predicted), reference = as.factor(y_test))

cor_log_reg_confusion # for the simple procedure, the accuracy was 91.67%
lasso_cv_confusion # for the LASSO with CV, the accuracy was higher at 95.83%

# I would say that in this instance, both methods yields reasonable results. 
# However, when relationships not (log) linear, the former method may miss
# important predictors, due to the use of correlations for variable selection.
# On the other hand, the lasso procedure is more robust against such things 
# as instead of performing feature/variable selection, we simply change the 
# parameter weights of the worst predictors towards zero.
