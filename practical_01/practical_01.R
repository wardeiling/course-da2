# practical 01 script
# by: Ward B. Eiling
# date: 14-11-2024

# load libraries
library(tidyverse)
library(glmnet)

##### Take Home Exercises #####

### 1. ------------------------------

# read data
gene_exp <- read_rds("practical_01/data/gene_expressions.rds")
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

par(mfrow = c(2,3))
for (i in 1:6) {
  hist(firstsix[,i], main = coln[i], xlab = "Values", col = "skyblue")
}

# Describe what you notice
# 1007_s_at/1053_at/121_at/1294_at: approximately normal distribution, a bit skewed
# 1255_g_at: log normal distribution or weibull distribution or poisson distribution or other.

### 3. ------------------------------

# load phenotypes.rds
phenotypes <- read_rds("practical_01/data/phenotypes.rds") %>%
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
gene_exp_dis$disease_dum[train$disease == "normal"] <- 1
gene_exp_dis$disease_dum[train$disease == "tumor"] <- 2
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


