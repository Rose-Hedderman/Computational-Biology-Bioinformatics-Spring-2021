# Question 1
#install.packages("boot")
library(boot)
library(dplyr)

urine <- na.omit(urine) %>% 
  mutate_at(-1, function(x)x-mean(x))
head(urine)

urine %>% 
  count(r)
tot = 34 / (34+45)
tot


# Question 2
glm(r ~ (.)^2, data = urine, family = "binomial") 

# Question 3

logodds <- predict(fit, type = "link")
length(logodds)
12/32

# Question 4

performance <-  data.frame(
  probs <- predict(fit, type = "response"),
  predicted <- ifelse(probs > 0.5, 1, 0),
  truth <- urine$r
)
performance
table(true_condition = performance$truth, 
      predicted_condition = performance$predicted) %>% 
  addmargins


# Question 5
library(plotROC)
ROCplot <- ggplot(performance) + 
  geom_roc(aes(d = truth, m = probs), n.cuts = 0)

AUC <- calc_auc(ROCplot)$AUC
AUC
# Question 6
set.seed(123)

k = 10 

# Randomly order rows in the dataset
data <- urine[sample(nrow(urine)), ] 

# Create k folds from the dataset
folds <- cut(seq(1:nrow(data)), breaks = k, labels = FALSE) 

# Use a for loop to get diagnostics for each test set
diags_k <- NULL
for(i in 1:k){
  # Create training and test sets
  train <- data[folds != i, ] # all observations except in fold i
  test <- data[folds == i, ]  # observations in fold i
  
  # Train model on training set (all but fold i)
  fit <- glm(r ~ (.)^2, data = train, family = "binomial")
  
  # Test model on test set (fold i)
  performance <- data.frame(
    probs = predict(fit, newdata = test, type = "response"),
    truth = test$r
  )
  
  # Consider the ROC curve for the test dataset
  ROCplot <- ggplot(performance) + geom_roc(aes(d = truth, m = probs), n.cuts = 0)
  
  # Get diagnostics for fold i (AUC)
  diags_k[i] <- calc_auc(ROCplot)$AUC
}

# Resulting diagnostics for each of the k folds
diags_k

# Average performance 
mean(diags_k)

# Question 7

# Question 9
install.packages("glmnet")
library(glmnet)
set.seed(123)
urine1 <- urine %>%
  na.omit %>% 
  as.matrix
cv.lasso1 <- cv.glmnet(x = urine1[ ,-1], 
                       y = urine1[ ,1], 
                       family = "binomial")
lasso1 <- glmnet(x = urine1[ ,-1], 
                 y = urine1[ ,1], 
                 family = "binomial", 
                 alpha = 1, 
                 lambda = cv.lasso1$lambda.1se)

coef(lasso1)
