setwd("/Users/sunwoojung/downloads")

dat <- read.csv("hospital_mortality.csv", header = T)
ind <- which(colSums(is.na(dat))> 100)
temp <- na.omit(dat[,-ind])

full <- glm(outcome ~ ., data = temp, family = "binomial")
summary(full)


## VIF diagnosis ##
car::vif(full)


## variable selection ##
step <- step(full, direction="both")


## shrinkage models ##
library(glmnet)
library(ModelMetrics)

set.seed(105)
ind <- sample(1:nrow(temp), round(0.7*nrow(temp)))
train <- temp[ind,]
test <- temp[-ind,]

train_X <- as.matrix(train[,-3])
train_Y <- train$outcome
test_X <- as.matrix(test[,-3])
test_Y <- test$outcome

# simple LM
fit <- glm(outcome ~., data = train, family = "binomial")
auc1 <- auc(test$outcome, predict(fit, test))

# stepwise variable selection (AIC)
fit_step <- step(fit, direction = "both")
auc2 <- auc(test$outcome, predict(fit_step, test))

# ridge 
cv_out <- cv.glmnet(train_X, train_Y, alpha = 0, nfolds = 10, family = "binomial")
lamb <- cv_out$lambda.min
fit_ridge <- glmnet(train_X, train_Y, alpha = 0, lambda = lamb, family = "binomial")
y_pred <- predict(fit_ridge, test_X)
auc3 <- auc(test_Y, y_pred)


# LASSO
cv_out <- cv.glmnet(train_X, train_Y, alpha = 1, nfolds = 10, family = "binomial")
lamb <- cv_out$lambda.min
fit_lasso <- glmnet(train_X, train_Y, alpha = 1, lambda = lamb, family = "binomial")
y_pred <- predict(fit_lasso, test_X)
auc4 <- auc(test_Y, y_pred)

# elastic net
cv_out <- cv.glmnet(train_X, train_Y, alpha = 0.5, nfolds = 10, family = "binomial")
lamb <- cv_out$lambda.min
fit_elastic <- glmnet(train_X, train_Y, alpha = 0.5, lambda = lamb, family = "binomial")
y_pred <- predict(fit_elastic, test_X)
auc5 <- auc(test_Y, y_pred)

# results
models <- c("LM", "step", "ridge", "LASSO", "elastic_net")
auc_vec <- c(auc1, auc2, auc3, auc4, auc5)
auc_table <- data.frame(model = models, auc = auc_vec)
auc_table


## selected variables by step_AIC and LASSO
beta <- fit_lasso$beta
# by LASSO
names(beta[which(abs(beta[,1]) > 0.0000000000001),])
# by stepwise(AIC)
names(fit_step$coefficients)
