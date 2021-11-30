setwd("/Users/sunwoojung/downloads")
dat <- read.csv("framingham.csv", header = T)
temp <- na.omit(dat)

full <- glm(TenYearCHD ~ ., data = temp, family = "binomial")

# VIF diagnosis
car::vif(full)
reduced <- glm(TenYearCHD ~ . - cigsPerDay, data = temp, family = "binomial")
car::vif(reduced)
reduced <- glm(TenYearCHD ~ . - glucose, data = temp, family = "binomial")
car::vif(reduced)
reduced <- glm(TenYearCHD ~ . - sysBP - prevalentHyp, data = temp, family = "binomial")
car::vif(reduced)


# glucose, cigsPerDay, sysBP explain variations of others
reduced <- glm(TenYearCHD ~ . - glucose - cigsPerDay - sysBP, data = temp, family = "binomial")

# variable selection
step <- step(fit, direction="both")


# randomize currentSmoker and diabetes
set.seed(100)
ind_smok <- sample(1:nrow(temp), 1788)
ind_diab <- sample(1:nrow(temp), 99)

temp2 <- temp
temp2$currentSmoker[ind_smok] <- 1
temp2$currentSmoker[-ind_smok] <- 0
temp2$diabetes[ind_diab] <- 1
temp2$diabetes[-ind_diab] <- 0

rand <- glm(TenYearCHD ~ ., data = temp2, family = "binomial")
summary(rand)
rand <- glm(TenYearCHD ~ . - glucose - cigsPerDay, data = temp2, family = "binomial")
summary(rand)


# shrinkage models
library(glmnet)
rmse <- function(x,y) {sqrt(mean((x-y)^2))}
set.seed(100)
ind <- sample(1:nrow(temp), round(0.7*nrow(temp)))
train <- temp[ind,]
test <- temp[-ind,]

train_X <- as.matrix(train[,-16])
train_Y <- train$TenYearCHD
test_X <- as.matrix(test[,-16])
test_Y <- test$TenYearCHD

# simple LM
fit <- lm(TenYearCHD ~., data = train)
r1 <- rmse(predict(fit, test), test$TenYearCHD)

# stepwise variable selection (AIC)
fit_step <- step(fit, direction = "both")
r2 <- rmse(predict(fit_step, test), test$TenYearCHD)

# ridge 
cv_out <- cv.glmnet(train_X, train_Y, alpha = 0, nfolds = 10)
lamb <- cv_out$lambda.min
fit_ridge <- glmnet(train_X, train_Y, alpha = 0, lambda = lamb)
y_pred <- predict(fit_ridge, test_X)
r3 <- rmse(y_pred, test_Y)

# LASSO
cv_out <- cv.glmnet(train_X, train_Y, alpha = 1, nfolds = 10)
lamb <- cv_out$lambda.min
fit_lasso <- glmnet(train_X, train_Y, alpha = 1, lambda = lamb)
y_pred <- predict(fit_lasso, test_X)
r4 <- rmse(y_pred, test_Y)

# elastic net
cv_out <- cv.glmnet(train_X, train_Y, alpha = 0.5, nfolds = 10)
lamb <- cv_out$lambda.min
fit_elastic <- glmnet(train_X, train_Y, alpha = 0.5, lambda = lamb)
y_pred <- predict(fit_elastic, test_X)
r5 <- rmse(y_pred, test_Y)

models <- c("LM", "step", "ridge", "LASSO", "elastic_net")
rmse_vec <- c(r1, r2, r3, r4, r5)
rmse_table <- data.frame(model = models, rmse = rmse_vec)
