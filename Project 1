# ============================================================
# MATH 4230 — In-Class Assignment 1
# Simple Linear Regression (SLR): Boston data
# ============================================================
# library(ISLR2)
library(dplyr)
library(tidyr)

# ---------------------------
# 0) Load data
# ---------------------------
data <- MASS::Boston

# ---------------------------
# 1) Split train / test + MSE
# ---------------------------
set.seed(123)

n <- nrow(data)
train_idx <- sample(seq_len(n), size = floor(0.70 * n))
train <- data[train_idx, ]
test  <- data[-train_idx, ]

mse <- function(y, yhat) mean((y - yhat)^2)

# ============================================================
# MODEL 1: medv ~ rm
# ============================================================

# Plot
plot(train$rm, train$medv,
     xlab = "rm (avg rooms)",
     ylab = "medv (median home value, $1000s)",
     main = "Model 1: medv vs rm (Train)")

# Fit + summary
m1 <- lm(medv ~ rm, data = train)
summary(m1)

# ANOVA (inference)
anova(m1)

# Test MSE (prediction performance)
mse1 <- mse(test$medv, predict(m1, newdata = test))
mse1

# ============================================================
# MODEL 2: medv ~ crim (handle outliers)
# ============================================================

# Plot (raw; shows outliers)
plot(train$crim, train$medv,
     xlab = "crim (crime rate)",
     ylab = "medv (median home value, $1000s)",
     main = "Model 2: medv vs crim (Train [raw])")

# Fit + summary (cleaned)
m1 <- lm(medv ~ crim, data = train)
summary(m1)

# Diagnostics (cleaned model)
par(mfrow = c(1, 2))
plot(m1, which = 1)
plot(m1, which = 2)
par(mfrow = c(1, 1))

# Boxplot (documents outliers)
boxplot(train$crim,
        horizontal = TRUE,
        xlab = "crim (crime rate)",
        main = "Training: Boxplot of crim")

# Outlier rule (1.5 * IQR) on TRAIN only
Q1 <- quantile(train$crim, 0.25)
Q3 <- quantile(train$crim, 0.75)
IQR_val <- IQR(train$crim)

lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val

train2 <- train %>% filter(crim >= lower, crim <= upper)

# Plot (cleaned train)
plot(train2$crim, train2$medv,
     xlab = "crim (crime rate)",
     ylab = "medv (median home value, $1000s)",
     main = "Model 2: medv vs crim (Train [clean])")

# Fit + summary (cleaned)
m2 <- lm(medv ~ crim, data = train2)
summary(m2)

# Diagnostics (cleaned model)
par(mfrow = c(1, 2))
plot(m2, which = 1)
plot(m2, which = 2)
par(mfrow = c(1, 1))

# ANOVA (inference) — for cleaned model
anova(m2)

# Test MSE (prediction performance) — test set unchanged
mse2 <- mse(test$medv, predict(m2, newdata = test))
mse2
# ============================================================
# MODEL 3: medv ~ log(lstat) (transformation)
# ============================================================

# Plot (often curved)
plot(train$lstat, train$medv,
     xlab = "lstat (%)",
     ylab = "medv (median home value, $1000s)",
     main = "Model 3: medv vs lstat (Train)")

# Create transformed variable
train <- train %>% mutate(log_lstat = log(lstat))
test  <- test  %>% mutate(log_lstat = log(lstat))

# Fit + summary
m3 <- lm(medv ~ log_lstat, data = train)
summary(m3)

# Predict + Test MSE
mse3 <- mse(test$medv, predict(m3, newdata = test))
mse3


# ============================================================
# Final comparison
# ============================================================

results <- tibble(
  Model = c("Model 1", "Model 2", "Model 3"),
  Predictor = c("rm", "crim (cleaned train)", "log(lstat)"),
  Test_MSE = c(mse1, mse2, mse3)
)

compare_raw_clean <-

# results
results %>% arrange(Test_MSE)
