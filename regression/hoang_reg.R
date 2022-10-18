library(gridExtra)
library(ggplot2)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(glmnet)
library(gam)

setwd("/home/hoangkplog/Projects/UTC/SY19/Git_TP4_SY19")
rm(list = ls())

############# REGRESSION ####################

##### 1.EXPLORATION
reg.data <- read.table(file = "data/TPN1_a22_reg_app.txt", header = TRUE)
head(reg.data)

summary.data <- as.data.frame(apply(reg.data, 2, summary))
# toutes les variables de X1 à X100 valent entre 0 & 10
# & ont une moyenne autour de 5 / y est compris entre -5 & 371
# & a une moyenne à 165
t(summary.data)
n <- nrow(reg.data)
p <- ncol(reg.data) - 1 # minus 1 because we exclude y (the response variable)

# Median plot
# matplot(t(summary.data[3, -length(summary.data)]),
#     type = "l",
#     main = "Median",
#     ylab = "value"
# )
# TODO: min-max candle plot
# PCA exploration
pca <- prcomp(reg.data, scale = TRUE, center = TRUE, retx = T)
pca.var <- pca$sdev^2

prop_var <- pca.var / sum(pca.var)
# plot(prop_var,
#     xlab = "principal component",
#     ylab = "Proportion of Variance Explained",
#     ylim = c(0, 1),
#     type = "b",
# )
# We notice that all of PCA components contribution are almost the same
# + very low correlation among variables
# => Cannot use PCA to reduce dimension!


######### 2. DATA PREPARATION
# Generation of 2/3 observations
rows <- nrow(reg.data)
cols <- ncol(reg.data) - 1
train_size <- 2 / 3
nb_train <- round(train_size * rows)
nb_test <- rows - nb_train

# seed
# set.seed(1729) # the Hardy–Ramanujan number
set.seed(1)

# Training/Testing data
train <- sample(1:rows, nb_train)
reg.train <- reg.data[train, ]
reg.test <- reg.data[-train, ]

# Normalisation
reg.train[1:100] <- scale(reg.train[1:100])
reg.test[1:100] <- scale(reg.test[1:100])



########### 3. MODEL TRAINING + MODEL SELECTION

MSE <- function(y_test, y_predict) {
    mean((y_test - y_predict)^2)
}

# 10-folds
kfolds <- 10
n <- nrow(reg.data)
p <- ncol(reg.data)
ntst <- n / kfolds

## Define fold_ids
fold_ids <- rep(seq(kfolds), ceiling(n / kfolds))
fold_ids <- fold_ids[1:n]
fold_ids <- sample(fold_ids, length(fold_ids))

## Initialize vectors to store CV errors
reg_cv_mse <- vector(length = kfolds, mode = "numeric")
fwd_cv_mse <- vector(length = kfolds, mode = "numeric")
bwd_cv_mse <- vector(length = kfolds, mode = "numeric")
elastic_cv_mse <- vector(length = kfolds, mode = "numeric")
lasso_cv_mse <- vector(length = kfolds, mode = "numeric")
ridge_cv_mse <- vector(length = kfolds, mode = "numeric")
full_cv_mse <- vector(length = kfolds, mode = "numeric")

# 10-CV execution
train.control <- trainControl(method = "cv", number = 10)
for (k in 1:kfolds) {

    ########## FORWARD STEPWISE SELECTION + REGRESSION #############
    fwd.model <- train(y ~ .,
        data = reg.data[which(fold_ids != k), ],
        method = "leapForward",
        tuneGrid = data.frame(nvmax = 1:100),
        trControl = train.control
    )
    # Save best set of predictors
    fwd_coef <- coef(fwd.model$finalModel, fwd.model$bestTune$nvmax)
    fwd.pred.names <- names(fwd_coef)[1:fwd.model$bestTune$nvmax + 1]

    # we generate a linear regression, with the best subset of predictors found,
    # on the current K-1 folds and then we evaluate it with the test fold k
    combined.fwd.pred.names <- paste(fwd.pred.names, collapse = " + ")
    fwd.model.linreg <- lm(paste("y", "~", combined.fwd.pred.names),
        data = reg.data[which(fold_ids != k), ]
    )
    preds <- predict(fwd.model.linreg,
        newdata = reg.data[which(fold_ids == k), ]
    )
    # Model prediction performance
    fwd_cv_mse[k] <- MSE(reg.data[which(fold_ids == k), p], preds)

    ########## BACKWARD STEPWISE SELECTION + REGRESSION #############
    bwd.model <- train(y ~ .,
        data = reg.data[which(fold_ids != k), ],
        method = "leapBackward",
        tuneGrid = data.frame(nvmax = 1:100),
        trControl = train.control
    )
    bwd_coef <- coef(bwd.model$finalModel, bwd.model$bestTune$nvmax)
    bwd.pred.names <- names(bwd_coef)[1:bwd.model$bestTune$nvmax + 1]
    combined.bwd.pred.names <- paste(bwd.pred.names, collapse = " + ")
    bwd.model.linreg <- lm(paste("y", "~", combined.bwd.pred.names),
        data = reg.data[which(fold_ids != k), ]
    )
    preds <- predict(bwd.model.linreg,
        newdata = reg.data[which(fold_ids == k), ]
    )
    bwd_cv_mse[k] <- MSE(reg.data[which(fold_ids == k), p], preds)

    ########################## PENALIZED REGRESSION #########################
    x <- model.matrix(y ~ ., reg.data[which(fold_ids != k), -p - 1])[, -1]
    # model.matrix() allows to automatically transform any qualitative variables
    # (if any) into dummy variables , which is important because glmnet() can
    # only take numerical, quantitative inputs.
    # After creating the model matrix, we remove the intercept component at
    # index equal to 1
    y <- reg.data[which(fold_ids != k), ]$y
    x.test <- model.matrix(y ~ ., reg.data[which(fold_ids == k), -p - 1])[, -1]

    ########## ELASTICNET REGRESSION #############
    # The elastic net regression is easily computed using the caret workflow,
    # which invokes the glmnet package. We use caret to automatically select
    # the best tuning parameters λ and α.
    # The caret packages tests a range of possible alpha and lambda values,
    # then selects the best values for λ and α, resulting to a final model that
    # is an elastic net model.
    # Here, we’ll test the combination of 10 different values λ and α.
    # This is specified using the option tuneLength.
    # The best λ and α values are those values that minimize the
    # 10-cross-validation error here:
    # => Find best tuning parameters through a 10-CV on the current K-1 folds
    elastic <- train(y ~ .,
        data = reg.data[which(fold_ids != k), ],
        method = "glmnet",
        trControl = train.control,
        tuneLength = 10
    )
    # (the best tuning parameter are saved in elastic$bestTune)
    elastic.alpha <- elastic$bestTune[1, 1]
    elastic.lambda <- elastic$bestTune[1, 2]

    # Train model
    fit.elsatic <- glmnet(x, reg.data[which(fold_ids != k), p],
        lambda = elastic.lambda,
        alpha = elastic.alpha
    )
    # Model performance
    preds <- predict(fit.elsatic, x.test)
    elastic_cv_mse[k] <- MSE(reg.data[which(fold_ids == k), p], preds)

    ########## RIDGE REGRESSION #############
    cv.ridge <- cv.glmnet(x, y, alpha = 0)
    fit.ridge <- glmnet(x, reg.data[which(fold_ids != k), p],
        lambda = cv.ridge$lambda.min, alpha = 0
    )
    # Model performance
    preds <- predict(fit.ridge, x.test)
    ridge_cv_mse[k] <- MSE(reg.data[which(fold_ids == k), p], preds)

    ########## LASSO REGRESSION #############
    cv.lasso <- cv.glmnet(x, y, alpha = 1)
    fit.lasso <- glmnet(x, reg.data[which(fold_ids != k), p],
        lambda = cv.lasso$lambda.min,
        alpha = 1
    )
    predictions <- predict(fit.lasso, x.test)
    lasso_cv_mse[k] <- MSE(reg.data[which(fold_ids == k), p], predictions)

    # REGRESSION OBTAINED W/ THE FULL MODEL (NO SELECTION)
    model.linreg <- lm(y ~ ., data = reg.data[which(fold_ids != k), ])
    preds <- predict(model.linreg, newdata = reg.data[which(fold_ids == k), ])
    # Model prediction performance
    full_cv_mse[k] <- MSE(reg.data[which(fold_ids == k), p], preds)
}

sprintf("forward step regression model MSE: %.3f", mean(fwd_cv_mse))
sprintf("backward step regression MSE: %.3f", mean(bwd_cv_mse))
sprintf("elastic net model MSE: %.3f", mean(elastic_cv_mse))
sprintf("lasso net model MSE: %.3f", mean(lasso_cv_mse))
sprintf("ridge model MSE: %.3f", mean(ridge_cv_mse))
sprintf("full model regression MSE: %.3f", mean(full_cv_mse))


##### 5. MODEL EVALUATION

modelDiagnostic <- function(model.reg) {
    pMod <- fortify(model.reg)

    # The following is a plot of the residuals against the predicted values
    # for the data set.
    p1 <- ggplot(pMod, aes(x = .fitted, y = .resid)) +
        geom_point() +
        geom_smooth(se = FALSE) +
        geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
        xlab("Fitted Values") +
        ylab("Residuals") +
        ggtitle("Residual vs Fitted Plot")

    # The following is a bunch of extra code to get around ggplot not being able
    # to automatically draw a normal distribution line on a QQ plot.
    pMod$.qqnorm <- qqnorm(pMod$.stdresid, plot.it = FALSE)$x
    y <- quantile(pMod$.stdresid, c(0.25, 0.75))
    x <- quantile(pMod$.qqnorm, c(0.25, 0.75))
    slope <- diff(y) / diff(x) # line slope
    int <- y[1] - slope * x[1] # line intercept

    # Residuals QQ plot
    p2 <- ggplot(pMod, aes(.qqnorm, .stdresid)) +
        geom_point(na.rm = TRUE) +
        geom_abline(intercept = int, slope = slope, color = "red") +
        xlab("Theoretical Quantiles") +
        ylab("Standardized Residuals") +
        ggtitle("Normal Q-Q Plot")

    # Residuals histogram plot
    p3 <- ggplot(data = pMod, aes(x = .resid)) +
        geom_histogram(binwidth = 0.5, fill = "blue") +
        xlab("Residuals") +
        ggtitle("Distribution of Residuals")

    grid.arrange(p1, p3, p2, nrow = 1, top = "Model Diagnostic Plots")
}

modelObservations <- function(model.reg, data.test) {
    # plotting ytest and ypred
    ypred <- predict(model.reg, data.test)
    plot(data.test$y, ypred,
        main = "predicted values versus test values",
        xlab = "predicted y",
        ylab = "test y"
    )
    abline(0, 1)

    # getting the observations outside the prediction interval
    ic.error.bar <- function(x, lower, upper, length = 0.1) {
        arrows(x, upper, x, lower, angle = 90, code = 3, length = length)
    }

    # Calculate the prediction intervals of y0 (the predicted value)
    # with the predict command using the interval = "prediction" option
    pred.test.ip <- predict(model.reg,
        interval = "prediction",
        newdata = data.test
    )
    ytest <- data.test$y

    # get the observations being outside the prediction interval
    idx.obs.out.ip <- which((ytest < pred.test.ip[, 2]) |
        (ytest > pred.test.ip[, 3]))
    # We observe whether these intervals can contain the truth y within them
    # (the observed values), i.e. whether the uncertainties associated with
    # the estimates and/or due to the risk of sampling error
    # (i.e. a set of individuals representative of the population)
    # do not affect our predictions.

    # Plotting the graph with the prediction intervals.
    plot(pred.test.ip[, 1], ytest,
        pch = 19, col = "blue", ylim = range(pred.test.ip, ytest),
        main = " predicted values and the 95% PI versus the observed values.",
        xlab = "predicted y",
        ylab = "test y"
    )
    abline(0, 1)
    ic.error.bar(pred.test.ip[, 1], pred.test.ip[, 2], pred.test.ip[, 3])
    points(pred.test.ip[idx.obs.out.ip, 1], ytest[idx.obs.out.ip],
        pch = 19, col = "red"
    )

    # display:
    sprintf("number of observed values outside the prediction interval among %d,
			observed values in total: %d", length(ytest), length(idx.obs.out.ip))
    # returns the percentage that the new observation in the prediction interval
    1 - (length(idx.obs.out.ip) / length(ytest))
}

# Select forward step model because it yields lowest MSE
fwd.model <- train(y ~ .,
    data = reg.train,
    method = "leapForward",
    tuneGrid = data.frame(nvmax = 1:100),
    trControl = train.control
)
# Best set of predictors
fwd.coefs <- coef(fwd.model$finalModel, fwd.model$bestTune$nvmax)
fwd.pred.names <- names(fwd.coefs)[1:fwd.model$bestTune$nvmax + 1]

# Fit linear regression, with the above predictors
fwd.model.linreg <- lm(paste("y", "~", paste(fwd.pred.names, collapse = " + ")),
    data = reg.train
)
summary(fwd.model.linreg)
ypred <- predict(fwd.model.linreg, newdata = reg.test)

# MSE
sprintf("Best model MSE: %.3f", MSE(reg.test$y, ypred))

# Model evaluation
modelDiagnostic(fwd.model.linreg)
pct_obs <- modelObservations(fwd.model.linreg, reg.test)
