## GETTING STARTED ------------------------------------------------------------------------
## set seed
set.seed(1)

## Choose values of alpha and lambda to try
alphas = seq(0, 1, 0.1)
lambdas = 10^seq(1, -3, length.out=50)

## load data and fill in NAs with column means
library(psych)
df = msq
numeric_cols = sapply(df, is.numeric)
df[numeric_cols] = lapply(df[,numeric_cols],
                          function(x) ifelse(is.na(x), mean(unlist(x), na.rm=TRUE), x))

## get features from active to scornful
library(dplyr)
features = select(df, active:scornful)
extraversion = df$Extraversion
neuroticism = df$Neuroticism

## generate fold assignments
n_folds = 10
folds = sample(nrow(features)) %% n_folds + 1

## precompute the training and test sets for each fold
train_features = vector(mode="list", length=10)
test_features = vector(mode="list", length=10)
extraversion_targets = vector(mode="list", length=10)
neuroticism_targets = vector(mode="list", length=10)
for (fold in 1:n_folds) {
    train = scale(features[folds != fold,])
    test = scale(features[folds == fold,],
                 center=attr(train, 'scaled:center'),
                 scale=attr(train, 'scaled:scale'))
    train_features[[fold]] = train
    test_features[[fold]] = test
    extraversion_targets[[fold]] = extraversion[folds != fold]
    neuroticism_targets[[fold]] = neuroticism[folds != fold]
}

## create results data frame
results_df = data.frame()

## write rmse function
rmse = function(x, y) sqrt(mean((x-y)^2))

## GRID SEARCH ----------------------------------------------------------------------------
for (alpha in alphas) {
    cat(sprintf("alpha = %f\n", alpha))
    extraversion_models = vector(mode="list", length=n_folds)
    neuroticism_models = vector(mode="list", length=n_folds)
    ## train model for each fold
    for (fold in 1:n_folds) {
        extraversion_models[[fold]] = glmnet(x=train_features[[fold]],
                                             y=extraversion_targets[[fold]],
                                             alpha=alpha,
                                             lambda=lambdas)
        neuroticism_models[[fold]] = glmnet(x=train_features[[fold]],
                                            y=neuroticism_targets[[fold]],
                                            alpha=alpha,
                                            lambda=lambdas)
    }
    ## compute predictions and rmse for each lambda
    for (lambda in lambdas) {
        cat(sprintf("lambda = %f\n", lambda))
        extraversion_predictions = numeric(nrow(df))
        neuroticism_predictions = numeric(nrow(df))
        for (fold in 1:n_folds) {
            ## extraversion predictions
            model = extraversion_models[[fold]]
            test = test_features[[fold]]
            extraversion_predictions[folds == fold] = predict(model,
                                                              test,
                                                              s=lambda)
            ## neuroticism predictions
            model = neuroticism_models[[fold]]
            neuroticism_predictions[folds == fold] = predict(model,
                                                             test,
                                                             s=lambda)
        }
        ## compute rmse
        rmse_extraversion = rmse(extraversion_predictions, df$Extraversion)
        rmse_neuroticism = rmse(neuroticism_predictions, df$Neuroticism)
        ## add row to results data frame
        results_df = rbind(results_df, list(alpha=alpha, lambda=lambda,
                               rmse_extraversion=rmse_extraversion,
                               rmse_neuroticism=rmse_neuroticism))
    }
}
print(results_df)

## FINISHING UP ---------------------------------------------------------------------------
## function that returns index of vector v corresponding to its minimal value
arg_min = function(v) {
    ##which.min(v)
    if (length(v) < 1) return(NA)
    min_value = v[1]
    min_index = 1
    for (i in 1:length(v)) {
        if (v[i] < min_value) {
            min_value = v[i]
            min_index = i
        }
    }
    return(min_index)
}

## get rows with minimal RMSE
extraversion_min = results_df[arg_min(results_df$rmse_extraversion),]
neuroticism_min = results_df[arg_min(results_df$rmse_neuroticism),]

## train regularized models on the whole dataset with chosen alpha, lambda values
## extraversion
alpha = extraversion_min$alpha
lambda = extraversion_min$lambda
extraversion_model = glmnet(scale(features), extraversion, alpha=alpha, lambda=lambda)
## neuroticism
alpha = neuroticism_min$alpha
lambda = neuroticism_min$lambda
neuroticism_model = glmnet(scale(features), neuroticism, alpha=alpha, lambda=lambda)

## extract coefficients
coef_extraversion = coef(extraversion_model, s=extraversion_min$lambda)
coef_neuroticism = coef(neuroticism_model, s=neuroticism_min$lambda)

## bind coefficients into matrix
coef_matrix = cbind(as.numeric(coef_extraversion), as.numeric(coef_neuroticism))
coef_matrix = as.matrix(coef_matrix)
colnames(coef_matrix) = c("Extraversion", "Neuroticism")
rownames(coef_matrix) = rownames(coef_extraversion)
## remove top row (intercept)
coef_matrix = coef_matrix[-1,]
## remove rows where both coefficients are 0
coef_matrix = coef_matrix[coef_matrix[,1] != 0 & coef_matrix[,2] != 0,]

## get statistics on each column
extraversion_quantiles = quantile(abs(coef_matrix[,1]))
neuroticism_quantiles = quantile(abs(coef_matrix[,2]))
## remove rows where magnitudes of both coefficients are under 75th %ile for their column
extraversion_above_75 = abs(coef_matrix[,1]) >= extraversion_quantiles[['75%']]
neuroticism_above_75 = abs(coef_matrix[,2]) >= neuroticism_quantiles[['75%']]
coef_matrix = coef_matrix[extraversion_above_75 | neuroticism_above_75,]

## plot matrix of coefficients
library(corrplot)
corrplot(coef_matrix, is.corr=FALSE)

## Extraversion and neuroticism seem to be largely opposite, in that most
## features that positively predict one negatively predict the other. 
## The biggest difference seems to be confidence, which is a strong positive
## predictor for extraversion and a strong negative predictor for neuroticism.
## Sociability also strongly predicts extraversion while seemingly having little
## to say about neuroticism. Neuroticism is strongly predicted by loneliness, though.
