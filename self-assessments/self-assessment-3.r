######################################################################
## NATIONAL ELECTION STUDY
election = read.csv("../datasets/nat-elections/nes_cleaned_1992.csv")

## expand factor variables into dummies with the dummies package
library(dummies)
election_dummies = dummy.data.frame(election, sep="_")
## remove one dummy from each variable
for (col in colnames(election)[sapply(election, is.factor)]) {
    value = names(sort(table(election[col])))[1]
    dummyName = (paste(col, value, sep='_'))
    election_dummies[dummyName] = NULL
}

## restrict to people who voted for either the Republican or Democrat candidate
library(dplyr)
df_voted = election_dummies[election$vote=='yes' &
                            election$presvote %in% c('republican', 'democrat'),]

## Model the probability of a voter casting a vote for the republican candidate with glm
df_repub = select(df_voted, -presvote_democrat, -presvote_NA, -vote_yes)
model_repub = glm(presvote_republican ~ ., df_repub, family="binomial")

## order features by size in order of decreasing magnitude
feature_coef = model_repub$coefficients[-1] # remove intercept
feature_coef[order(abs(feature_coef), decreasing=TRUE)]

## compute area under ROC curve with pROC
library(pROC)
plot(roc(df_repub$presvote_republican, predict(model_repub, df_repub)))

## predict how non-voters would have voted
df_non_voters = election_dummies[election$vote=='no',]
## (using threshold of 0.5)
predict_non_voters = predict(model_repub, df_non_voters, type="response") > 0.5
cat(sprintf("percent of non-voters predicted to vote for Clinton: %.2f\n", (1-mean(predict_non_voters))*100))
cat(sprintf("percent of voters who actually voted for Clinton: %.2f\n", mean(df_voted$presvote_democrat)*100))


######################################################################
## NATIONAL MERIT TWIN STUDY
nmsqt = read.csv("../datasets/nmsqt-twin/NMSQT.csv")

## determine best values of alpha and lambda for modeling test score
## in terms of answers to questions
library(caret)
answers = select(nmsqt, starts_with("V"))
scores = nmsqt$NMSQT
## (ran several times with different lambda sequences)
param_grid = expand.grid(alpha=1:10*0.1,
                         lambda=2^seq(3, -1, length.out=10))
control = trainControl(method="repeatedcv", number=10,
                       repeats=2, verboseIter=TRUE)
caret_fit = train(answers, scores, method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
alpha = caret_fit$bestTune[['alpha']]
lambda = caret_fit$bestTune[['lambda']]

nmsqt_model = glmnet(scale(answers), scores, alpha=alpha, lambda=lambda)

## calculate the percent variance explained by this model (R^2)
nmsqt_predictions = predict(nmsqt_model, scale(answers), s=lambda)
1 - sum((nmsqt_predictions-scores)^2)/sum((scores-mean(scores))^2)

## bind the question text to the corresponding coefficients and look at
## the most predictive ones (largest magnitude of coef)
nmsqt_coef = coef(nmsqt_model, s=lambda)
question_text = read.csv("../datasets/nmsqt-twin/NMSQTcodebook.csv", stringsAsFactor=FALSE)
question_df = data.frame(coef=nmsqt_coef[-1], question=question_text$name)
head(question_df[order(abs(question_df$coef), decreasing=TRUE),],20)

## do PCA on the questions and plot the standard deviations
p_nmsqt = prcomp(answers, scale.=TRUE)
library(ggplot2)
ggplot() + geom_point(aes(1:ncol(answers),p_nmsqt$sdev)) +
    xlab("component") +
    ylab("standard deviation")

## zoom in to better see the early shape
ggplot() + geom_point(aes(1:50,p_nmsqt$sdev[1:50])) +
    xlab("component") +
    ylab("standard deviation")

## I will take the first 13 components, as it flattens out after that point

## oblique factor analysis with k=13
library(psych)
f_nmsqt = fa(scale(answers), nfactors=13, rotate="oblimin")
## bind the loadings to the question text for each factor
loadings_df = data.frame(f_nmsqt$loadings[,])
loadings = lapply(loadings_df, function(x) data.frame(x, question_text$name))
## look at the top questions for each factor
top_questions = lapply(loadings, function(x) head(x[order(x[1], decreasing=TRUE),2], 10))
top_questions[[1]] # introversion
top_questions[[2]] # mechanical_inclination
top_questions[[3]] # self_loathing
top_questions[[4]] # misanthropy
top_questions[[5]] # family_trouble
top_questions[[6]] # propriety
top_questions[[7]] # blaming_others
top_questions[[8]] # extraversion
top_questions[[9]] # intellectuality
top_questions[[10]] # integrity
top_questions[[11]] # organization
top_questions[[12]] # petulance
top_questions[[13]] # righteousness

## create a data frame of scores with factor labels
factor_score_df = data.frame(f_nmsqt$scores)
colnames(factor_score_df) = c('introversion', 'mechanical', 'self loathing', 'misanthropy', 'family trouble', 'propriety', 'blaming others', 'extraversion', 'intellectuality', 'integrity', 'organization', 'petulance', 'righteousness')
## scale each column so units are standard deviations
##score_df = data.frame(lapply(score_df, function(x) x/sd(x)))
factor_score_df = scale(factor_score_df) # same thing

## aggregate by gender
aggregate(factor_score_df, by=nmsqt['SEX'], FUN=mean)

## compute percent variance explained by additive genetic effects
## as 2 * (r_mz - r_dz) (r_mz correlation between members of identical
## twin pairs, r_dz correlation between members of fraternal twin pairs)
##
df_full = cbind(select(nmsqt, ID, ZYG, NMSQT), factor_score_df)
## sort by id to put twins together
df_full = df_full[order(df_full$ID),]

## separate identical and fraternal sets
df_ident = filter(df_full, ZYG=='identical')
df_frat = filter(df_full, ZYG=='fraternal')

## separate pairs into two lists
df_ident_1 = df_ident[seq(1, nrow(df_ident), 2),]
df_ident_2 = df_ident[seq(2, nrow(df_ident), 2),]
df_frat_1 = df_frat[seq(1, nrow(df_frat), 2),]
df_frat_2 = df_frat[seq(2, nrow(df_frat), 2),]

## define a function to compute the variance explained by additive
## genetic effects for a specified column
compute_AGE_explained_variance = function(col) {
    r_mz = cor(df_ident_1[,col], df_ident_2[,col])
    r_dz = cor(df_frat_1[,col], df_frat_2[,col])
    return(2*(r_mz-r_dz))
}

## apply it to all columns except the first two (ID and ZYG)
sapply(colnames(df_full[-(1:2)]), compute_AGE_explained_variance)

## this is essentially measuring how much more correlated identical
## twins are than fraternal twins. If an attribute were perfectly genetically
## determined, then r_mz should be 1, and r_dz should be about 0.5 since fraternal
## twins share about half their genes. Hence the score would be about 1.
## Introversion has the highest score by a good margin, suggesting that this
## is determined strongly by genetics, as opposed to environment. The mechanical
## and propriety attributes have very low scores, suggesting they may be more
## influenced by environment. 
