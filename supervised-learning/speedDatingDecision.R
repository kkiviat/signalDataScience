library(dplyr)
library(glmnet)
library(pROC)
library(dummies)
df = read.csv("../datasets/speed-dating/speeddating-full.csv")

#Create data frame with decisions, average decision frequencies, careers and races
df = select(df, gender, iid, pid, wave, dec, attr, race, career_c)
genders = c("female", "male")
df$gender = factor(df$gender, labels = genders)
careers = c("Lawyer",
            "Academic",
            "Psychologist",
            "Doctor",
            "Engineer",
            "Creative",
            "Business",
            "RealEstate",
            "IntRelations",
            "Undecided",
            "SocialWork",
            "Speech",
            "Politics",
            "Athletics",
            "Other",
            "Journalism",
            "Architecture")
races = c("Black", "White", "Latino", "Asian", "Other")
# df$gender = factor(df$gender, labels = genders)
# df$race = factor(df$race, labels = races)
# df$career_c = factor(df$career_c, labels = careers)
agged = aggregate(df["dec"], df["iid"], FUN = mean, na.rm = T)

colnames(agged) = c("iid", "decAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("dec", "attr")], df["pid"], FUN = mean, na.rm = T)
colnames(agged) = c("pid", "decPartnerAvg", "attrPartnerAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("race", "career_c")], df["iid"], FUN = mean)
agged$race = factor(agged$race, labels = races)
agged$career_c = factor(agged$career_c, labels = careers)
names(agged)
df = inner_join(df[!(names(df) %in% c("race", "career_c"))], agged)
colnames(agged)[1:3] = c("pid", "race_Partner", "career_c_Partner")
df = inner_join(df, agged)



#Cross validate regularized logistic regression at the level of waves

crossValidate = function(features,
                         target,
                         waves = df$wave,
                         lambdas = (1.2)^(10:(-30)),
                         alphas = seq(0, 0.24, 0.03)){
  s = scale(features)
  s = s[,!is.nan(colSums(s))]
  rocs = expand.grid(lambda = lambdas, alpha = alphas)
  rocs$logLoss = 0
  rocs$ROC = 0
  for(alpha in alphas){
    print(alpha)
    l = lapply(1:21, function(wave){
      trainFeatures = s[waves != wave,]
      testFeatures = s[waves == wave,]
      set.seed(1); m = glmnet(trainFeatures, target[waves != wave],
                              alpha = alpha,
                              lambda = lambdas,
                              family = "binomial")
      as.data.frame(predict(m, testFeatures))
    })
    predictions = do.call(rbind, l)
    predictions = exp(predictions)/(1 + exp(predictions))
    rocTemp = sapply(predictions, function(cv){
      as.numeric(roc(target,cv)$auc)
    })
    rocs[rocs$alpha == alpha,"ROC"] = rocTemp[length(rocTemp):1]
  }
  rocs
}


## create data frames with dummy variables for race and career code
## first for participant making the decision
dums1 = dummy.data.frame(df[c('race', 'career_c')])

## then for partner being decided on
dums2 = dummy.data.frame(df[c('race_Partner', 'career_c_Partner')])

dums = cbind(dums1, dums2)

## create interaction terms
## race of decider x attractiveness of partner
races1 = colnames(dums1)[startsWith(colnames(dums1), "race")]
for (r1 in races1) {
    dums[paste(r1, 'attrPartnerAvg', sep=':')] = dums1[r1]*df['attrPartnerAvg']
}

## career of decider x attractiveness of partner
careers1 = colnames(dums1)[startsWith(colnames(dums1), "career")]
for (c1 in careers1) {
    dums[paste(c1, 'attrPartnerAvg', sep=':')] = dums1[c1]*df['attrPartnerAvg']
}

## race of decider x race of partner
races2 = colnames(dums2)[startsWith(colnames(dums2), "race")]
for (r1 in races1) {
    for (r2 in races2) {
        dums[paste(r1, r2, sep=':')] = dums1[r1]*dums2[r2]
    }
}

## career of decider x career of partner
careers2 = colnames(dums2)[startsWith(colnames(dums2), "career")]
for (c1 in careers1) {
    for (c2 in careers2) {
        dums[paste(c1, c2, sep=':')] = dums1[c1]*dums2[c2]
    }
}

## remove columns with 20 or fewer entries
length(dums[, colSums(dums) <= 20]) # 268 to be removed
dums = dums[, colSums(dums) > 20]

## form a features data frame by binding decAvg, decPartnerAvg, and attrPartnerAvg to dums
features = cbind(dums, select(df, decAvg, decPartnerAvg, attrPartnerAvg))


## use crossValidate() to find optimal values of alpha and lambda for predicting
## dec in terms of the features
## males
features_male = features[df$gender == 'male',]
target_male = df[df$gender == 'male', 'dec']
waves_male = df[df$gender == 'male', 'wave']
rocs_male = crossValidate(features_male, target_male, waves=waves_male)
optimal_male = rocs_male[which.max(rocs_male$ROC),]

## females
features_female = features[df$gender == 'female',]
target_female = df[df$gender == 'female', 'dec']
waves_female = df[df$gender == 'female', 'wave']
rocs_female = crossValidate(features_female, target_female, waves=waves_female)
optimal_female = rocs_female[which.max(rocs_female$ROC),]

## build models with chosen alpha/lambda
scale_male = scale(features_male)
scale_male = scale_male[,!is.nan(colSums(scale_male))]
male_model = glmnet(scale_male, target_male,
                    alpha=optimal_male$alpha,
                    lambda=optimal_male$lambda)

scale_female = scale(features_female)
scale_female = scale_female[,!is.nan(colSums(scale_female))]
female_model = glmnet(scale_female, target_female,
                    alpha=optimal_female$alpha,
                    lambda=optimal_female$lambda)

## inspect coefficients
coef(male_model)
coef(female_model)
