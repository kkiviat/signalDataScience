# sampling ------------
library(ggplot2)
library(Rmisc)
# returns data frame with two columns
# x: sample of size n from standard normal
# y: y = ax + error where error has s.d. b such that a^2 + b^2 = 1 (b = sqrt(1-a^2))
getSamples = function(a, n) {
  x = rnorm(n, 0, 1)
  y = a*x + rnorm(n, 0, sqrt(1-a^2))
  return(data.frame(x=x, y=y))
}
graphs = lapply(seq(0.1, 0.9, 0.1), 
                function(a) ggplot(getSamples(a, 100), aes(x, y)) + geom_point() 
                + ggtitle(paste("a =",a))
                + geom_smooth(method="lm"))
multiplot(plotlist=graphs, cols=2)

# estimateSlopes --------------
estimateSlopes = function(a, n, numTrials=500) {
  a_estimates = vector("double", numTrials)
  for (i in 1:numTrials) {
    df = getSamples(a, n)
    a_estimates[i] = coef(lm(y ~ x, data=df))[2]
  }
  return(a_estimates)
}
ggplot() + aes(estimateSlopes(0.4, 10000, 500)) + geom_histogram(binwidth=0.004)

# see how sd of estimates varies with a -----------
dfSD = data.frame(matrix(1:36, nrow=9))
colnames(dfSD) = c(100, 500, 2500, 10000)
rownames(dfSD) = seq(0.1, 0.9, 0.1)

for (a in rownames(dfSD)) {
  for (n in colnames(dfSD)) {
    dfSD[a, n] = sd(estimateSlopes(as.numeric(a), as.numeric(n)))
  }
}
dfSD
# sd gets lower as a and n increase


# see how sd of estimates varies with n for a = 0.1 -----------
dfSD2 = data.frame(1:20)
rownames(dfSD2) = seq(100, 2000, 100)
dfSD2

for (n in rownames(dfSD2)) {
  dfSD2[n, 1] = sd(estimateSlopes(0.1, as.numeric(n)))
}
dfSD2

ggplot() + aes(x=seq(100, 2000, 100), y=dfSD2$X1.20) + geom_line() + xlab("n") + ylab("sd of slope estimate")


# p-values ----------------
estimateSlopesWithPVals = function(a, n, numTrials=500) {
  a_estimates = vector("double", numTrials)
  p_vals = vector("double", numTrials)
  for (i in 1:numTrials) {
    df = getSamples(a, n)
    linear_model = lm(y ~ x, data=df)
    a_estimates[i] = coef(linear_model)[2]
    p_vals[i] = summary(linear_model)$coefficients["x", "Pr(>|t|)"]
  }
  return(data.frame(a=a_estimates, p=p_vals))
}
a = 0.15
df = estimateSlopesWithPVals(a, 500, 10000)
ggplot(df, aes(a, p)) + geom_point()
median(df$p)
mean(df$a <= 0)
