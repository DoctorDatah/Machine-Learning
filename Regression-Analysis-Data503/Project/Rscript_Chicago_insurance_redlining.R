# Rscript to reproduce results in Faraway Ch 12
# Chicago insurance redlining 

# Brief explanation of the data (source: Andrews and Herzberg (1985))
# Several communities in Chicago claimed that insurance companies 
#    were redlining their neighborhoods.
# Redlining = canceling insurance policies or refusing to renew.

# Data is given by zipcode:
#    race: racial decomposition in percent minority
#    fire: fires per 100?? housing units in 1975
#    theft: thefts per 1000 people in 1975
#    age: percent of housing units built before 1939
#    involact: new FAIR plan policies and renewals per 100 housing units
#              in the first half of 1978 (FAIR plan policies are mostly 
#              given after normal insurance is denied).
#    income: median family income

# Dependent variable = involact
# We will focus on the relationship between race and involact
# In particular: is there discrimination?

# remove all objects
rm(list=ls(all=TRUE))

#############
# read data #
#############

library(faraway)
data(chicago)

# remove variable 'volact' and rescale 'income'
ch <- data.frame(chicago[,1:4],involact=chicago[,6],
                 income=chicago[,7]/1000)
summary(ch)

# note high spread in 'race' -> good for our analysis


###############
# exploration #
###############

# histograms
par(mfrow=c(2,3))
for (i in 1:6){
  hist(ch[,i], main=names(ch)[i],xlab=names(ch)[i],prob=T)
  lines(density(ch[,i]))
}

# boxplots
par(mfrow=c(2,3))
for (i in 1:6){
  boxplot(ch[,i],main=names(ch)[i])
}

# dependent variable versus each independent variable
par(mfrow=c(2,3))
for (i in c(seq(1,4),6)){
  plot(ch[,i],ch[,5],xlab=names(ch)[i],ylab=names(ch)[5])
  lines(loess.smooth(ch[,i],ch[,5]))
}
# we'll consider log2(income) because of better interpretation:
plot(log2(ch[,6]),ch[,5],xlab="log2(income)",ylab=names(ch)[5])
lines(loess.smooth(log2(ch[,6]),ch[,5]))

# compute correlations
ch.new <- ch
ch.new[,6] <- log2(ch[,6])
names(ch.new)[6] <- "log2(income)"
round(cor(ch.new),2)


##########################
# full model diagnostics #
##########################

m <- lm(involact ~ race + fire + theft + age + log2(income), data=ch)
summary(m)
# race is very highly significant (p<0.001)

par(mfrow=c(2,3))
plot(m, which=c(1:5))

# ZIP codes 60610, 60607 (and perhaps 60621) stick out.
# these are cases 6, 24 (and 35)
# what is special about these observations?

# histograms, with value of ZIP code 60610 indicated:
par(mfrow=c(2,3))
for (i in 1:6){
  hist(ch[,i], main=c(names(ch)[i],"zip code 60610"), 
       xlab=names(ch)[i], prob=T)
  lines(density(ch[,i]))
  abline(v=ch[6,i],col="blue",lwd=2)
}

# histograms, with value of ZIP code 60607 indicated:
par(mfrow=c(2,3))
for (i in 1:6){
  hist(ch[,i], main=c(names(ch)[i],"zip code 60607"), 
       xlab=names(ch)[i], prob=T)
  lines(density(ch[,i]))
  abline(v=ch[24,i],col="blue",lwd=2)
}

# histograms, with value of ZIP code 60621 indicated:
par(mfrow=c(2,3))
for (i in 1:6){
  hist(ch[,i], main=c(names(ch)[i],"zip code 60621"), 
       xlab=names(ch)[i], prob=T)
  lines(density(ch[,i]))
  abline(v=ch[35,i],col="blue",lwd=2)
}


########################################
# remove the 60610 and 60607 zip codes #
########################################

m <- lm(involact ~ race + fire + theft + age + log2(income), 
        data=ch, subset=c(1:47)[-c(6,24)])
par(mfrow=c(3,2))
plot(m, which=c(1:5))

# what is a reasonable cut-off for Cook's distance?
# 4/(n-k-1):
4/(45-5-1)
# there are still influential cases, but we'll keep the rest of the data

# note only fire is very highly significant now, 
# race is less stong than before, but still significant (p<0.05)
summary(m)


### quadratic terms? ###

# should we include quadratic terms?
# we don't transform involact and race to keep clear interpretation
m.quad <- lm(involact ~ race + poly(fire,2) + poly(theft,2) + poly(age,2) 
                        + poly(log2(income),2), data=ch, 
                        subset=c(1:47)[-c(6,24)])
anova(m, m.quad)
# p-value insignficant -> we do not reject the null hypothesis that model
# without quadratic terms is OK. 


### variable selection ###

# find the model with the "best" AIC score:
step(m)
# the model includes race, fire, theft and age

# find the model with the "best" BIC score:
step(m, k=log(45))
# the model just includes race and fire

# find the model with the best adjusted R^2 score:
y <- ch$involact[-c(6,24)]
x <- cbind(ch[,1:4],"log2(income)"=log2(ch[,6]))
x <- x[-c(6,24),]

library(leaps)
library(faraway)

m <- leaps(x,y,method="adjr2")
ind <- order(m$adjr2, decreasing=T)
which <- m$which[ind,]
nr <- nrow(which)
nc <- ncol(which)

par(mfrow=c(1,1))
image(c(1:nr),c(1:nc),which,col=c("white","black"),
	xlab="Models, ordered by adjusted R^2",ylab="Predictors")
# the best model includes race, fire, theft and age

maxadjr(m,20)

# find best model according to Mallows Cp criterion:
m <- leaps(x,y,method="Cp")
par(mfrow=c(1,1))
Cpplot(m)
# 1,2,3,4 (race, fire, theft, age) seems best
# 1,2,4 (race, fire, age) is also pretty good

# so we have several competing models. Three of them are:
# m0: involact ~ race + fire  (BIC)
# m1: involact ~ race + fire + age (Runner up in Cp)
# m2: involact ~ race + fire + theft + age  (AIC, R^2, Cp)

# let's take a look at them:
m0 <- lm(involact ~ race + fire, data=ch, subset=c(1:47)[-c(6,24)])
summary(m0)
m1 <- lm(involact ~ race + fire + age, data=ch, subset=c(1:47)[-c(6,24)])
summary(m1)
m2 <- lm(involact ~ race + fire + theft + age, 
         data=ch, subset=c(1:47)[-c(6,24)])
summary(m2)

# race is significant in all of them

# there are some highly influential observations
par(mfrow=c(3,2))
plot(m0, which=c(1:5))
par(mfrow=c(3,2))
plot(m1, which=c(1:5))
par(mfrow=c(3,2))
plot(m2, which=c(1:5))

# take out zip codes 60612, 60621, 60653 (cases 23,30,35):
m0 <- lm(involact ~ race + fire, data=ch, 
         subset=c(1:47)[-c(6,23,24,30,35)])
summary(m0)
m1 <- lm(involact ~ race + fire + age, data=ch, 
         subset=c(1:47)[-c(6,23,24,30,35)])
summary(m1)
m2 <- lm(involact ~ race + fire + theft + age, 
         data=ch, subset=c(1:47)[-c(6,23,24,30,35)])
summary(m2)

# what happens if we use all observations?
m0 <- lm(involact ~ race + fire, data=ch)
summary(m0)
m1 <- lm(involact ~ race + fire + age, data=ch)
summary(m1)
m2 <- lm(involact ~ race + fire + theft + age, data=ch)
summary(m2)

# what can we conclude?
# there does seem to be a relationship between race and involact, while 
# controlling for a subset of the other variables. 

# possible critique on this analysis:
# - involact is not a perfect measure of insurance redlining
# - the size of the effect is not very large. the largest value of the 
#   response is only 2.2 per 100. if there is discrimination, relatively
#   few people are affected.
# - there may be a hidden variable that causes the correlation between 
#   race and involact. i can't think of a very likely candidate though.
# - we have aggregate data (zip-codes). hence we cannot draw conclusions
#   about individual houses/families in the zip-codes (ecological fallacy)
