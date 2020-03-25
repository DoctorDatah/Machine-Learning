###################################################################################
#
# HW3.R
#
###################################################################################
# External Functions
###################################################################################
library(faraway)
library(Matrix)
library(ellipse)
###################################################################################
# Internal Functions
###################################################################################
# Processing 
###################################################################################
#Save the environment
parSave=par(no.readonly = TRUE)
###################################################################################
# Problem 3 
###################################################################################
data(prostate)
head(prostate)
?prostate
dim(prostate)
n=97; p=9; q=4
#a)
summary(prostate)
#svi min is 0, max is 1. So check the data.
prostate
#svi only takes the values 0 and 1, so change it to be a factor
prostate$svi=as.factor(prostate$svi)
summary(prostate)

#b)
lmod<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,prostate)
lmod<-lm(lpsa~.,prostate) #Same
lmod2<-lm(lpsa~lcavol+lweight+svi,prostate)
summary(lmod)
summary(lmod2)
coef(lmod)
coef(lmod2)
deviance(lmod)
deviance(lmod2)
df.residual(lmod)
df.residual(lmod2)

#c)
anova(lmod2,lmod)

#d)
(deviance(lmod2)-deviance(lmod))/deviance(lmod2)
#Extra: compare the R2 for both models
summary(lmod)$r.squared;summary(lmod2)$r.squared

#e)
fstat=anova(lmod2,lmod)[2,5]

#Create permutations for the variables not in lmod2 and calculate their F
nreps=4000
set.seed(123)
fstats<-numeric(nreps)
for (i in 1:nreps) {
  lm1<-update(lmod,lpsa~lcavol+lweight+sample(age)+sample(lbph)+svi+sample(lcp)+sample(gleason)+sample(pgg45),prostate)
  fstats[i]<-anova(lmod2,lm1)[2,5]
}
#Calculate the proportion larger than what we originally found
mean(fstats>fstat)

#f)
(tstat=(coef(summary(lmod2))["lcavol",1]-.7)/coef(summary(lmod2))["lcavol",2])
2*(1-pt(abs(tstat),n-q))

#g)
#We create the vector betai of length nb
set.seed(123)
nb=4000
betai<-rep(NA,nb)
resids<-residuals(lmod2)
preds<-fitted(lmod2)
for (i in 1:nb) {
  booty<-preds+sample(resids,rep=T)
  bmod<-update(lmod2,booty~.)
  betai[i]<-coef(bmod)[2]
}
quantile(betai,c(.025,.975))
#Assuming normality
confint(lmod2)[2,]

#h)
plot(ellipse(lmod2,c(3,4)),type="l",ylim=c(-.1,1.2),xlim=c(-.1,1))
#Add the individual CIs
abline(v=confint(lmod2)[3,],lty=2)
abline(h=confint(lmod2)[4,],lty=2)
points(coef(lmod2)[3],coef(lmod2)[4],pch=19) #Get the center
points(.4,.2,pch=19,col=2) #Get the point (.4,.2)

#i)
x0=c(1,1.3,3,1); names(x0)=names(coef(lmod2))
names(x0)[4]="svi"
crossprod(x0,coef(lmod2))
df=data.frame(t(x0))
df$svi=factor(df$svi)
predict(lmod2,new=df,interval="prediction")

#j)
predict(lmod2,new=df,interval="confidence",level=.99)

