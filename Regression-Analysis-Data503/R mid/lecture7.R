###################################################################################
#
# Lecture7.R
#
###################################################################################
# External Functions
###################################################################################
#install.packages("faraway")
#install.packages("ggplot2")
#install.packages("ellipse")
library(faraway)
library(ggplot2)
library(ellipse)
###################################################################################
# Internal Functions
###################################################################################
###################################################################################
# Processing 
###################################################################################
#Save the environment
parSave=par(no.readonly = TRUE)
###################################################################################
# Hypothesis tests review
###################################################################################
set.seed(10)
x=-50:50/10
#Plot the normal distribution
plot(x,dnorm(x),type='l')
#Obtain 100 observations from the normal distribution
y=rnorm(100)
#Plot the density for those observations in red
lines(density(y),col=2)
summary(y)

#P(X>c) when X~norm(0,1) (p-value for 1-sided hypothesis test)
c=1.5
plot(x,dnorm(x),type='l')
abline(v=c)
arrows(c,-.01,5,-.01,col=2,.1,15)
1-pnorm(c)

#P(X<-|c| or X>|c|) when X~norm(0,1) (p-value for 2-sided hypothesis test)
c=1.5
plot(x,dnorm(x),type='l')
abline(v=c(-abs(c),abs(c)))
arrows(-abs(c),-.01,-5,-.01,col=2,.1,15)
arrows(abs(c),-.01,5,-.01,col=2,.1,15)
pnorm(-abs(c))+1-pnorm(abs(c))
2*(1-pnorm(abs(c)))

#t-distribution vs. normal
df1=1;df2=100
plot(x,dnorm(x),type='l')
lines(x,dt(x,df1),col=2)
lines(x,dt(x,df2),col=3)

#Sample
set.seed(10)
mu=0; sigma2=20; n=64
x=-100:100
plot(x,dnorm(x,mu,sigma2),type='l', ylim=c(0,.025))
y=rnorm(n,mu,sigma2)
lines(density(y),col=2)
#Distribution with sample mean and deviation
lines(x,dnorm(x,mean(y),sqrt(var(y))),col=3)

#F distribution
x2=0:500/100
plot(x2,df(x2,2,100),type='l',ylim=c(0,2))
lines(x2,df(x2,5,100),col=2)
lines(x2,df(x2,20,100),col=3)
lines(x2,df(x2,100,100),col=4)
plot(x2,df(x2,5,1),type='l',ylim=c(0,1))
lines(x2,df(x2,5,5),col=2)
lines(x2,df(x2,5,20),col=3)
lines(x2,df(x2,5,100),col=4)
lines(x2,df(x2,5,1000),col=5)

#P(X>c) when X~f(5,100) (p-value for hypothesis test)
c=1.5
plot(x2,df(x2,5,100),type='l')
abline(v=c)
arrows(c,-.01,5,-.01,col=2,.1,15)
1-pf(c,5,100)
###################################################################################
############################# CHAPTER 3 ###########################################
###################################################################################
# 3.2 Testing Examples  
###################################################################################
data(gala)
head(gala)
head(gala[,-2])
?gala
dim(gala)
lmod<-lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,gala)
nullmod<-lm(Species~1,gala)
#Compare the two models
anova(nullmod,lmod)
#Compare
summary(lmod)

n=30;p=6;q=1
(rss0=deviance(nullmod))
(rss=deviance(lmod))
(fstat=((rss0-rss)/(p-q))/(rss/(n-p)))
p-q;n-p
1-pf(fstat,p-q,n-p)

#Leave the area out of the model
(q=5)
lmods<-lm(Species~Elevation+Nearest+Scruz+Adjacent,gala) #No area
anova(lmods,lmod) #Fail to reject, area can be left off
#Note that Df in anova is p-q
#p-value >.05, so we fail to reject the null hypothesis (remove area)

# Alternative here is estimate for area divided by its SE follows a t(n-p),
# same p-value
summary(lmod)
#Note:
(co=coefficients(summary(lmod)))
(tv=co[,1]/co[,2])  #t-values
# Probability of finding a t-value more extreme than what we found
2*(1-pt(abs(tv),n-p))
# Square the t-values
tv^2

#Now test if beta(area)=0 versus a model containing just area
summary(lm(Species~Area,gala)) #Significant!

#Test whether area and adjacent are needed in the full model
lmods<-lm(Species~Elevation+Nearest+Scruz,gala) #No area of current or adjacent island
anova(lmods,lmod) #reject, need at least one of them
#Obtain just the F-statistic
anova(lmods,lmod)[2,5]

#Test to see if we can replace area and adjacent with their sum
lmods<-lm(Species~I(Area+Adjacent)+Elevation+Nearest+Scruz,gala)
anova(lmods,lmod) #reject, cannot replace with the sum

#Test to see if elevation equals .5
(tstat=(coefficients(summary(lmod))["Elevation","Estimate"]-.5)/coefficients(summary(lmod))["Elevation","Std. Error"])
2*(1-pt(abs(tstat),n-p))
#Test to see if elevation equals .3
(tstat=(coefficients(summary(lmod))["Elevation","Estimate"]-.3)/coefficients(summary(lmod))["Elevation","Std. Error"])
2*(1-pt(abs(tstat),n-p))

#Alternate method (not efficient)
lmods<-lm(Species~Area+offset(0.3*Elevation)+Nearest+Scruz+Adjacent,gala) 
anova(lmods,lmod) #reject

tstat^2 #Same as F
###################################################################################
# 3.3 Permutation Tests  
###################################################################################
data(gala)
head(gala)
?gala
dim(gala)
lmod<-lm(Species~Nearest+Scruz,gala)
(lms<-summary(lmod))
lms$fstat
1-pf(lms$fstatistic[1],lms$fstatistic[2],lms$fstatistic[3])

#Create the permutations and calculate their F
nreps=4000
set.seed(123)
fstats<-numeric(nreps)
for (i in 1:nreps) {
  lmods<-update(lmod,sample(Species)~.,gala)
  fstats[i]<-summary(lmods)$fstat[1]
}
#Calculate the proportion larger than what we originally found
mean(fstats>lms$fstat[1]) #Close to normal p-value

#Now permute the values of a predictor
summary(lmod)$coef[3,3] #t-value for Scruz

tstats<-numeric(nreps)
set.seed(123)
for (i in 1:nreps) {
  lmods<-update(lmod,.~Nearest+sample(Scruz),gala)
  tstats[i]<-summary(lmods)$coef[3,3]
}
#Calculate the proportion larger than what we originally found
mean(abs(tstats)>abs(lms$coef[3,3]))
summary(lmod) #Compare to normal version
###################################################################################
# 3.5 Confidence Intervals for Beta  
###################################################################################
lmod<-lm(Species~.-Endemics,gala)
n=30;p=6;alpha=.05
summary(lmod)
(Ar=summary(lmod)$coef["Area",])
#95% CI
Ar[1]+c(-1,1)*qt(1-alpha/2,n-p)*Ar[2]
#Zero lies in the CI, so null is not rejected (Area not needed in model)

Adj=summary(lmod)$coef["Adjacent",]
Adj[1]+c(-1,1)*qt(1-alpha/2,n-p)*Adj[2]
#Zero does not lie in the CI, so null is rejected (Adjacent is needed in model)

#Get all of the CIs
confint(lmod)
confint(lmod,level=.99)

#Confidence region for two parameters (area and adjacent)
?ellipse.lm
plot(ellipse(lmod,c(2,6)),type="l",ylim=c(-.13,0))
points(Ar[1],Adj[1],pch=19) #Get the center
points(0,0,pch=19,col=2) #Get the point (0,0)
#Add the individual CIs
abline(v=confint(lmod)["Area",],lty=2)
abline(h=confint(lmod)["Adjacent",],lty=2)
points(-.065,-.05,pch="A")
points(.0235,-.036,pch="B",cex=.8)
#We fail to reject bArea=0, reject bAdj=0, and reject bArea=0 and bAdj=0.
#If (0,0) was in area A, we would fail to reject bArea=0, fail to reject bAdj=0,
#but reject (bArea,bAdj)=(0,0).
#If (0,0) was in area B, we would reject bArea=0, reject bAdj=0,
#but fail to reject (bArea,bAdj)=(0,0).
###################################################################################
# 3.6 Bootstrap Confidence Intervals  
###################################################################################
#Sample function
sample(10,rep=T)

#We create the data frame coefmat with columns equal to the predictors,
#each column containing 4000 bootstrap values of beta for that predictor
set.seed(123)
nb=4000
coefmat<-matrix(NA,nb,6)
resids<-residuals(lmod)
preds<-fitted(lmod)
for (i in 1:nb) {
  booty<-preds+sample(resids,rep=T)
  bmod<-update(lmod,booty~.)
  coefmat[i,]<-coef(bmod)
}
colnames(coefmat)<-c("Intercept",colnames(gala[,3:7]))
coefmat<-data.frame(coefmat)

#Use the apply function to run a function on each column of coefmat
result=apply(coefmat,2,function(x) quantile(x,c(.025,.975)))

#Plot the data in coefmat for the predictor Area, adding its quantiles as lines 
ggplot(coefmat,aes(x=Area))+geom_density()+geom_vline(xintercept=result[,2],lty=2)
#Plot the data in coefmat for the predictor Adjacent, adding its quantiles as lines 
ggplot(coefmat,aes(x=Adjacent))+geom_density()+geom_vline(xintercept=result[,6],lty=2)

###################################################################################
#Other examples of the apply function
###################################################################################
#calculate the mean of each column
apply(coefmat,2,mean)
#calculate the sd of each column
apply(coefmat,2,sd)
#Compare
summary(lmod)$coef

#calculate the mean of each row (only display the first couple)
#Note that these are not meaningful numbers in this case
head(apply(coefmat,1,mean))
###################################################################################
############################# CHAPTER 4 ###########################################
###################################################################################
# 4.2 Predicting Body Fat  
###################################################################################
data(fat)
head(fat)
?fat
dim(fat)
lmod<-lm(brozek~age+weight+height+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,fat)
summary(lmod)
x<-model.matrix(lmod)
(x0<-apply(x,2,median))
(y0<-crossprod(x0,coef(lmod)))
predict(lmod,new=data.frame(t(x0)))

#Intervals
predict(lmod,new=data.frame(t(x0)),interval="prediction")
predict(lmod,new=data.frame(t(x0)),interval="confidence")

#Now use values at the 95th percentile
(x1<-apply(x,2,function(x) quantile(x,.95)))

predict(lmod,new=data.frame(t(x1)),interval="prediction")
predict(lmod,new=data.frame(t(x1)),interval="confidence")

#Now a smaller example
lmod<-lm(brozek~age+weight+height,fat)
summary(fat)
summary(lmod)
x0=c(1,25,170,60)
(y0=crossprod(x0,coef(lmod)))
#This does not work; needs names!
predict(lmod,new=data.frame(t(x0)),interval="prediction")
names(x0)=names(coef(lmod))
#Now it does
predict(lmod,new=data.frame(t(x0)),interval="prediction")
predict(lmod,new=data.frame(t(x0)),interval="confidence")
