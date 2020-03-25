###################################################################################
#
# HW4.R
#
###################################################################################
# External Functions
###################################################################################
library(faraway)
library(ggplot2)
library(lmtest)
library(MASS)
library(Amelia)
###################################################################################
# Internal Functions
###################################################################################
# Processing 
###################################################################################
#Save the environment
parSave=par(no.readonly = TRUE)
###################################################################################
# Problem 1 
###################################################################################
data(sat)
head(sat)
?sat
dim(sat)
n=nrow(sat)
summary(sat)

#a)
lmod<-lm(total~expend+ratio+salary+takers,sat)
p=length(coef(lmod))
summary(lmod)

#b)
par(mfrow=c(2,2))
plot(lmod)
par(parSave)
#Check just the normal plot
#qqnorm(residuals(lmod),ylab="Residuals", main="")
#qqline(residuals(lmod))

shapiro.test(residuals(lmod))

#d)
2*p/n
sort(hatvalues(lmod))
#also
halfnorm(hatvalues(lmod),4,labs=row.names(sat),ylab="leverages")

#e)
par(mfrow=c(2,2))
plot(sat$expend,residuals(lmod), xlab="Expenditure", ylab="Residuals")
points(sat["Utah","expend"],residuals(lmod)["Utah"],pch="X",col=2)
abline(h=0)
plot(sat$ratio,residuals(lmod), xlab="Avg. Pupil/Teacher ratio", ylab="Residuals")
points(sat["Utah","ratio"],residuals(lmod)["Utah"],pch="X",col=2)
abline(h=0)
plot(sat$salary,residuals(lmod), xlab="Estimated Average Annual Salary of Teachers", ylab="Residuals")
points(sat["Utah","salary"],residuals(lmod)["Utah"],pch="X",col=2)
abline(h=0)
plot(sat$takers,residuals(lmod), xlab="% Eligible Students Taking the SAT", ylab="Residuals")
points(sat["Utah","takers"],residuals(lmod)["Utah"],pch="X",col=2)
abline(h=0)
par(parSave)

#g)
sort(abs(residuals(lmod)))

#h)
sort(abs(rstudent(lmod)))

#j)
par(mfrow=c(1,2))
d=residuals(lm(total~expend+ratio+salary,sat))
m=residuals(lm(takers~expend+ratio+salary,sat))
plot(m,d,xlab="% Eligible Students Taking the SAT", ylab="SAT residuals")
abline(0,coef(lmod)['takers'])

termplot(lmod,partial.resid=T,terms=4)
par(parSave)

#k)
X1=as.matrix(sat[,1:4])
#eigendecomposition of X1'X1
ev<-eigen(crossprod(X1))
ev$val
(K=sqrt(ev$val[1]/ev$val)) #One value is too large (>=30)

#l)
round(cor(X1),2) #Salary and expenditure are highly correlated

#m)
vif(X1)

#n)
summary(lmod)
lmod2=lm(total~ratio+salary+takers,sat)
summary(lmod2)
summary(lm(total~expend+ratio+takers,sat))
par(mfrow=c(2,2))
plot(lmod2)
par(parSave)
#o)
boxcox(lmod2,plotit=T)
boxcox(lmod2,plotit=T,lambda=seq(-5,1.5,by=.1))
lmod3=lm(total^-2~ratio+salary+takers,sat)
summary(lmod3)

#p)
lmod4=lm(total~ratio+poly(takers,3)+salary,sat)
summary(lmod4)
lmod4=lm(total~ratio+poly(takers,2)+salary,sat)
summary(lmod4)

#q)
par(mfrow=c(4,2))
plot(lmod)
plot(lmod4)
par(parSave)

#r)
#Imputation by regression
set.seed(123)
sat2=sat
sat2[sample(n,20),3]=NA
sat3=sat2

lmodr<-lm(salary~expend+ratio+takers,sat3)
sat3[is.na(sat2[,3]),3]=round(predict(lmodr,sat3[is.na(sat2[,3]),]),3)
head(sat[is.na(sat2[,3]),])
head(sat2[is.na(sat2[,3]),])
head(sat3[is.na(sat2[,3]),])
cbind(sat[,3],sat2[,3],sat3[,3])

#s)
summary(lm(total~salary+takers,sat))
summary(lm(total~salary+takers,sat2))
summary(lm(total~salary+takers,sat3))

#t)
set.seed(123)
sata<-amelia(sat2[,c(1:4,7)],25) 

betas<-NULL
ses<-NULL
for (i in 1:sata$m) {
  lmodi<-lm(total~salary+takers,sata$imputations[[i]])
  betas=rbind(betas,coef(lmodi))
  ses=rbind(ses,coef(summary(lmodi))[,2])
}

#Find the combined values
cr<-mi.meld(q=betas,se=ses)

#Regression coefficient comparison
cr$q.mi
coef(lm(total~salary+takers,sat))
coef(lm(total~salary+takers,sat2))
coef(lm(total~salary+takers,sat3))

#Standard error comparision
cr$se.mi
coef(summary(lm(total~salary+takers,sat)))[,2]
coef(summary(lm(total~salary+takers,sat2)))[,2]
coef(summary(lm(total~salary+takers,sat3)))[,2]
