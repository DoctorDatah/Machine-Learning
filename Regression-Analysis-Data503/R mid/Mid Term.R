library(faraway)
library(ggplot2)
library(ellipse)
library(Matrix)
library(datasets)
source("systemEq.R")

tr<-function(X) {
  X=as.matrix(X)
  if (nrow(X)!=ncol(X)) stop("Matrix must be square")
  sum(diag(X))
}
proj<-function(X) {
  X=as.matrix(X)
  if (ncol(X) > nrow(X)) stop("Matrix has more columns than rows")
  if (rankMatrix(X)!=ncol(X)) stop("Matrix is not full rank")
  X%*%(solve(t(X)%*%X))%*%t(X)
}
#vectors
(x.2 = c(-1,2,1,5,6,5,3,0))
(x.3 =c(2,0,-2,2,4,-6,0,5))
(x.4 = c(-4,4,4,8,8,16,6,-5))
(y = c(200,180,150,205,300,100,120,230))


#Matrix
v1 = c(1,1,1,1,1,1,1,1)
(X = cbind(v1,x.2,x.3))
(Z = cbind(v1,x.2,x.3,x.4))

##### Prob 1#####
#a
norm(x.2,"2")^2
#b
crossprod(x.3,x.4) #no 
#c
#paper
#d

x1 = c(1,1,1)
x2= c(-1,2,1)
x3 = c(2,0,-2)
X2 = cbind(x1,x2,x3)
X2
y2 = c(200,180,150)
systemEq(X2,y2)
solve(X2,y2)
#e
tr(X2)
#f
as.numeric(rankMatrix(X))
as.numeric(rankMatrix(Z))

#g
#paper
#h
systemEq(X,x.4)
crossprod(Z,X)
#i
t(round(crossprod(proj(X),y)))
#j
t(round(crossprod(proj(Z),y)))
#k
lmod = lm(y~x.2+x.3+x.4,data.frame(Z))
summary(lmod)
#i
lmod_i = lm(y~x.2+x.3,data.frame(X))
#lmod_i = lm(y~.,data.frame(X))
summary(lmod_i)
round(fitted(lmod_i))
#m
summary(lmod_i)
round(coef(lmod_i),2)
#n
lmod_n = lm(y~x.3,data.frame(X))
round(coef(lmod_n),2)
crossprod(x.2,x.3)

#problem 2
?attitude
summary(attitude)
attitude
#a
plot(attitude$privileges,attitude$complaints)

#b
lmod = lm(rating~.,attitude)
deviance(lmod)
df.residual(lmod)
round(summary(lmod)$r.squared,2)
#c
summary(lmod)
#d
summary(lmod)
#e
lmodw= lm(rating~learning+raises,attitude)
round(coef(lmodw),2)
#f
summary(lmodw)
#g
anova(lmodw,lmod)
deviance(lmodw) - deviance(lmod)
#h
summary(lmod)
summary(lmod)$coef[4,3] #t value for raises b4
nreps = 4000
tstats<-numeric(nreps)
set.seed(123)
for (i in 1:nreps) {
  lmods<-update(lmod,.~complaints+privileges+sample(learning)+raises+critical+advance,gala)
  tstats[i]<-summary(lmods)$coef[4,3]
}
#Calculate the proportion larger than what we originally found
mean(abs(tstats)>abs(lms$coef[4,3]))
summary(lmod) #Compare to normal version

#i #j #k
round(confint(lmod),3)

#l
summary(lmod)
plot(ellipse(lmod,c(4,7)),type="l")
#Add the individual CIs
abline(v=confint(lmod)[4,],lty=2)
abline(h=confint(lmod)[7,],lty=2)
points(coef(lmod)[4],coef(lmod)[7],pch=19) #Get the center
points(.6,.1,pch=19,col=2) #Get the point (.4,.2)

#m
#We create the vector betai of length nb
set.seed(123)
nb=4000
betai<-rep(NA,nb)
resids<-residuals(lmod)
preds<-fitted(lmod)
for (i in 1:nb) {
  booty<-preds+sample(resids,rep=T)
  bmod<-update(lmod,booty~.)
  betai[i]<-coef(bmod)[4]
}
round(sd(betai),3)
round(quantile(betai,c(.025,.975)),3)
#Assuming normality
confint(lmod)[4,]
#n
summary(lmodw)
u = c(1,45,43)
names(u) =names(coef(lmodw))
round(predict(lmodw,new=data.frame(t(u)),interval="confidence", level=.90),1)
#o
u = c(1,66,70)
names(u) =names(coef(lmodw))
round(predict(lmodw,new=data.frame(t(u)),interval="prediction"))