###################################################################################
#
# midterm.R
#
###################################################################################
# External Functions
###################################################################################
library(faraway)
library(Matrix)
library(ellipse)
library(datasets)
source("systemEq.R")
###################################################################################
# Internal Functions
###################################################################################
# tr
#     Obtain the trace of a matrix
# Parameters
#     x:      A square matrix
#     Output: Trace of the matrix X if it is square, error if it is not square
###################################################################################
tr<-function(X) {
  X=as.matrix(X)
  if (nrow(X)!=ncol(X)) stop("Matrix must be square")
  sum(diag(X))
}
###################################################################################
# proj
#     Obtain the projection matrix of a matrix X
# Parameters
#     x:      A full rank matrix with the # of columns (p) >= number of rows (n)
#     Output: Projection matrix of X, error if it is not full rank or p > n
###################################################################################
proj<-function(X) {
  X=as.matrix(X)
  if (ncol(X) > nrow(X)) stop("Matrix has more columns than rows")
  if (rankMatrix(X)!=ncol(X)) stop("Matrix is not full rank")
  X%*%(solve(t(X)%*%X))%*%t(X)
}
###################################################################################
# Processing 
###################################################################################
#Save the environment
parSave=par(no.readonly = TRUE)
###################################################################################
# Problem 1 
###################################################################################
x.2=c(-1,2,1,5,6,5,3,0)
x.3=c(2,0,-2,2,4,-6,0,5)
x.4=c(-4,4,4,8,8,16,6,-5)
y=c(200,180,150,205,300,100,120,230)
one=rep(1,8)
X=cbind(one,x.2,x.3)
Z=cbind(one,x.2,x.3,x.4)
#a)
norm(x.2,"2")^2
crossprod(x.2) #alternative
#b)
crossprod(x.3,x.4)
#c) 
#d)
systemEq(X[1:3,],y[1:3])
#e)
tr(X[1:3,])
#f)
as.numeric(rankMatrix(X))
as.numeric(rankMatrix(Z))
#g)
#h)
systemEq(X,x.4)
#i)
t(round(proj(X)%*%y))
#j)
proj(Z)%*%y
#k)
lmod=lm(y~x.2+x.3+x.4)
summary(lmod)
#l)
round(fitted(lmod))
#m)
lmod=lm(y~x.2+x.3)
summary(lmod)
round(coef(lmod),2)
#n)
round(coef(lm(y~x.3)),2)
#All must be orthogonal
crossprod(x.2,x.3)  #This one is orthogonal
crossprod(one,x.2)  #This one is not
crossprod(one,x.3)  #This one is not
###################################################################################
# Problem 2 
###################################################################################
data(attitude)
head(attitude)
?attitude
dim(attitude)
summary(attitude)
n=nrow(attitude)
#a)
plot(complaints~privileges,attitude)
#b)
lmod=lm(rating~.,attitude)
summary(lmod)
round(deviance(lmod))
df.residual(lmod)
round(summary(lmod)$r.squared,2)
p=length(coef(lmod))
#c)
summary(lmod)
#d)
summary(lmod)
#e)
lmod2=update(lmod,.~learning+raises)
q=length(coef(lmod2))
round(coef(lmod2),2)
#f)
summary(lmod2)
#g)
anova(lmod2,lmod)
#h)
summary(lmod)$coef[4,3] #t-value for learning
nreps=4000
tstats<-numeric(nreps)
set.seed(123)
for (i in 1:nreps) {
  lmods<-update(lmod,.~complaints+privileges+sample(learning)+raises+critical+advance)
  tstats[i]<-summary(lmods)$coef[4,3]
}
#Calculate the proportion larger than what we originally found
mean(abs(tstats)>abs(summary(lmod)$coef[4,3]))
#i)
round(confint(lmod)[4,],3)
#j)
#k)
round(confint(lmod)[7,],3)
#l)
plot(ellipse(lmod,c(4,7)),type="l")
points(coef(lmod)[4],coef(lmod)[7],pch=19) #Get the center
#Add the individual CIs
abline(v=confint(lmod)[4,],lty=2)
abline(h=confint(lmod)[7,],lty=2)
points(.6,.1,pch=19,col=2)
#m)
set.seed(123)
nb=4000
betai<-rep(NA,nb)
for (i in 1:nb) {
  booty<-fitted(lmod)+sample(residuals(lmod),rep=T)
  bmod<-update(lmod,booty~.)
  betai[i]<-coef(bmod)[4]
}
round(sd(betai),3)
#n)
x0=c(1,45,43); names(x0)=names(coef(lmod2))
round(predict(lmod2,new=data.frame(t(x0)),interval="confidence", level=.90),1)
#o)
t(c(1,66,70))%*%coef(lmod2)
#Alternative
x0=c(1,66,70); names(x0)=names(coef(lmod2))
round(predict(lmod2,new=data.frame(t(x0))))
