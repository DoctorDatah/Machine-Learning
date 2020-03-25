###################################################################################
#
# Lecture5.R
#
###################################################################################
# External Functions
###################################################################################
#install.packages("faraway")
#install.packages("ggplot2")
library(faraway)
library(ggplot2)
library(Matrix)
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
# Review columnspace 
###################################################################################
(X=matrix(1:12,4,3))
k=c(1,1,1)
(v=X%*%k)    #Vector in the columnspace of X
systemEq(X,v)
(x=systemEq(X[,1:2],v))
X%*%c(x,0)
(x=systemEq(X[,c(1,3)],v))
X%*%c(x[1],0,x[2])
#The vector v (15,18,21,24) lies in M(X), but also in M(X[1:2] and M(X[c(1,3)])
#The columnspaces for X, X[1:2], and X[c(1,3)] are the same
#However, the vector v can be obtained in many different ways using X,
#but only in one way in X[1:2], and X[c(1,3)] since they are full rank
#Note that X[1 1 1]'=v, but also X[0 3 0]'=v and X[1.5 0 1.5]'=v
systemEq(X[,1:2],c(7,6,5,5))
#Note that [7 6 5 5]' does NOT lie in M[X]
#i.e. we cannot find a multiple of the columns of X that creates this vector
###################################################################################
# Linear Model Example using the theory
###################################################################################
data(gala)
head(gala)
head(gala[,-2])
?gala
dim(gala)
#Get some summary values
summary(gala)

(X=model.matrix(~Area+Elevation+Nearest+Scruz+Adjacent,data=gala))
(y=gala$Species)
dim(X)
n=nrow(X); p=ncol(X)

(yhat=proj(X)%*%y)     #y-hat, or the fitted values
(resid=y-yhat)         #The residual vector
RSS=crossprod(resid)   #RSS
(RSS=as.numeric(RSS))

#Check if the matrix is full rank
as.numeric(rankMatrix(X))
p

#It is full rank, so we can calculate the regression coefficients (beta-hat)
XXinv=solve(t(X)%*%X)   #(X'X)^-1
XXinv%*%t(X)%*%y        #beta-hat
#Alternative method for beta-hat: Solve b in X'Xb=X'y (the normal equations)
(betahat=solve(crossprod(X),crossprod(X,y)))


#Assuming normality, estimate the variance of the errors
(sigmahat2=RSS/(n-p))
#Residual standard error
(sigmahat=sqrt(sigmahat2))

#Variance of the regression coefficients
(varbeta=solve(crossprod(X))*sigmahat2)
#variances of the regression coefficients are on the diagonal 
diag(varbeta)
#standard errors of betahat are given by the square root of the variances
(betase=sqrt(diag(varbeta)))
###################################################################################
# Linear Model Example using R functions
###################################################################################
lmod<-lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)

#fitted values
fitted(lmod)
yhat           #Compare

#Residuals
residuals(lmod)
resid          #Compare

#RSS
deviance(lmod)
RSS            #Compare

#Regression coefficients
coef(lmod)
betahat        #Compare

#Degrees of freedom
df.residual(lmod)
n-p            #Compare

#Sigma-hat, i.e. estimate for sigma
lmodsum$sigma
sigmahat       #Compare

#See what other values are available
names(lmod)
lmod$rank

#Summary object
lmodsum<-summary(lmod)
names(lmodsum)

#Standard errors for beta
lmodsum$coef[,2]   #From the summary object
betase         #Compare

#(X'X)^-1
lmodsum$cov.unscaled
XXinv          #Compare

#Model with mean only
lmod2<-lm(Species~1,data=gala)
#Check out the two RSS values  
deviance(lmod2); deviance(lmod)
sum(y^2)-(sum(y)^2)/n      #Obtain the RSS of the model with mean only
t(y)%*%(diag(1,n)-proj(rep(1,n)))%*%y  #Y'(I-P1)Y

#R-squared
(r2=(deviance(lmod2)-deviance(lmod))/deviance(lmod2))
#The sum of squared errors reduced a lot when we added the predictors to the
#model; we saw a 77% improvement.

#R-squared
lmodsum$r.squared
r2             #Compare

#Summary values
summary(lmod)
sumary(lmod)  #Shorter version of summary
