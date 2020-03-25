###################################################################################
#
# Lecture1.R
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
# Linear Model Example 
###################################################################################
data(gala)
head(gala)
head(gala[,-2])
?gala

X=model.matrix(~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
y=gala$Species
solve(t(X)%*%X)%*%t(X)%*%y  #beta-hat
#Alternative method for beta-hat: Solve b in X'Xb=X'y (the normal equations)
solve(crossprod(X),crossprod(X,y))

proj(X)%*%y   #y-hat, or the fitted values

#Now via R function
lmod<-lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
coef(lmod)
fitted(lmod)

#Summary values
summary(lmod)
sumary(lmod)  #Shorter version of summary

#Residuals
residuals(lmod)
y-fitted(lmod)   #y-yhat

#Degrees of freedom
df.residual(lmod) #This is n-p

#RSS
crossprod(residuals(lmod))
deviance(lmod)

#See what other values are available
names(lmod)
lmodsum<-summary(lmod)
names(lmodsum)

#Sigma-hat, i.e. estimate for sigma
sqrt(deviance(lmod)/df.residual(lmod))
lmodsum$sigma

#(X'X)^-1
solve(crossprod(X))
(xtxi<-lmodsum$cov.unscaled)

#Standard errors for beta
sqrt(diag(xtxi))*lmodsum$sigma #Calculate them
lmodsum$coef[,2]   #From the summary object

#R-squared
lmodsum$r.squared

#Model with mean only
lmod2<-lm(Species~1,data=gala)
#Different method for getting R-squared
(deviance(lmod2)-deviance(lmod))/deviance(lmod2)
                       
#Check out the two RSS values  
deviance(lmod2); deviance(lmod)
#The sum of squared errors reduced a lot when we added the predictors to the
#model; we saw a 77% improvement.