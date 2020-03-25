###################################################################################
#
# HW2.R
#
###################################################################################
# External Functions
###################################################################################
library(faraway)
library(Matrix)
###################################################################################
# Internal Functions
###################################################################################
# tr
#         Obtain the trace of a matrix
# Parameters
# x:      A square matrix
# Output: Trace of the matrix X if it is square, error if it is not square
###################################################################################
tr<-function(X) {
  X=as.matrix(X)
  if (nrow(X)!=ncol(X)) stop("Matrix must be square")
  sum(diag(X))
}
###################################################################################
# proj
#         Obtain the projection matrix of a matrix X
# Parameters
# x:      A full rank matrix with the # of columns (p) >= number of rows (n)
# Output: Projection matrix of X, error if it is not full rank or p > n
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
data(cheddar)
head(cheddar)
?cheddar
dim(cheddar)
#a)
summary(cheddar)

#b)
lmod<-lm(taste~Acetic+H2S+Lactic,cheddar)
coef(lmod)
summary(lmod)
#Estimated model: Taste=-28.88+.3277Acetic+3.9118H2S+19.6705Lactic

#c)
X=model.matrix(~Acetic+H2S+Lactic,cheddar)
dim(X)
as.numeric(rankMatrix(X))

#d)
PX=proj(X)
tr(PX)

#e)
PX%*%cheddar$taste #The fitted values
#Alternate method
fitted(lmod)

#f)
summary(lmod)$r.squared
deviance(lmod)

#g)
#We expect it to be zero, since the residual vector is orthogonal to the
# columnspace (manifold) of X by design.
crossprod(fitted(lmod),residuals(lmod))
#It is very close to zero (rounding error)

#h)
df.residual(lmod)

#i)
deviance(lmod)/df.residual(lmod)
#or
summary(lmod)$sigma^2

#j) Lactic is most variable as its standard error is 8.6291 vs. 1.2484
# for H2S and 4.4598 for Acetic. H2S is most consistent.

#k) The coefficient of Acetic is more likely to be equal to zero than
# the coefficients of the other two predictors.

###################################################################################
# Problem 2 
###################################################################################
#a)
lmod2<-lm(taste~1,cheddar)
coef(lmod2)
#Estimated model: Taste=24.53
#Here, we are estimating the reponse "taste" to equal its mean.

#b)
deviance(lmod2)

#c)
summary(lmod2)
#No R2 is reported because R2 represents the improvement of the model
#with respect to the model containing no predictors; since this IS the
#model with no predictors, there is no improvement and R2 would be 0.
summary(lmod2)$r.squared

#d)
deviance(lmod2)-deviance(lmod)

#e) Yes, it makes sense! The residuals are much smaller when the
#three predictors are included.

#f) An error is the difference between the response and its expected value
# (long term average), while the residual is the difference between the
# response and its fitted value, where the fitted value is the ESTIMATED
# expected value (using least squares estimates).

