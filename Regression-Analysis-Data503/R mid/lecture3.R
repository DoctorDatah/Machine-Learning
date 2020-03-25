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
# Linear Algebra Review: Orthogonal Projection 
###################################################################################
(X=matrix(1:12,4,3))
PX=proj(X)
(X2=X[,1:2])
#Note that M(X2)=M(X)!
(PX=proj(X2))

#property i)
t(PX)
PX%*%PX

#property ii)
y=c(-1,0,5,-3)
(yhat=PX%*%y)
er=y-yhat
#The error is orthogonal to all columns of X
round(crossprod(er,X[,1]),3)
round(crossprod(er,X[,2]),3)
round(crossprod(er,X[,3]),3)
#Shortcut
round(crossprod(er,X),3)
#It is also orthogonal to any vector in M(X)
(v=X[,1]+7*X[,2]-2*X[,3])
round(crossprod(er,v),3)

#property iii)
tr(PX)

#Projection of a matrix
(A=matrix(c(y,5,3,2,0),4))
(Ahat=PX%*%A)
round(crossprod((diag(4)-PX)%*%A,X),3)

#Projection of a vector that lies in M(X)
X[,3]
PX%*%X[,3]
t(X[,3])%*%PX
#Any other vector in M(X)
(v=3*X[,1]+2*X[,2]-4*X[,3])
X%*%c(3,2,-4)       #Alternatively
PX%*%v
t(v)%*%PX
