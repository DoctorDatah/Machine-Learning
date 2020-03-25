###################################################################################
#
# HW1.R
#
###################################################################################
# External Functions
###################################################################################
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
(x.1=1:3)
(x.2=rep(2,3))
(x.3=0:2)
(x.4=c(1,1,-1))
(y=c(5,4,2))
(X=cbind(x.1,x.2))
(Z=matrix(c(-1,-1,4,-1,8,2,3,1),2))

#a)
t(x.1)
#b)
crossprod(x.1,x.2)
#c)
x.1+x.2
#d)
x.1-x.2
#e)
norm(x.1,"2")
#f)
crossprod(x.1)
norm(x.1,"2")^2
#g) NO
#h) 
crossprod(x.1,x.4) #YES
#i)
(s=crossprod(rep(1,3),y))
s/3
#j)
mean(y)
#k) (a1+...+an)/n
#l) The mean of the elements of a.
###################################################################################
# Problem 2 
###################################################################################
#a)
X%*%Z
#b)
t(X)%*%X
#Alternative
crossprod(X)
#c)
solve(t(X)%*%X)
#d)
proj(X)
#e)
as.numeric(rankMatrix(X))
#f)
tr(t(X)%*%X)
#g)
crossprod(X,Z)
#No, because their columnvectors are not the same length.
#Additional question; are X' and Z orthogonal?
crossprod(t(X),Z)
#No, because their cross product is not a zero matrix.
#h)
#Yes, because its rank is 2 and min(2,3)=2.
#i)
proj(X)%*%y
#j)
#No, since PXy does not equal y
#k)
round(proj(X)%*%x.3,3)
#l)
#Yes, since PXx.3=x.3
#m)
Z%*%X
dim(Z);dim(X)
#No, because the # of columns of Z does not equal the # of rows of X
