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
# Linear Algebra Review: Matrices 
###################################################################################
(X=matrix(1:12,4,3))
X[1,]           #This is x1
X[,1]           #This is x(1)
X[1,,drop=F]    #This is a matrix consisting of 1 row
X[,1,drop=F]    #This is a matrix consisting of 1 column
X[1:2,]
X[1:2,c(2,3)]
y1=c(2,3,4);y2=1:3;y3=c(-1,2,0)
(Y=rbind(y1,y2,y3))

t(X)            #Transpose of X

dim(X);dim(Y)
(M=X%*%Y)
dim(M)

#Transpose property
t(X%*%Y)
t(Y)%*%t(X)
t(X)%*%t(Y)

#Identity matrices
(I3=diag(3))
(I4=diag(4))

#Other options with diag #
diag(5,4) #This is 5*I4
diag(1:4)
diag(c(2,0,1))
diag(Y) #For a square matrix
diag(X)

X%*%I3
I4%*%X

#Rank of a matrix
rankMatrix(X)
as.numeric(rankMatrix(X))
as.numeric(rankMatrix(t(X)))
as.numeric(rankMatrix(Y))
as.numeric(rankMatrix(t(Y)))

#Inverse of a matrix
solve(X)                 #Matrix must be square
solve(X[1:3,])           #Matrix must be full rank
(Yi=solve(Y))            #This one is invertible (non-singular) as it is square and full rank
round(Y%*%Yi,digits=3)
round(Yi%*%Y,digits=3)
t(solve(Y))
solve(t(Y))

#X is not full rank, but its first two columns are
dim(X[,1:2])
(A=t(X[,1:2])%*%X[,1:2])
solve(A)

(U=matrix(c(3,2,0,5,3,-3,-1,0,4),3))
as.numeric(rankMatrix(U))
solve(U)
solve(Y%*%U)
solve(U)%*%solve(Y)
solve(Y)%*%solve(U)

#Trace of a matrix
sum(diag(Y))
#Via a function
tr(Y)
X
(Z=matrix(1:12,3,4))
tr(X%*%Z)
tr(Z%*%X)

#1 matrix
one3=rep(1,3)
one3%*%t(one5) #3x5 matrix of ones

#non-full rank matrix, find out how to obtain linear combination of one column w.r.t. the others
X
solve(X[1:3,])   #X[1:3,] is square, but singular
as.numeric(rankMatrix(X[1:3,])) #Rank is 2, so use a 2x2
(k=solve(X[1:2,1:2],X[1:2,3]))
#Alternative way of finding k (now answer is a matrix)
solve(X2[1:2,1:2])%*%X2[1:2,3]
X[,1:2]%*%k
X[,3]
