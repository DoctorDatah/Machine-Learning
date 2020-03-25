###################################################################################
#
# systemEq.R
#
###################################################################################
# systemEq.R
#         Solve a system of Equations
# Parameters
#   X:      A matrix
#   v:      A vector
# Output: A vector that solves the system or an error message
###################################################################################
systemEq<-function(X,v) {
  if (!require(Matrix)) stop("Package 'Matrix' must be installed")
  X=as.matrix(X)
  v=as.vector(v)
  if (nrow(X)!=length(v)) stop("Dimensions do not conform")
  if (nrow(X)<ncol(X)) stop("# of rows must be greater or equal to # columns")
  p=ncol(X)
  if (rankMatrix(X[1:p,])!=p) stop("Many solutions")
  x=solve(X[1:p,],v[1:p])
  if (!all((X%*%x)==v)) stop("Inconsistent System: No solution")
  x
}
