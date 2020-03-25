###################################################################################
#
# Lecture6.R
#
###################################################################################
# External Functions
###################################################################################
#install.packages("faraway")
#install.packages("ggplot2")
library(faraway)
library(ggplot2)
library(Matrix)
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
# 2.10 Identifiability
###################################################################################
data(gala)
head(gala)
?gala
# Create a new variable
gala$Adiff<-gala$Area-gala$Adjacent

# Create model including new variable
lmod<-lm(Species~Area+Elevation+Nearest+Scruz+Adjacent+Adiff,gala)
summary(lmod)
coef(lmod)
lmod$rank
###################################################################################
# 2.11 Orthogonality  
###################################################################################
data(odor)
dim(odor)
?odor
odor

#Check if orthogonal
crossprod(odor$temp,odor$gas)
crossprod(as.matrix(odor[,-1]))
# Alternative
cov(odor[,-1])

#Add 1 to every score, then covariance between them is still 0, but no orthogonality
cov(odor[,-1]+1)
crossprod(as.matrix(odor[,-1])+1)

#lmod<-lm(odor~temp+gas+pack,odor)
lmod<-lm(odor~.,odor)
summary(lmod,cor=T)

# Drop one of the predictors
lmod2<-lm(odor~gas+pack,odor)
summary(lmod2,cor=T)

#One at a time
coef(lmod)
coef(lm(odor~1,odor))
#Note that lm always adds the 1 vector unless we explicitly exclude it
coef(lm(odor~temp-1,odor))
coef(lm(odor~gas-1,odor))
coef(lm(odor~pack-1,odor))
#Compare
coef(lm(odor~pack,odor))
###################################################################################
############################# CHAPTER 3 ###########################################
###################################################################################
# 3.2 Testing Examples  
###################################################################################
data(gala)
head(gala)
head(gala[,-2])
?gala
dim(gala)
lmod<-lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,gala)
nullmod<-lm(Species~1,gala)
#Compare the two models
anova(nullmod,lmod)
#Compare
summary(lmod)

n=30;p=6;q=1
(rss0=deviance(nullmod))
(rss=deviance(lmod))
(fstat=((rss0-rss)/(p-q))/(rss/(n-p)))
p-q;n-p
1-pf(fstat,p-q,n-p)

#Leave the area out of the model
(q=5)
lmods<-lm(Species~Elevation+Nearest+Scruz+Adjacent,gala) #No area
anova(lmods,lmod) #Fail to reject, area can be left off
#Note that Df in anova is p-q
#p-value >.05, so we fail to reject the null hypothesis (remove area)

# Alternative here is estimate for area divided by its SE follows a t(n-p),
# same p-value
summary(lmod)
#Note:
(co=coefficients(summary(lmod)))
(tv=co[,1]/co[,2])  #t-values
# Probability of finding a t-value more extreme than what we found
2*(1-pt(abs(tv),n-p))
# Square the t-values
tv^2

#Now test if beta(area)=0 versus a model containing just area
summary(lm(Species~Area,gala)) #Significant!

#Test whether area and adjacent are needed in the full model
lmods<-lm(Species~Elevation+Nearest+Scruz,gala) #No area of current or adjacent island
anova(lmods,lmod) #reject, need at least one of them

#Test to see if we can replace area and adjacent with their sum
lmods<-lm(Species~I(Area+Adjacent)+Elevation+Nearest+Scruz,gala)
anova(lmods,lmod) #reject, cannot replace with the sum

#Test to see if elevation equals .5
(tstat=(coefficients(summary(lmod))["Elevation","Estimate"]-.5)/coefficients(summary(lmod))["Elevation","Std. Error"])
2*(1-pt(abs(tstat),n-p))
#Test to see if elevation equals .3
(tstat=(coefficients(summary(lmod))["Elevation","Estimate"]-.3)/coefficients(summary(lmod))["Elevation","Std. Error"])
2*(1-pt(abs(tstat),n-p))

#Alternate method (not efficient)
lmods<-lm(Species~Area+offset(0.5*Elevation)+Nearest+Scruz+Adjacent,gala) 
anova(lmods,lmod) #reject

tstat^2 #Same as F

