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
###################################################################################
# Processing 
###################################################################################
#Save the environment
parSave=par(no.readonly = TRUE)
###################################################################################
# Initial Data Analysis 
###################################################################################
#Check the data
data(pima) #Reset the data to its original value
head(pima)
?pima
summary(pima)
sort(pima$diastolic,na.last=F)

#Set zero values to NA
pima$diastolic[pima$diastolic==0]<-NA
pima$glucose[pima$glucose==0]<-NA
pima$triceps[pima$triceps==0]<-NA
pima$insulin[pima$insulin==0]<-NA
pima$bmi[pima$bmi==0]<-NA

#Test is a factor rather than a number
pima$test<-factor(pima$test)
summary(pima$test)
levels(pima$test)<-c("negative","positive")

#regular plots
hist(pima$diastolic,xlab="Diastolic",main="")
plot(density(pima$diastolic,na.rm=T),main="")
par(mfrow=c(1,2))
plot(pima$diastolic)
plot(sort(pima$diastolic),ylab="Sorted Diastolic")
par(parSave)
plot(diabetes~diastolic,pima)
plot(diabetes~test,pima)

#ggplots
ggplot(pima,aes(x=diastolic))+geom_histogram()
ggplot(pima,aes(x=diastolic))+geom_density()
ggplot(pima,aes(x=diastolic,y=diabetes))+geom_point()
ggplot(pima,aes(x=diastolic,y=diabetes,shape=test))+geom_point()+
  theme(legend.position="top",legend.direction="horizontal")
ggplot(pima,aes(x=diastolic,y=diabetes))+geom_point(size=1)+facet_grid(~test)
ggplot(pima,aes(x=test))+stat_count()
###################################################################################
# Linear Algebra Review: Vectors 
###################################################################################
x=1
(x=c(2,3,4))               #Vector
length(x)                  #Dimension
matrix(x)                  #What it really looks like
t(x)                       #Transpose
(one5=rep(1,5))            #1 vector

#Multiplication
y=c(-2,4,0)
crossprod(x,y)             #x'y
norm(x,"2")                #||x||

#Subtraction
a=c(2,5); b=c(1,1)
c=a-b
plot(1,type='n',xlim=c(0,3),ylim=c(0,6),xlab="",ylab="")
arrows(0,0,a[1],a[2])
arrows(0,0,b[1],b[2])
arrows(0,0,c[1],c[2])     #Vector a-b
arrows(b[1],b[2],c[1]+b[1],c[2]+b[2],col=2)    #Vector a-b moved to the end of vector b

#Orthogonal vectors
y2=c(5,-2,-1)
crossprod(x,y2)

#property i)
x
z=c(9,-3,2)
y+z
crossprod(x,y+z)
crossprod(x,y)+crossprod(x,z)

#property ii)
t(x+y)
t(x)+t(y)

#property iii)
norm(x+y,"2")^2
crossprod(x+y,x+y) #Alternative
crossprod(x,x)+crossprod(y,y)+2*crossprod(x,y)
#NOTE: crossprod(x,x)=crossprod(x). so we may write:
crossprod(x)+crossprod(y)+2*crossprod(x,y)


#property iv)
crossprod(x+y2,x+y2)
crossprod(x,x)+crossprod(y2,y2)

#property v)
norm(x,"2")                #||x||
norm(x,"2")^2              #||x||^2
crossprod(x,x)             #x'x, or ||x||^2
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
as.vector(X[1:2,])  #Turn it into a vector
y1=c(2,3,4);y2=1:3;y3=c(-1,2,0)
(Y=rbind(y1,y2,y3))

t(X)            #Transpose of X

dim(X);dim(Y)
(M=X%*%Y)
dim(M)
