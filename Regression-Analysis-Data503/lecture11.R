###################################################################################
#
# Lecture11.R
#
###################################################################################
# External Functions
###################################################################################
#install.packages("faraway")
#install.packages("ggplot2")
#install.packages("ellipse")
library(faraway)
library(ggplot2)
library(lmtest)
###################################################################################
# Internal Functions
###################################################################################
###################################################################################
# Processing 
###################################################################################
#Save the environment
parSave=par(no.readonly = TRUE)
###################################################################################
# 6.1.1 Constant Variance  
###################################################################################
data(savings,package="faraway")
head(savings)
?savings
dim(savings)
lmod<-lm(sr~pop15+pop75+dpi+ddpi,savings)
summary(lmod)
#Residuals vs. fitted
plot(fitted(lmod),residuals(lmod),xlab="Fitted",ylab="Residuals")
abline(h=0)
plot(fitted(lmod),sqrt(abs(residuals(lmod))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))

summary(lm(sqrt(abs(residuals(lmod)))~fitted(lmod)))

#Check to see what plots geneerally look like
par(mfrow=c(3,3))
n=50
set.seed(123)
#Constant variance
for (i in 1:9) {x=runif(n); plot(x,rnorm(n))}
#Strong nonconstant variance
for (i in 1:9) {x=runif(n); plot(x,x*rnorm(n))}
#Mild nonconstant variance
for (i in 1:9) {x=runif(n); plot(x,sqrt(x)*rnorm(n))}
#Nonlinearity
for (i in 1:9) {x=runif(n); plot(x,cos(x*2*pi)+rnorm(n,sd=1))}
for (i in 1:9) {x=runif(n); plot(x,cos(x*pi/25)+rnorm(n,sd=.002))}
#for (i in 1:9) {x=runif(n); plot(x,cos(x*pi/25)+rnorm(n,sd=1))}
par(parSave)

#Residuals vs. predictors
plot(savings$pop15,residuals(lmod), xlab="Population under 15", ylab="Residuals")
abline(h=0)
plot(savings$pop75,residuals(lmod), xlab="Population over 75", ylab="Residuals")
abline(h=0)

var.test(residuals(lmod)[savings$pop15>35],residuals(lmod)[savings$pop15<=35])

data(gala)
lmod<-lm(Species~Area+Elevation+Nearest+Scruz+Nearest+Adjacent,gala)
plot(fitted(lmod),residuals(lmod),xlab="Fitted",ylab="Residuals")
abline(h=0)
lmod<-lm(sqrt(Species)~Area+Elevation+Nearest+Scruz+Nearest+Adjacent,gala)
plot(fitted(lmod),residuals(lmod),xlab="Fitted",ylab="Residuals")
abline(h=0)
###################################################################################
#Check non-included variable to see if it should be included
###################################################################################
data(prostate)
lmod<-lm(lpsa~lweight+age+lbph+svi+lcp+gleason+pgg45,prostate)
plot(residuals(lmod)~fitted(lmod))  #Not great
plot(residuals(lmod)~prostate$lcavol)  #There appears to be a relationship!
lmod<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,prostate)  #Add it
plot(residuals(lmod)~fitted(lmod))  #Better
plot(residuals(lmod)~prostate$lcavol)  #No more relationship
###################################################################################
# 6.1.2 Normality  
###################################################################################
#quantiles
qnorm(.1)     #10th quantile
qnorm(.25)    #1st quartile
qnorm(.5)     #median
qnorm(.75)    #3rd quartile
qnorm(.9)     #90th quantile
###################################################################################
lmod<-lm(sr~pop15+pop75+dpi+ddpi,savings)
qqnorm(residuals(lmod),ylab="Residuals", main="")
qqline(residuals(lmod))

hist(residuals(lmod),xlab="Residuals",main="")

par(mfrow=c(3,3))
n=50
#Normal data
for (i in 1:9) {x=rnorm(n); qqnorm(x);qqline(x)}
#lognormal data
for (i in 1:9) {x=exp(rnorm(n)); qqnorm(x);qqline(x)}
#for (i in 1:9) {x=rlnorm(n); qqnorm(x);qqline(x)} #Alternate
#Cauchy data
for (i in 1:9) {x=rcauchy(n); qqnorm(x);qqline(x)}
#Uniform data
for (i in 1:9) {x=runif(n); qqnorm(x);qqline(x)}
par(parSave)

shapiro.test(residuals(lmod))
###################################################################################
# 6.2.1 Leverage  
###################################################################################
plot(-400:400/100,dnorm(-400:400/100),type='l',ylim=c(0,1),xlab="",ylab="")
lines(0:400/100,2*dnorm(0:400/100),col=2)
abline(v=0,col=2)
set.seed(123)
z=rnorm(100)
lines(density(z),col=3)
lines(density(abs(z)),col=4)
qqnorm(z);qqline(z)
halfnorm(abs(z))
###################################################################################
data(savings)
n=nrow(savings)
lmod<-lm(sr~pop15+pop75+dpi+ddpi,savings)
p=length(coef(lmod))
summary(lmod)
hatv<-hatvalues(lmod)
head(hatv)
sum(hatv)
hatv[hatv>2*p/n]

#Half-normal plot
countries<-row.names(savings)
halfnorm(hatv,labs=countries,ylab="Leverages")

#Plot the standardized residuals
qqnorm(rstandard(lmod))
abline(0,1)
qqnorm(residuals(lmod));qqline(residuals(lmod)) #Compare
###################################################################################
# 6.2.2 Outliers  
###################################################################################
set.seed(123)
testdata<-data.frame(x=1:10,y=1:10+rnorm(10))
n=10; p=2
plot(y~x,testdata)
lmod<-lm(y~x,testdata)
summary(lmod)
halfnorm(hatvalues(lmod),ylab="Leverages")

#Add an outlier with a central predictor value 
p1=c(5.5,12)
lmod1<-lm(y~x,rbind(testdata,p1))
plot(y~x,rbind(testdata,p1))
points(p1[1],p1[2],pch=4,cex=2,col=2)
abline(lmod)
abline(lmod1,col=2)
halfnorm(hatvalues(lmod1),ylab="Leverages")

#Add a point with high leverage, but not an outlier
p2=c(15,15.1)
lmod2<-lm(y~x,rbind(testdata,p2))
plot(y~x,rbind(testdata,p2))
points(p2[1],p2[2],pch=4,cex=2,col=2)
abline(lmod)
abline(lmod2,col=2)
halfnorm(hatvalues(lmod2),ylab="Leverages")

#Add an outlier with high leverage
#Influential, i.e. substantial effect on the analysis
p3=c(15,5.1)
lmod3<-lm(y~x,rbind(testdata,p3))
plot(y~x,rbind(testdata,p3),ylim=c(0,16))
points(p3[1],p3[2],pch=4,cex=2,col=2)
abline(lmod)
abline(lmod3,col=2)
halfnorm(hatvalues(lmod3),ylab="Leverages")
#Compare the two fits for R2, residual sd, p-value
summary(lmod)
summary(lmod3)
#Studentized residuals for lmod3
stud=rstudent(lmod3)
(tt=stud[which.max(abs(stud))])
#new alpha
alpha2=.05/n
#Calculate the p-value: 2*P(t>tt)
2*(1-pt(abs(tt),n-p-1)); alpha2  #This is an outlier


#Studentized residuals for real data
data(savings)
n=nrow(savings)
lmod<-lm(sr~pop15+pop75+dpi+ddpi,savings)
p=length(coef(lmod))
stud<-rstudent(lmod)
(tt=stud[which.max(abs(stud))])
#Bonferroni critical value
qt(1-(.05/(50*2)),50-5-1)
#Alternative
alpha2=.05/n
#Calculate the p-value: 2*P(t>tt)
2*(1-pt(abs(tt),n-p-1)); alpha2  #p-value is greater than alpha2, so not recognized as an outlier

#Multiple influential outliers hiding each other
data(star)
head(star)
?star
dim(star)
plot(light~temp,star,xlab="log(Temperature)",ylab="log(Light Intensity")
lmod<-lm(light~temp,star)
abline(lmod)
qt(1-(.05/(47*2)),47-2-1) #Bonferroni cv
range(rstudent(lmod))

#Exclude the giants
lmodi<-lm(light~temp,star,subset=(temp>3.6))
abline(lmodi,col=2,lty=2)

#Check the two fits
summary(lmod)
summary(lmodi)
###################################################################################
# 6.2.3 Influential Observations  
###################################################################################
data(savings)
lmod<-lm(sr~pop15+pop75+dpi+ddpi,savings)
summary(lmod)
cook<-cooks.distance((lmod))
plot(cook)
tail(sort(cook))
cases=which(cook>.09)
cook[cases]
#We label the 3 largest points
halfnorm(cook,3,labs=countries,ylab="cook's Distances")

#What happens if we leave the three most influential points out?
lmod2<-lm(sr~pop15+pop75+dpi+ddpi,savings[-cases,])
#Or just the most influential one
lmod3<-lm(sr~pop15+pop75+dpi+ddpi,savings,subset=(cook<max(cook)))
summary(lmod)
summary(lmod2)
summary(lmod3)
#Not a large difference