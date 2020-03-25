###################################################################################
# 9.1 Transforming the Response  
###################################################################################
data(savings)
head(savings)
?savings
dim(savings)
lmod<-lm(sr~pop15+pop75+dpi+ddpi,savings)
summary(lmod)
boxcox(lmod,plotit=T) #aprros .5 and 1.5
boxcox(lmod,plotit=T,lambda=seq(.5,1.5,by=.1))
#1 is in the CI, so no reason to transform

data(gala)
head(gala)
?gala
dim(gala)
lmod<-lm(Species~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
summary(lmod)
boxcox(lmod,plotit=T)
bc=boxcox(lmod,plotit=T,lambda=seq(-.25,.75,by=.01))
boxcox(lmod,plotit=T,lambda=seq(.4,.6,by=.01))
bc$x[which.max(bc$y)] #Best transformation is the power of .32 #.3 becoasue is middle 
summary(lm(Species^.32~Area+Elevation+Nearest+Scruz+Adjacent,data=gala))


data(leafburn)
head(leafburn)
?leafburn
dim(leafburn)
lmod<-lm(burntime~nitrogen+chlorine+potassium,leafburn)
summary(lmod)
boxcox(lmod,plotit=T)
lt=boxcox(lmod,plotit=T,lambda=seq(-.75,.1,by=.01))
lt$x[which.max(lt$y)]

#0 does not lie in the CI
logtrans(lmod,plotit=T)
lt=logtrans(lmod,plotit=T,alpha=seq(-.999,0,by=.01))
lt$x[which.max(lt$y)] #Alpha equal to -.839 is optimal

summary(lm(log(burntime-.84)~nitrogen+chlorine+potassium,leafburn))
summary(lm(burntime^(-1/3)~nitrogen+chlorine+potassium,leafburn))
summary(lm(burntime^(-.32)~nitrogen+chlorine+potassium,leafburn))
###################################################################################
# 9.4 Polynomials  
###################################################################################
data(savings)
plot(sr~ddpi,savings)
lmod=lm(sr~ddpi,savings)
summary(lmod)
abline(lmod)
lmod2=lm(sr~ddpi+I(ddpi^2),savings)
summary(lmod2)
lines(0:200/10,predict(lmod2,data.frame(ddpi=0:200/10)),col=2,lty=2)
lmod3=lm(sr~ddpi+I(ddpi^2)+I(ddpi^3),savings)
summary(lmod3)
lines(0:200/10,predict(lmod3,data.frame(ddpi=0:200/10)),col=3,lty=3)
#Cube term not significant, so stick with quadratic

#Additive changes cause differences in all but the highest term
savings$mddpi=savings$ddpi-10
summary(lm(sr~mddpi+I(mddpi^2),savings))

#Use orthogonal polynomials
lmod=lm(sr~poly(ddpi,4),savings)
summary(lmod)   #This shows the quadratic model is best
summary(lm(sr~poly(ddpi,2),savings))

#Perspective plot
persp(1:10,1:10,matrix(1:100,10),theta=0,ticktype="detailed",shade=.1)
persp(1:10,1:10,matrix(1:100,10),theta=45,ticktype="detailed",shade=.1)
persp(1:10,1:10,matrix(1:100,10),theta=90,ticktype="detailed",shade=.1)
persp(1:10,1:10,matrix(1:100,10),theta=180,ticktype="detailed",shade=.1)
persp(1:10,1:10,matrix(1:100,10),theta=135,ticktype="detailed",shade=.1)
persp(1:10,1:10,matrix(1:100,10),theta=200,ticktype="detailed",shade=.1)
persp(1:10,1:10,matrix(1:100,10),theta=270,ticktype="detailed",shade=.1)

#Polynomial in two variables
lmod<-lm(sr~polym(pop15,ddpi,degree=2),savings)
summary(lmod)
#Plot using the perspective plot
pop15r<-seq(20,50,len=10)
ddpir<-seq(0,20,len=10)
pgrid<-expand.grid(pop15=pop15r,ddpi=ddpir)
pv<-predict(lmod,pgrid)
persp(pop15r,ddpir,matrix(pv,10,10),theta=30,xlab="Pop under 15",ylab="Growth",zlab="Savings Rate",
      ticktype="detailed",shade=.25)
###################################################################################
# 13.2 Deletion   
###################################################################################
data(chmiss)
chmiss
?chmiss
dim(chmiss)
summary(chmiss)
rowSums(is.na(chmiss))
sum(is.na(chmiss))

image(is.na(chmiss),axes=F,col=c(0,grey(.5)))
axis(2,at=0:5/5,labels=colnames(chmiss))
axis(1,at=0:46/46,labels=row.names(chmiss),las=2)

data(chredlin)
head(chredlin)
?chredlin
dim(chredlin)
summary(chredlin)
modfull<-lm(involact~.-side,chredlin)
summary(modfull)
#Compare to the missing values version
modmiss<-lm(involact~.,chmiss)
summary(modmiss)
###################################################################################
# 13.3 Single Imputation  
###################################################################################
(cmeans<-colMeans(chmiss,na.rm=T))
mchm<-chmiss
for (i in c(1:4,6)) mchm[is.na(chmiss[,i]),i]<-cmeans[i]
imod<-lm(involact~.,mchm)
summary(imod)

#Use regression to impute missing values for race
lmodr<-lm(race~fire+theft+age+income,chmiss)
chmiss[is.na(chmiss$race),]
predict(lmodr,chmiss[is.na(chmiss$race),])
#Cannot have negative values for race, so use logit
lmodr<-lm(logit(race/100)~fire+theft+age+income,chmiss)
ilogit(predict(lmodr,chmiss[is.na(chmiss$race),]))*100
#Compare to actual values
chredlin$race[is.na(chmiss$race)]

#Replace
chmissr=chmiss
chmissr[is.na(chmiss[,1]),1]=ilogit(predict(lmodr,chmiss[is.na(chmiss$race),]))*100

#fire
lmodr<-lm(fire~race+theft+age+income,chmiss)
chmissr[is.na(chmiss[,2]),2]=predict(lmodr,chmiss[is.na(chmiss$fire),])
#theft
lmodr<-lm(theft~race+fire+age+income,chmiss)
chmissr[is.na(chmiss[,3]),3]=predict(lmodr,chmiss[is.na(chmiss$theft),])
#age
lmodr<-lm(age~race+fire+theft+income,chmiss)
chmissr[is.na(chmiss[,4]),4]=predict(lmodr,chmiss[is.na(chmiss$age),])
#income
lmodr<-lm(income~race+fire+theft+age,chmiss)
chmissr[is.na(chmiss[,6]),6]=predict(lmodr,chmiss[is.na(chmiss$income),])

rmod<-lm(involact~.,chmissr)
summary(rmod)
###################################################################################
# 13.4 Multiple Imputation  
###################################################################################
set.seed(123)
chimp<-amelia(chmiss,m=25) #Repeat the imputation 25 times

betas<-NULL
ses<-NULL
for (i in 1:chimp$m) {
  lmod<-lm(involact~race+fire+theft+age+income,chimp$imputations[[i]])
  betas=rbind(betas,coef(lmod))
  ses=rbind(ses,coef(summary(lmod))[,2])
}

#Find the combined values
(cr<-mi.meld(q=betas,se=ses))
cr$q.mi/cr$se.mi   #t values
coef(summary(modfull))

###################################################################################
# Compare the methods
###################################################################################
#Betas
coef(modfull)  #Full model
coef(modmiss)  #Missing cases deleted
coef(imod)     #Replace missing cases with predictor mean
coef(rmod)     #Replace missing cases with predictions of regression on other predictors
cr$q.mi        #Multiple imputation
#t-values
coef(summary(modfull))[,3]
coef(summary(modmiss))[,3]
coef(summary(imod))[,3]
coef(summary(rmod))[,3]
cr$q.mi/cr$se.mi