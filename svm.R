setwd("/Users/tbecker/Documents/Projects/GitHubProjects/DiatomTPMetrics/data")

library(ggplot2)
library(e1071)

IND <-  read.csv ("DiatomMetrics_GAM.csv",sep=",",header=TRUE)
IND[is.na(IND)] <- 0
IND$TPGrp<-factor(ifelse(IND$TP_MGL<0.065,"low","high"))

ggplot(IND,aes(L,H))+
  geom_point(aes(colour=factor(TPGrp)))+
  xlim(0,1)+
  ylim(0,1)+
  theme(legend.position=c(0.75,0.75))+
  geom_abline(intercept = l1,slope=l2)

x<-as.matrix(IND[,c(5:6)])
y<-IND[,8]
m<-IND[,c(5,6,8)]

##Run Linear SVM Model
model<-svm(TPGrp~.,data=m,kernel="linear",cost=10,scale=FALSE) 
pred<-predict(model,x)

##Get the slope and intercept of the line
beta<- drop(t(model$coefs)%*%x[model$index,])
beta0 <- model$rho

l1<-(beta0 / beta[2])
l2<-(-beta[1] / beta[2])