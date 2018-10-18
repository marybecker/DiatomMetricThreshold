setwd("/Users/tbecker/Documents/Projects/GitHubProjects/DiatomMetricThreshold/data")

library(ggplot2)
library(e1071)

IND <-  read.csv ("DiatomMetrics_GAM.csv",sep=",",header=TRUE)
IND<-IND[IND$ID!='S18882_2015',]
IND[is.na(IND)] <- 0
IND$TPGrp<-factor(ifelse(IND$TP_MGL<0.065,"low","high"))
IND$TPGrp3<-factor(ifelse(IND$TP_MGL<=0.02,"low",
                          ifelse(IND$TP_MGL>0.065,"high","medium")))
IND$R<-(IND$H*10)/(IND$H+IND$L)
IND$SH<-IND$H*10

INDT <-  read.csv ("DiatomMetrics_2017.csv",sep=",",header=TRUE)
INDT[is.na(INDT)] <- 0
INDT$TPGrp<-factor(ifelse(INDT$TP_MGL<0.065,"low","high"))
INDT$R<-(INDT$H*10)/(INDT$H+INDT$L)
INDT$SH<-INDT$H*10

x<-as.matrix(IND[,c(5,6)])
y<-IND[,8]
m<-IND[,c(5,6,8)]

##Run Linear SVM Model
model<-svm(TPGrp~.,data=m,kernel="linear",cost=1,scale=FALSE,
           class.weights=(c("high"=3,"low"=1))) 
pred<-predict(model,m)
table(pred=pred,true=m$TPGrp)# confusion matrix

##Run Test Dataset
m2<-INDT[,c(5,6,7)]
pred2<- predict(model,m2)
table(pred=pred2,true=m2$TPGrp)# confusion matrix

##Run Linear SVM Model with Bootstrap

n<-999
lineoutput<- matrix(nrow=n,ncol=4)
confoutput<- matrix(nrow=n,ncol=4)

for(k in 1:n) {

  r<-sample(nrow(IND),size=138,replace=TRUE)
  m<-IND[r,]
  m<-m[,c(5,6,8)]

  model<-svm(TPGrp~.,data=m,kernel="linear",cost=1,scale=FALSE,
  class.weights=(c("high"=4,"low"=1)))
  pred<-predict(model,m)
  hh<-table(pred=pred,true=m$TPGrp)[1]# confusion matrix
  hl<-table(pred=pred,true=m$TPGrp)[2]
  lh<-table(pred=pred,true=m$TPGrp)[3]
  ll<-table(pred=pred,true=m$TPGrp)[4]
  confoutput[k,]<-as.matrix(cbind(hh,hl,lh,ll))

}

cfotp<-as.data.frame(confoutput)
cfotp$PctHH<-cfotp$V1/(cfotp$V1+cfotp$V2)

##Get the slope and intercept of the hyperplane (line)
##and the upper margin and the lower margin of the decision boundary
beta<- drop(t(model$coefs)%*%x[model$index,])
beta0 <- model$rho

int<-(beta0 / beta[1]) #line intercept
sl<-(-beta[2] / beta[1]) # line slope
int1<-(beta0 - 1) / beta[1] #lower decision boundary
int2<-(beta0 + 1) / beta[1] #upper decision boundary

mindx<-as.data.frame(x[model$index,])#support points that determine the boundary

##Plot the hyperplane
ggplot()+
  geom_point(data=IND,aes(H,L,colour=factor(TPGrp)))+
  #geom_point(data=mindx,aes(H,L),shape=21,size=5)+
  #support points that determine the boundary
  xlim(0,1)+
  ylim(0,1)+
  theme(legend.position=c(0.75,0.75))+
  geom_abline(intercept = int,slope=sl)+
  geom_abline(intercept=int1,slope=sl,linetype=2)+
  geom_abline(intercept=int2,slope=sl,linetype=2)+
  labs(x="Rel Abund Tolerant Diatoms",
       y="Rel Abund Senstitive Diatoms",
       colour="TP GRP")




ggplot(IND,aes(H,TP_MGL))+
  geom_point(aes(colour=factor(TPGrp)))+
  scale_y_log10()+
  abline(intercept=beta0/beta,slope=- beta)

IND$RPred<-ifelse(IND$H>=0.25,"high","low")
table(pred=IND$RPred,true=IND$TPGrp)

INDT$RPred<-ifelse(INDT$H>=0.25,"high","low")
table(pred=INDT$RPred,true=INDT$TPGrp)
