library(e1071)
library(pROC)
load("XandYdata.Rdata")
x <- as.matrix(x)
rbf.par=c(0.00001,0.00002,0.00005,0.0001,0.0002,0.0005,0.001,0.002,0.005)
SVM.kernel="radial"
result<-list();length(result)<-length(rbf.par)
for(j in 1:length(rbf.par)){
  result[[j]]<-list();length(result[[j]])<-2
}

for(m in 1:length(rbf.par)){
  y.test.pred.total <- c()
  y.test.total <- c()
  tmp <- c()
  rbf.kpar=rbf.par[m]
  cat(rbf.kpar,"\t")
  for(i in 1:10){
    test.group.id <- which(label$fold == i)
    train.group.id <- which(label$fold != i)
    x.train <- x[train.group.id,]
    y.train <- label$y[train.group.id]
    x.test <- x[test.group.id,]
    y.test <- label$y[test.group.id]
    SVM.model<-svm(x.train,y.train,kernel=SVM.kernel,gamma=rbf.kpar)
    y.test.pred.SVM<-predict(SVM.model,x.test)
    y.test.pred.total<-c(y.test.pred.total,y.test.pred.SVM)
    y.test.total <- c(y.test.total,y.test)
    tmp <- c(tmp,test.group.id)
  }
  names(y.test.pred.total)<-tmp
  names(y.test.total)<-tmp
  ROC <- roc(y.test.total,y.test.pred.total)
  m=1
  y.test.pred.total.2<-y.test.pred.total
  y.test.pred.total.2[names(which(y.test.pred.total>=0.5))]<-1
  y.test.pred.total.2[names(which(y.test.pred.total<0.5))]<-0
  cm <- table(y.test.total,y.test.pred.total.2)
  result[[m]][[1]] <- ROC
  result[[m]][[2]] <- cm
}
save(result,file="cla_svm.Rdata")




