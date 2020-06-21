library(randomForest)
library(pROC)
load("XandY.Rdata")
x <- as.matrix(x)
ntree=50
result<-list();length(result)<-50
for(j in 1:50){
  result[[j]]<-list();length(result[[j]])<-2
}
for(m in 1:50){
  mtry <- 10+10*(m-1)
  cat(mtry,"\t")
  y.test.pred.total <- c()
  y.test.total <- c()
  tmp <- c()
  for(i in 1:10){
    test.group.id <- which(label$fold == i)
    train.group.id <- which(label$fold != i)
    x.train <- x[train.group.id,]
    y.train <- label$y[train.group.id]
    x.test <- x[test.group.id,]
    y.test <- label$y[test.group.id]
    RF.cv<-randomForest(x.train,y.train,ntree=ntree,mtry=mtry)
    y.test.pred.RF<-predict(RF.cv,x.test)
    y.test.pred.total<-c(y.test.pred.total,y.test.pred.RF)
    y.test.total <- c(y.test.total,y.test)
    tmp <- c(tmp,test.group.id)
  }
  names(y.test.pred.total)<-tmp
  names(y.test.total)<-tmp
  ROC <- roc(y.test.total,y.test.pred.total)
  
  y.test.pred.total.2<-y.test.pred.total
  y.test.pred.total.2[names(which(y.test.pred.total>=0.5))]<-1
  y.test.pred.total.2[names(which(y.test.pred.total<0.5))]<-0
  cm <- table(y.test.total,y.test.pred.total.2)
  result[[m]][[1]] <- ROC
  result[[m]][[2]] <- cm
}




