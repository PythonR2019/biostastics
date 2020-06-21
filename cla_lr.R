
library(pROC)
load("XandYdata.Rdata")
xy <- x
xy$y <- as.factor(label$y)
#x <- as.matrix(x)
#result<-list();length(result)<-2
y.test.pred.total <- c()
y.test.total <- c()
tmp <- c()
for(i in 1:10){
  test.group.id <- which(label$fold == i)
  train.group.id <- which(label$fold != i)
  xy.train <- xy[train.group.id,]
  #x.train <- x[train.group.id,]
  #y.train <- label$y[train.group.id]
  x.test <- x[test.group.id,]
  y.test <- label$y[test.group.id]
  LR.model<-glm(y~.,data=xy.train,family = binomial(link = "logit"),control = list(1e-8,maxit=50,F))
  y.test.pred.LR<-predict(LR.model,x.test,type = "response")
  y.test.pred.total <- c(y.test.pred.total,y.test.pred.LR)
  y.test.total <- c(y.test.total,y.test)
  tmp <- c(tmp,test.group.id)
}
names(y.test.pred.total)<-tmp
names(y.test.total)<-tmp
ROC = roc(y.test.total,y.test.pred.total)
y.test.pred.total.2<-y.test.pred.total
y.test.pred.total.2[names(which(y.test.pred.total>=0.5))]<-1
y.test.pred.total.2[names(which(y.test.pred.total<0.5))]<-0
cm <- table(y.test.total,y.test.pred.total.2)
save(cm,ROC,file="cla_lr.Rdata")



