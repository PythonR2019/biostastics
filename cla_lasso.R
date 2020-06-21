library(glmnet)
library(pROC)
load("XandY.Rdata")
x <- as.matrix(x)

sum_y_total <- rep(0,5974)
for(m in 1:10){
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
    Lasso.cv<-cv.glmnet(x.train,y.train)
    bestlambda<-Lasso.cv$lambda.min
    Lasso.model<-Lasso.cv$glmnet.fit
    y.test.pred.Lasso<-predict(Lasso.model,newx=x.test,s=bestlambda)
    y.test.pred.total<-c(y.test.pred.total,y.test.pred.Lasso)
    y.test.total <- c(y.test.total,y.test)
    tmp <- c(tmp,test.group.id)
  }
  sum_y_total <- sum_y_total + y.test.pred.total
  names(y.test.pred.total)<-tmp
  names(y.test.total)<-tmp
}
y.test.pred.total.mean <- sum_y_total/10
ROC <- roc(y.test.total,y.test.pred.total.mean)

y.test.pred.total.mean.2<-y.test.pred.total.mean
y.test.pred.total.mean.2[names(which(y.test.pred.total.mean>=0.5))]<-1
y.test.pred.total.mean.2[names(which(y.test.pred.total.mean<0.5))]<-0
cm <- table(y.test.total,y.test.pred.total.mean.2)
save(ROC, cm, file="Ridge.Rdata")





