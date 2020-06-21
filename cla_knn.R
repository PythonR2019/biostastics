library(class)
library(pROC)
load("XandYdata.Rdata")
#xy <- x
#xy$y <- label$y
x <- as.matrix(x)
result<-list();length(result)<-2
#for(j in 1:length(rbf.par)){
# result[[j]]<-list();length(result[[j]])<-2
#}

for(k in c(35,45,55)){
 cat(k,"\t")
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
   y.test.pred.KNN<-as.numeric(knn(x.train,x.test,y.train,k=k))-1
   y.test.pred.total<-c(y.test.pred.total,y.test.pred.KNN)
   y.test.total <- c(y.test.total,y.test)
   tmp <- c(tmp,test.group.id)
 }
 names(y.test.pred.total)<-tmp
 names(y.test.total)<-tmp
 ROC = roc(y.test.total,y.test.pred.total)
 k=1
 y.test.pred.total.2<-y.test.pred.total
 y.test.pred.total.2[names(which(y.test.pred.total>=0.5))]<-1
 y.test.pred.total.2[names(which(y.test.pred.total<0.5))]<-0
 cm <- table(y.test.total,y.test.pred.total.2)
 result[[k]][[1]] <- ROC
 result[[k]][[2]] <- cm
}
save(result,file="cla_knn.Rdata")



