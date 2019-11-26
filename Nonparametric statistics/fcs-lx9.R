#����k���ڽ��з���
library(class)
attach(iris)
train <- iris[,1:2]
y <- as.numeric(Species)
x <- train
fit <- knn(x,x,y)
1-sum(y==fit)/length(y)

#����������
library(rpart)
attributes(Titanic)
x=Titanic
names(x)
fit = rpart(Survived~.,x,method = "class")
y.pr=predict(fit,x)
yhat=ifelse(y.pr[,1]>0.5,1,0)
table(yhat,x)
plot(fit,asp=3)
text(fit,use.n=T,cex=0.6)
print(fit)

#AdaBoost�㷨
library(adabag)
library(rpart)
library(mlbench)
data(BreastCancer)
set.seed(12345)
sa=sample(1:length(BreastCancer[,1]),replace=T)
train = BreastCancer[unique(sa),-1]
test = BreastCancer[-unique(sa),-1]
#sa = sample(1:length(BreastCancer[,1]),replace = F)
#train = BreastCancer[unique(sa),-1]
#test = BreastCancer[-unique(sa),-1]
#a=rep(0,10)
#for(i in seq(10,100,10))
#{
  BC.adaboost = boosting(Class~.,data=train,mfinal=10,maxdepth=3);
  BC.adaboost.pred=predict.boosting(BC.adaboost,test);
  #table(BC.adaboost.pred,train$Class)
 #a[i/10]=BC.adaboost.pred$erro;
#}
#plot(a,type="o",main="Adaboost",xlab="the number of iterative",ylab="test error")

  
  
#AadBoost�㷨
set.seed(1044)  #�趨�������
samp=c(sample(1:50,25),sample(51:100,25),sample(101:150,25)) #�����������
a=boosting(Species~.,data=iris[samp,]) #����ѵ��������adaboost����ģ
(z0=table(iris[samp,5],predict(a,iris[samp,])$class))  #�鿴ģ��ѵ������Ԥ����
(z1=table(iris[-samp,5],predict(a,iris[-samp,])$class))  #�鿴ģ�Ͳ��Լ���Ԥ����
#bagging�㷨
set.seed(1044)  #�趨�������
samp=c(sample(1:50,25),sample(51:100,25),sample(101:150,25)) #�����������
b=bagging(Species~.,data=iris[samp,])  #����ѵ��������bagging����ģ��
(z0=table(iris[samp,5],predict(b,iris[samp,])$class))  #�鿴ģ��ѵ������Ԥ����
(z1=table(iris[-samp,5],predict(b,iris[-samp,])$class))  #�鿴ģ��Ԥ�⼯��Ԥ����





#֧��������
install.packages("e1071")
library(e1071)
data("iris")
x=iris[51:150,c(3,4,5)]
x[,3]=as.character(x[,3])
x[,3]=as.factor(x[,3])
iris.svm = svm(Species~.,data=x)
plot(iris.svm,x,Petal.Width~Petal.Length)


#���ɭ��������
install.packages("randomForest")
library(randomForest)
data(iris)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3)) 
trainData <- iris[ind==1,]  
testData <- iris[ind==2,]  
iris.rf <- randomForest(Species~.,data=trainData,ntree=100)
iris.pred <- predict(iris.rf)
table(iris.pred,trainData$Species)

#MARS��Ԫ����Ӧ�����ع�
library(mda)
library(class)
data(trees)
fit1 <- mars(trees[,-3],trees[3])
showcuts <- function(obj){
  tmp <- obj$cuts[obj$sel,]
  dimnames(tmp) <- list(NULL,names(trees)[-3])
  tmp
}
showcuts(fit1)