#testing for the optimal k using cross validation and brute force
#Using cross-valiadation method
library(MASS)

library(class)

library(ggplot2)

normal <- function(x) {
  return( ((x - min(x)) /(max(x)-min(x))) )
}

small.musk<-read.csv("clean1.data")
small.musk1<-small.musk[,-c(1,2)]
small.musk1<-abs(small.musk1)


#doing a 10-fold cross validation
v1<-10

smp_size1<-floor(1 * nrow(small.musk1))

set.seed(123)#original at 123 seed

small.musk1.ind<- sample(nrow(small.musk1), size = smp_size1)

small.musk1<-small.musk1[small.musk1.ind,]

small.musk1<-as.data.frame(lapply(small.musk1,normal))

v.frac<-floor(nrow(small.musk1)/v1)

reduced_musk2<-small.musk1

v.fold<-numeric(v1)

krange<-2:30

v.fold.knn.mat <- matrix(0,nrow=length(krange),ncol=v1)

knn_cv<-numeric(length(krange))
knn_cv_se<-numeric(length(krange))

knn_cv_min<-numeric(length(krange))
knn_cv_max<-numeric(length(krange))

for (i in 1:v1){
  
  v.test.ind<-((i-1)*v.frac+1):(v.frac*i)
  
  v.train.ind <- setdiff(1:nrow(reduced_musk2),v.test.ind)
  
  v.train.set <- reduced_musk2[v.train.ind,]
  
  v.test.set  <- reduced_musk2[v.test.ind, ]
  
  k.test.res <- numeric(length(krange))
  # loop over k
  for (j in krange){
    
      vmusk.knn <-  knn(v.train.set,v.test.set,v.train.set$X1.,k=j)

      vmusk.knn.cl<-as.numeric(vmusk.knn)-1

      vmusk.knn.prob <- attributes(knn(v.train.set,v.test.set,v.train.set$X1.,k=j,prob=TRUE))$prob

      vmusk.test.pred <- vmusk.knn.cl * vmusk.knn.prob + (1-vmusk.knn.cl)*(1-vmusk.knn.prob)

      vknn.err.rate <- sum((vmusk.test.pred >= 0.5) != v.test.set$X1.)/nrow(v.test.set)

      k.test.res[j-1] <- vknn.err.rate
      print(j)
      }
  
  v.fold.knn.mat[,i] <- k.test.res 
  
}

for (x in krange){
  knn_cv[x-1]<-mean(v.fold.knn.mat[x-1,])
}


for (x in krange){
  knn_cv_min[x-1]<-mean(v.fold.knn.mat[x-1,])-sqrt(var(v.fold.knn.mat[x-1,]))/sqrt(v1)
    
  knn_cv_max[x-1]<-mean(v.fold.knn.mat[x-1,])+sqrt(var(v.fold.knn.mat[x-1,]))/sqrt(v1)
# knn_cv<-1-knn_cv
}

knn_cvx<-as.vector(krange)

knn_cvdfr<-data.frame(x=knn_cvx,y=knn_cv,low=knn_cv_min,high=knn_cv_max)

ggplot(data=knn_cvdfr,aes(x=knn_cvx,y=knn_cv,colour="Error Rate"))+geom_line(size=1)+geom_point()+theme(plot.title = element_text(hjust = 0.5))+labs(title="Error Rate vs. K ", x="K value", y="Error Rate")+geom_ribbon(aes(ymin =low, ymax = high,fill="Confidence Interval"),alpha=0.3)+scale_colour_manual("",breaks = c('Error Rate'),values=c('cornflowerblue'))+scale_fill_manual(values="steelblue", name="Confidence Interval")

print(knn_cv)

#boxplot(t(v.fold.knn.mat),xlab=krange)