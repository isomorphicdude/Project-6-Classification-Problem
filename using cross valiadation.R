#Using cross-valiadation method
library(MASS)
library(class)

musk.small<-read.csv("clean1.data")
musk.small<-musk.small[,-c(1,2)]
musk.small<-abs(musk.small)

normal <- function(x) {
  return( ((x - min(x)) /(max(x)-min(x))) )
}

#musk.small<-as.data.frame(lapply(musk.small,normal))


#write.csv(musk.small,file="testing musk1.csv")

#musk.small<-musk.small[-c(207:269),]

#musk.small<-musk.small[-c(1:63),]

h <- paste(names(musk.small)[167], "~", paste(names(musk.small)[-167], collapse=" + "))

h<-as.formula(h)


#doing a 10-fold cross validation
v<-10

#v<-nrow(musk.small)

smp_size2<-floor(1 * nrow(musk.small))

set.seed(123)

musk.small.ind<- sample(nrow(musk.small), size = smp_size2)

musk.small<-musk.small[musk.small.ind,]


v.frac<-floor(nrow(musk.small)/v)

reduced_musk3<-musk.small
# 
v.fold2<-numeric(v)
# v.fold3<-numeric(v)
# mcnemar_vec<-numeric(v)



for (i in 1:v)
{
  k<-5
  
  v.test.ind<-((i-1)*v.frac+1):(v.frac*i)

  v.train.ind <- setdiff(1:nrow(reduced_musk3),v.test.ind)

  v.train.set <- reduced_musk3[v.train.ind,]

  v.test.set  <- reduced_musk3[v.test.ind, ]
  
  
  # vmusk.log<-glm(h,family=binomial,data=v.train.set)
  # 
  # vmusk.log.prob <- predict(vmusk.log, newdata = v.test.set, type="response")
  # 
  # vmusk.log.pred.class <- ifelse(vmusk.log.prob>0.5,"1","0")
  # 
  # 
  # vmusk.logerr<-mean(vmusk.log.pred.class!=v.test.set$X1.)
  # 
  # v.fold2[i]<-vmusk.logerr
  
  vmusk.knn <-  knn(v.train.set,v.test.set,v.train.set$X1.,k=k)

  vmusk.knn.cl<-as.numeric(vmusk.knn)-1

  vmusk.knn.prob <- attributes(knn(v.train.set,v.test.set,v.train.set$X1.,k=k,prob=TRUE))$prob

  vmusk.test.pred <- vmusk.knn.cl * vmusk.knn.prob + (1-vmusk.knn.cl)*(1-vmusk.knn.prob)

  vknn.err.rate <- sum((vmusk.test.pred >= 0.5) != v.test.set$X1.)/nrow(v.test.set)
  

  v.fold2[i]<-vknn.err.rate
  
  #Getting the vector ready for McNemar, creating a vector with 1 for equal value and 0 for unequal
  
  # vknn.err.bit<-bitwAnd(as.numeric(vmusk.test.pred>=0.5),v.test.set$X1.)
  
  


  # 
  # vfold.lda<-lda(h,data=v.train.set,subset=v.train.ind)
  # 
  # vfold.pred.lda<-predict(vfold.lda, reduced_musk3[v.test.ind,])$posterior[,2]
  # 
  # vfold.err.rate.lda<-sum((vfold.pred.lda>=0.5)!=reduced_musk3[v.test.ind,"X1."])/length(v.test.ind)
  # 
  # 
  # v.fold3[i]<-vfold.err.rate.lda
  # 
  # vlda.err.bit<-bitwAnd(as.numeric(vfold.pred.lda>=0.5),reduced_musk3[v.test.ind,"X1."])
  
  
  
  # vfold.qda<-qda(h,data=v.train.set,subset=v.train.ind)
  # 
  # vfold.pred<-predict(vfold.qda, reduced_musk3[v.test.ind,])$posterior[,2]
  # 
  # vfold.err.rate<-sum(reduced_musk3[v.test.ind,"X1."]!=(vfold.pred>0.5))/length(v.test.ind)
  # 
  # v.fold2[i]<-vfold.err.rate
  
  # no_of_common<-length(as.vector(which((vlda.err.bit+vknn.err.bit)==0)))
  # print(no_of_common)
  # no_of_errors.lda<-sum(1-vlda.err.bit)-no_of_common
  # print(no_of_errors.lda)
  # no_of_errors.knn<-sum(1-vknn.err.bit)-no_of_common
  # print(no_of_errors.knn)
  
  #Applying McNemar's test and storing values of test results in a vector
  
  # mcnemar_vec[i]<-(abs(no_of_errors.knn-no_of_errors.lda)-1)/(sqrt(no_of_errors.knn+no_of_errors.lda))

}

#using mcnemar's test to see if any significant differences 


print(v.fold2)
print(mean(v.fold2))
print(sqrt(var(v.fold2))/sqrt(v))
# print(v.fold3)
# print(mean(v.fold3))
# print(mcnemar_vec)
