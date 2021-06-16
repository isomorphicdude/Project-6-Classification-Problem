library(MASS)
library(sparsediscrim)
library(class)
library(textreuse)
library(ggplot2)


musk.data<-read.csv("clean2.data")

musk.data1<-musk.data[,-c(1,2)]

# #normalization function
normal <- function(x) {
  return( ((x - min(x)) /(max(x)-min(x))) )
}
# 
# #normalize everything in the dataset

musk.data2<-as.data.frame(lapply(musk.data1,normal))
small.musk2<-as.data.frame(lapply(small.musk1,normal))

musk.data2<-small.musk1
#musk.data2<-musk.data1

# musk.data2<-musk.data[,-31]
# musk.data3<-musk.data[-c(61,82,1061,1088,1134,1181,1527,1611,1710,1724,1798,1920,2134,2439,2520,2583,3511,3864,4592,4887,5013,5030,5032),]

#The task is to classify the data into two categories, either musk  or non-musk
# We will compare these methods and study their advantages and disadvantages


#Before we start, shuffle and choose training and test samples
#set.seed(223)

smp_size <- floor(1 * nrow(musk.data2))

reduced_musk_ind <- sample(nrow(musk.data2), size = smp_size)#drawing numbers in the range here

reduced_musk <- as.data.frame(musk.data2[reduced_musk_ind,])


new_smp_size <- floor(0.9*nrow(reduced_musk))

# set.seed(1234)

train_ind <- sample(nrow(reduced_musk), size = new_smp_size)

train.df <- as.data.frame(reduced_musk[train_ind, ])#training set generated

test.df <- as.data.frame(reduced_musk[-train_ind, ])#minus indicates the part that has been discarded




# First we will use lda method 
f <- paste(names(train.df)[167], "~", paste(names(train.df)[-167], collapse=" + "))
f<-as.formula(f)
musk.lda<-lda(f,data=train.df)
musk.lda.predict <- predict(musk.lda, newdata = test.df)
musk.lda.predict.posteriors <- musk.lda.predict$posterior[,2]

#Calculate accuracy
table(test.df$X1. ,musk.lda.predict$class)
table(test.df$X1.,musk.lda.predict.posteriors > 0.5)
#empirical error rate
musk.pred.error <- sum(test.df$X1.!=(musk.lda.predict.posteriors > 0.5))/nrow(test.df)
print(musk.pred.error)
#A LOW ERROR RATE
#TEST IF OVERFITTING HAS OCCURRED
musk.lda.of<-predict(musk.lda,newdata=train.df)
musk.lda.of.posteriors<-musk.lda.of$posterior[,2]
musk.of.error<-sum(train.df$X1. != (musk.lda.of.posteriors > 0.5))/nrow(train.df)
print(musk.of.error)
#THE TWO ERROR RATES ARE VERY CLOSE,SO MEANS VERY GOOD CLASSIFICATION

#NOW WE USE THE KNN-METHOD, WHICH SHOULD BE WORKING BADLY DUE TO THE METRIC
k<-5
#we do something interesting here, train the model using the whole large set and test
#on the smaller dataset

#musk.knn <-  knn(train.df,test.df,train.df$X1.,k=k)
musk.knn<-knn(train.df,small.musk2,train.df$X1.,k=k)
musk.knn.cl<-as.numeric(musk.knn)-1
#musk.knn.prob <- attributes(knn(train.df,test.df,train.df$X1.,k=k,prob=TRUE))$prob
musk.knn.prob <- attributes(knn(train.df,small.musk2,train.df$X1.,k=k,prob=TRUE))$prob
musk.test.pred <- musk.knn.cl * musk.knn.prob + (1-musk.knn.cl)*(1-musk.knn.prob)
# knn.err.rate <- sum((musk.test.pred >= 0.5) != test.df$X1.)/nrow(test.df) 
knn.err.rate <- sum((musk.test.pred >= 0.5) != small.musk2$X1.)/nrow(test.df) 
knn.err.rate
#SEE IF OVERFITTING OCCURS

musk.knnof <-  knn(train.df,train.df,train.df$X1.,k=k)
musk.knnof.cl<-as.numeric(musk.knnof)-1
musk.knnof.prob <- attributes(knn(train.df,train.df,train.df$X1.,k=k,prob=TRUE))$prob
musk.of.pred <- musk.knnof.cl * musk.knnof.prob + (1-musk.knnof.cl)*(1-musk.knnof.prob)
knn.oferr <- sum((musk.of.pred >= 0.5) != train.df$X1.)/nrow(train.df)
knn.oferr
#NOW WE USE A FOR LOOP TO FIND AN OPTIMAL K
optk<-function(x){
  kvec<-numeric(x)
  ofvec<-numeric(x)
  no_of_rows<-NROW(test.df)
  for (i in 1:x){
    k<-i
    my.gr.knn <- knn(test=test.df,train=train.df,train.df$X1.,k=k)
    #kvec[i] <- 100 * sum(test.df$X1.== my.gr.knn)/no_of_rows
    my.knn.cl<-as.numeric(my.gr.knn)-1
    my.knn.prob <- attributes(knn(train.df,test.df,train.df$X1.,k=k,prob=TRUE))$prob
    my.knn.pred <- my.knn.cl * my.knn.prob + (1-my.knn.cl)*(1-my.knn.prob)
    
    kvec[i] <- sum((my.knn.pred >= 0.5) != test.df$X1.)/no_of_rows
    
    musk.knnof <-  knn(train.df,train.df,train.df$X1.,k=k)
    musk.knnof.cl<-as.numeric(musk.knnof)-1
    musk.knnof.prob <- attributes(knn(train.df,train.df,train.df$X1.,k=k,prob=TRUE))$prob
    musk.of.pred <- musk.knnof.cl * musk.knnof.prob + (1-musk.knnof.cl)*(1-musk.knnof.prob)
    knn.oferr <- sum((musk.of.pred >= 0.5) != train.df$X1.)/nrow(train.df)
    
    
    ofvec[i] <- knn.oferr
    
  }
  #Accuracy plot
  
  myvecx<-as.vector(1:x)
    
  
  knndfr<-data.frame(x=kvec,y=ofvec)
  
  gg_test <- ggplot(data=knndfr,aes(x=myvecx))+geom_line(aes(y=kvec,colour='Test'),size=1)+geom_point(aes(y=kvec,colour='Test'))+geom_line(aes(y=ofvec,colour='Train'),size=1)+geom_point(aes(y=ofvec,colour='Train'))+scale_colour_manual("",breaks = c('Test','Train'),values=c('#E69F00','#56B4E9'))
  
  
  
  p<-gg_test+theme(plot.title = element_text(hjust = 0.5))+labs(title="Error Rate vs. K ", x="K value", y="Error Rate")
  
  # p<-p+geom_point(data=dfr_train,aes(y=your_y),size=0.5,color="#56B4E9")
  print(p)
  
  
  
  
  # plot(kvec*100, type="b", xlab="K- Value",ylab="Accuracy level",col='red')
  # points(ofvec*100,type="b",col='blue')
  # 
  # legend("bottomright", legend = c("TEST", "TRAIN"),lwd = 3, col = c("red", "blue"))
  
  print(max(kvec))
  print(which.max(kvec))
  print(ofvec)
}


optk(30)
#TRY DIAGONAL DLDA METHOD ASSUMING THE ATTRIBUTES ARE UNCORRELATED DOESN'T WORK SO WELL
#WHY IS THAT? THINK AND EXPLAIN

#NOW TRY SOMETHING ELSE, LOGISTIC REGRESSION FOR TWO CLASSES SEEMS GOOD? 




