

# normal based methods on Ripley data
library(MASS)
library(class)

# compute LDA
#By default, the prior probability is set to be the proportion
#the yc~ is the group we wish to classify, note that it must be categorical data
rip.lda <- lda(yc~.,data=synth.tr)

# careful, different classifiers provide output in different forms. Here, getting the posterior probabilities
#posterior probabilities is a list with two columns, here the second class is extracted

rip.lda.pred <- predict(rip.lda,synth.te)$posterior[,2]

#confusion matrix, test set
#the greater than 0.5 acts as a classifier in some sense that it divides 
#those posterior with data plugged into true (greater than half that it's in the second class)
#and false (vice versa), 

table(synth.te$yc,rip.lda.pred > 0.5)
rip.lda.test.err <- sum(synth.te$yc != (rip.lda.pred > 0.5))/nrow(synth.te)

print(rip.lda.test.err)


#compute QDA
rip.qda <- qda(yc~.,data=synth.tr)
rip.qda.pred <- predict(rip.qda,synth.te)$posterior[,2]
#confusion matrix,test set
table(synth.te$yc,rip.qda.pred > 0.5)
rip.qda.test.err <- sum(synth.te$yc != (rip.qda.pred > 0.5))/nrow(synth.te)


# recall the comments about in-sample assessment

rip.qda.train.pred <- predict(rip.qda,synth.tr)$posterior[,2]
rip.lda.train.pred <- predict(rip.lda,synth.tr)$posterior[,2]

#confusion matrix, training set
table(synth.tr$yc,rip.lda.train.pred > 0.5)
rip.lda.train.err <- sum(synth.tr$yc!=(rip.lda.train.pred > 0.5))/nrow(synth.tr)

table(synth.tr$yc,rip.qda.train.pred > 0.5)
rip.qda.train.err <- sum(synth.tr$yc!=(rip.qda.train.pred > 0.5))/nrow(synth.tr)


# aside: some plots

m <- 120
x <- seq(-1.25,0.87,length=m)
y <- seq(-0.2,1.1,length=m)
gr <- expand.grid(x,y)
colnames(gr) <- c("xs","ys")

my.mod <- rip.lda
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)

plot(synth.tr[,1:2])
points(synth.tr[synth.tr$yc==1,1:2],col="red")

contour(x,y,my.gr.pred,levels=0.5,add=T,col="blue")

# now, compute for QDA
my.mod <- rip.qda
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="green")



# add polynomial transformations

rip.lda.poly <- lda(yc~poly(xs,3)+poly(ys,3),data=synth.tr)
rip.qda.poly <- qda(yc~poly(xs,3)+poly(ys,3),data=synth.tr)

# now, plot these decisions boundaries

plot(synth.tr[,1:2])
points(synth.tr[tr.cl,1:2],col="red")

my.mod <- rip.lda.poly
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="blue")
# now, compute for QDA
my.mod <- rip.qda.poly
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="green")



# reduced sample example

idx <- c(sample(1:125,10,replace=F),sample(126:250,10,replace=F)  )
rip.lda.poly.subsamp <- lda(yc~poly(xs,3)+poly(ys,3),data=synth.tr,subset=idx)
rip.qda.poly.subsamp <- qda(yc~poly(xs,3)+poly(ys,3),data=synth.tr,subset=idx)


 # now, plot these decisions boundaries

plot(synth.tr[,1:2])
points(synth.tr[tr.cl,1:2],col="red")
 
my.mod <- rip.lda.poly.subsamp
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="blue")
# now, compute for QDA
my.mod <- rip.qda.poly.subsamp
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="green")

 
# lets take a bunch or draws (code modified from above)

reps <- 10
ssize <- 100
plot(synth.tr[,1:2])
points(synth.tr[tr.cl,1:2],col="red")

for (i in 1:reps){
  idx <- c(sample(1:125,ssize,replace=F),sample(126:250,ssize,replace=F)  )
  rip.lda.poly.subsamp <- lda(yc~poly(xs,3)+poly(ys,3),data=synth.tr,subset=idx)
  rip.qda.poly.subsamp <- qda(yc~poly(xs,3)+poly(ys,3),data=synth.tr,subset=idx)
  my.mod <- rip.lda.poly.subsamp
  my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
  contour(x,y,my.gr.pred,levels=0.5,add=T,col="blue")
  my.mod <- rip.qda.poly.subsamp
  my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
  contour(x,y,my.gr.pred,levels=0.5,add=T,col="green")
}






