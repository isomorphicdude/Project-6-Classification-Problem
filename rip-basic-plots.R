
# Here is an R script to reproduce the simple example from the overview. 
# This is based on Ripley's "synthetic data" - for details see Ripley (1996) Pattern Recognition and Neural Networks. Cambridge. 


# start with some required libraries 
# (install, if you don't have them. For example, use     install.packages("MASS")       )
# 
library(MASS)
library(class)
library(mvtnorm)
# 
# the two data sets are 
# 
# synth.tr
# synth.te
# 
# the former is the "training" set, the latter the "test" set.
# 
# Their dimensions?

dim(synth.tr)

dim(synth.te)


# worth looking at the data 

head(synth.te)


# let's plot the training data

plot(synth.tr[,1:2])
points(synth.tr[synth.tr$yc==1,1:2],col="red")



#lets add the optimal decision boundary for error rate, for comparison. Don't worry about the next code. Run it, but don't spend time
#making sense of it (unless you are really curious)

# make a grid to plot the decision boundary
m <- 120
x <- seq(-1.25,0.87,length=m)
y <- seq(-0.2,1.1,length=m)
gr <- expand.grid(x,y) #creating combination like the meshgrid in Python
colnames(gr) <- c("xs","ys")



# 
# evaluate the true density on the grid points. 
# the mu's are vector of means
# the sd times a diagonal matrix for covariance equals 0 (independent variable)
# for the Ripley dataset's true density

tru.dens <- function(x){
  mu11 <- c(-0.7,0.3)
  mu12 <- c(0.3,0.3)
  mu21 <- c(-0.3,0.7)
  mu22 <- c(0.4,0.7)
  sd <- (0.03)
  f1 <- 0.5*(dmvnorm(x,mu11,sd*diag(1,2))+ dmvnorm(x,mu12,sd*diag(1,2)))
  f2 <- 0.5*(dmvnorm(x,mu21,sd*diag(1,2))+ dmvnorm(x,mu22,sd*diag(1,2)))
  f2/(f2+f1)
}

# 
# 
#  add a contour plot, with the 0.5 contour.  
# 

#the real contour
my.gr.pred <- apply(gr,1,tru.dens)
my.gr.pred.mat <- matrix(my.gr.pred,ncol=m)
contour(x,y,my.gr.pred.mat,levels=0.5,add=T,col="red",lty=1,lwd=2,drawlabels=F)



# let's try 3-nearest neighbour
#  Note (not standardizing the data at this point - we will revisit that!)
#  This package is a bit fussy about how it operates, don't worry about the details quite yet. 

k <- 21

#compute k-nn on the test set

my.knn <-  knn(synth.tr[,1:2],synth.te[,1:2],synth.tr[,3],k=k)


knn.cl <- as.numeric(my.knn)-1
print(as.numeric(my.knn))
#print(knn.cl)
# extract the posterior probabilities, and compute the test set error rate
knn.prob <- (attributes(knn(synth.tr[,1:2],synth.te[,1:2],synth.tr[,3],k=k,prob=T)))$prob

#the proportion of the votes for the winning class are returned as attribute prob

#the following line calculates the probability of proportion of the votes for each class
#basically p(w_i)

my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)

#print(my.test.pred)

#the next line selects those with prob greater than a half and not equal to test set with class 1
#Is half the threshold? HALF IS CHOSEN BECAUSE THERE ARE TWO CLASSES ,THEY ADD UP TO 1

err.rate <- sum((my.test.pred >= 0.5) != synth.te$yc)/nrow(synth.te) 

my.test.pred

err.rate

# for illustration, let's compare the k=3 decision boundary with the optimal boundary. 
# Note, you can never really do this with practical problems - it is informative though. 


# evalute the predicted probabilites on the grip, extract the probabilities

my.gr.knn <- knn(synth.tr[,1:2],gr,synth.tr[,3],k=k)
my.gr.knn.cl <- as.numeric(my.gr.knn)-1
my.gr.knn.prob <- (attributes(knn(synth.tr[,1:2],gr,synth.tr[,3],k=k,prob=T)))$prob
my.gr.pred <- my.gr.knn.cl * my.gr.knn.prob + (1-my.gr.knn.cl)*(1-my.gr.knn.prob)
my.gr.pred.mat <- matrix(my.gr.pred,ncol=m)

# add the contour plot
contour(x,y,my.gr.pred.mat,levels=0.5,add=T,col="orange",lty=2,lwd=2,drawlabels=F)


# Now, 
# try playing around with different choices of k

# write a for loop that tests different values of k and get a graph

test_sp<-synth.te[,3]
train_sp<-synth.tr[,3]

train<-synth.tr[,1:2]
test<-synth.te[,1:2]

print(test_sp)

optk<-function(x){
  kvec<-as.vector(matrix(0,1,x))
  no_of_rows<-NROW(test_sp)
  for (i in 1:x){
    k<-i
    my.gr.knn <- knn(test=test,train=train,train_sp,k=k)
    kvec[i] <- 100 * sum(test_sp == my.gr.knn)/no_of_rows
    
  }
  #Accuracy plot
  plot(kvec, type="b", xlab="K- Value",ylab="Accuracy level")
}

optk(150)

# try some other classifiers


