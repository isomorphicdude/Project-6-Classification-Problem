#wbdc Breast Cancer Prediction from Wisconsin
library(MASS)

wdbc<-read.csv("wdbc.data",header=FALSE)

dim(wdbc)

#Initializing the dataset as on the website

#Assigning names to the columns

features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")

names(wdbc) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))


wdbc.data <- as.matrix(wdbc[,c(3:32)])
dim(wdbc.data)

#Replacing individual (row names) with their ids

row.names(wdbc.data) <- wdbc$id

#Replacing the BENIGN ONES BY 0 AND MALIGNANT ONES BY 1

wdbc$diagnosis<-replace(wdbc$diagnosis,which(wdbc$diagnosis=="B"),0)

wdbc$diagnosis<-replace(wdbc$diagnosis,which(wdbc$diagnosis=="M"),1)


wdbc_raw <- cbind(wdbc.data, as.numeric(wdbc$diagnosis))

#head(wdbc_raw)

colnames(wdbc_raw)[31] <- "diagnosis"

#Using the sample function in R to randomly select (a different way)

set.seed(2345)

smp_size_raw <- floor(0.75 * nrow(wdbc_raw))#taking 75percent of the dataset for training

train_ind_raw <- sample(nrow(wdbc_raw), size = smp_size_raw)#drawing numbers in the range here

train_raw.df <- as.data.frame(wdbc_raw[train_ind_raw, ])#training set generated

test_raw.df <- as.data.frame(wdbc_raw[-train_ind_raw, ])#minus indicates the part that has been discarded

#Using the LDA but note how repeatedly writing the names is avoided here
#First the category and then the factors with plus signs in between
f <- paste(names(train_raw.df)[31], "~", paste(names(train_raw.df)[-31], collapse=" + "))
#linear discriminant
wdbc_raw.lda <- lda(as.formula(f), data = train_raw.df)
#prediction
wdbc_raw.lda.predict <- predict(wdbc_raw.lda, newdata = test_raw.df)
# Get the posteriors as a dataframe.
wdbc_raw.lda.predict.posteriors <- as.data.frame(wdbc_raw.lda.predict$posterior)

wdbc_raw.lda.predict.class<-wdbc_raw.lda.predict$class

table(test_raw.df$diagnosis,wdbc_raw.lda.predict.class)
