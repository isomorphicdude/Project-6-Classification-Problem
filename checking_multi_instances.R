#checking repetitions and multiple instances

library(cluster)
library(gplots)
library(ggplot2)

#first define the 0 vector

multi_inst<-numeric(166)

for (x in 1:166){
  
  num_inst<-length(unique(small.musk1[,x]))
  
  multi_inst[x]<-num_inst
  
}

#print(multi_inst)
multi_inst_x<-as.vector(1:166)
multi_inst_dfr<-data.frame(x=multi_inst_x,y=multi_inst)

#ggplot(data=multi_inst_dfr,aes(x=multi_inst_x,y=multi_inst,colour="Instances"))+geom_line()+geom_point()+theme(plot.title = element_text(hjust = 0.5))+labs(title="No. of instances", x="Column number", y="Instances")+scale_colour_manual("",breaks = c('Instances'),values=c('#E69F00'))
ggplot(data=multi_inst_dfr,aes(x=multi_inst_x, y=multi_inst))+geom_bar(stat="identity", fill="steelblue")+labs(title="No. of instances", x="Column number", y="Instances")+theme(plot.title = element_text(hjust = 0.5))

#creating an array, where 1 denotes the entries that are equal, and 0 denotes those are different
#Instead we will use the kmeans method to  identify an underlying (natural) classification
# lvec<-numeric(0)
# 
# for (c in 2:20){
#   
#   k_means<-kmeans(musk.data2,centers = c, algorithm = "Lloyd", iter.max=50)
#   musk2_cluster<-k_means$cluster
#   lvec[c]<-sum(musk.data2[,167]==(musk2_cluster-1))/nrow(musk.data2)
#   
# }
# 
# plot(lvec)
# 
# k_means<-kmeans(musk.data2,centers = rbind(musk.data2[1017,],musk.data2[1,]),iter.max=100)
# musk2_cluster<-k_means$cluster
# sum(musk.data2[,167]==(musk2_cluster-1))/nrow(musk.data2)
#[1] 0.8459906


musk.data<-read.csv("clean2.data")
musk.data1<-musk.data[,-c(1,2)]
small.musk1<-small.musk[,-c(1,2)]
# #normalization function
normal <- function(x) {
  return( ((x - min(x)) /(max(x)-min(x))) )
}
# 
# #normalize everything in the dataset

small.musk1<-as.data.frame(lapply(small.musk1,normal))

smp_size <- floor(0.01 * nrow(musk.data2))

reduced_musk_ind <- sample(nrow(musk.data2), size = smp_size)#drawing numbers in the range here

reduced_musk <- as.data.frame(musk.data2[reduced_musk_ind,])

multi_dist<-dist(small.musk1[,-167])

filled.contour(as.matrix(multi_dist),main="Similarity between instances")

diff_vec<-numeric(0)
diff_vecx<-as.vector(1:166)
for (x in 1:166){
  print(max(small.musk1[,x])-min(small.musk1[,x]))
  diff_vec[x]<-max(small.musk1[,x])-min(small.musk1[,x])
  
}
diff_dfr<-data.frame(x=diff_vecx,y=diff_vec)

ggplot(diff_dfr,aes(y=diff_vec))+geom_boxplot(color="navy",fill="steelblue",lwd=1.8)+labs(title="Range of Features", y="Values in Angstroms")+theme(plot.title = element_text(hjust = 0.5))+coord_flip()

ggplot(diff_dfr,aes(x=diff_vecx,y=diff_vec))+geom_point(color="steelblue")+labs(title="Range of Features", x="Column Index",y="Values in Angstroms")+theme(plot.title = element_text(hjust = 0.5))
