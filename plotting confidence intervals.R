#adding segments to the plot
# & Error rate (SE) \\ \hline
# LDA & 0.200 (0.022) \\ \hline
# KNN (k=5) &0.043 (0.008) \\ \hline
# LR  &0.238 (0.013)  \\ \hline 
# KNN (k=7) & 0.051 (0.012) \\ \hline
# 
# \end{tabular}
# \captionof{table}{10-fold CV}

plot(0, 0, col = "white", xlab = "", ylab = "",xlim=c(0,0.3),ylim=c(0,5),main="Error rate 10-fold CV")
#LDA
segments(x0 = 0.2-0.022, y0 = 1, x1 = 0.2+0.022, y1 = 1, col = "cornflowerblue",lwd = 5)
abline(v=0.2,col='cornflowerblue',lwd=2)
#KNN K=5
segments(x0 = 0.043-0.008, y0 = 2, x1 = 0.043+0.008, y1 = 2, col = "chocolate1",lwd = 5)
abline(v=0.043,col='chocolate1',lwd=2)
#LR
segments(x0 = 0.238-0.013, y0 = 3, x1 = 0.238+0.013, y1 = 3, col = "chartreuse4",lwd = 5)
abline(v=0.238,col='chartreuse4',lwd=2)
#KNN K=7
segments(x0 = 0.051-0.012, y0 = 4, x1 = 0.051+0.012, y1 = 4, col = "cadetblue",lwd = 5)
abline(v=0.051,col='cadetblue',lwd=2)
legend(cex=0.7,xpd=TRUE,inset=c(0,0),"bottomright",legend=c("LDA","k=5","LR","k=7"),col=c('cornflowerblue','chocolate1','chartreuse4','cadetblue'),pch=15)

# \begin{tabular}{|c|c|} %doing leave one out cross validation
# \hline
# & Error rate (SE) \\ \hline
# LDA & 0.185 (0.018) \\ \hline
# KNN (k=5) &0.051 (0.010) \\ \hline
# LR  & 0.223 (0.019)  \\ \hline 
# KNN (k=7) & 0.061 (0.011) \\ \hline
# 
# \end{tabular}
# \captionof{table}{LOOCV}

plot(0, 0, col = "white", xlab = "", ylab = "",xlim=c(0,0.3),ylim=c(0,5),main="Error rate LOOCV")
#LDA
segments(x0 = 0.185-0.018, y0 = 1, x1 = 0.185+0.018, y1 = 1, col = "cornflowerblue",lwd = 5)
abline(v=0.185,col='cornflowerblue',lwd=2)
#KNN K=5
segments(x0 = 0.051-0.008, y0 = 2, x1 = 0.051+0.008, y1 = 2, col = "chocolate1",lwd = 5)
abline(v=0.051,col='chocolate1',lwd=2)
#LR
segments(x0 = 0.223-0.019, y0 = 3, x1 = 0.223+0.019, y1 = 3, col = "chartreuse4",lwd = 5)
abline(v=0.223,col='chartreuse4',lwd=2)
#KNN K=7
segments(x0 = 0.061-0.011, y0 = 4, x1 = 0.061+0.011, y1 = 4, col = "cadetblue",lwd = 5)
abline(v=0.061,col='cadetblue',lwd=2)
legend(cex=0.7,xpd=TRUE,inset=c(0,0),"bottomright",legend=c("LDA","k=5","LR","k=7"),col=c('cornflowerblue','chocolate1','chartreuse4','cadetblue'),pch=15)












