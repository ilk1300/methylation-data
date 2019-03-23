 #Question2b
 #load data
 all1= read.csv("/Users/billy/Desktop/Biomi600/known-meth.csv")
 k_meth=data.frame(all1)
 all2= read.csv("/Users/billy/Desktop/Biomi600/known-unmeth.csv")
 k_unmeth=data.frame(all2)
 
 #cleanup data
 concat=left_join(k_unmeth,k_meth)
 
 m1=filter(k_meth, meth == "TRUE")
 m2=filter(k_unmeth,unmeth=="TRUE")
 
 #plot the pdf for mythelated 
 methIPD=m1$ipdRatio
 ipdRatio_pdf <- density(methIPD, bw=.1, kernel = "rectangular", from = 0, to = 15)
  plot(ipdRatio_pdf, xlim=c(0,15), ylim=c(0.00, 2.25), col='green', xlab = "ipdRatio", main = "Conditional Probability Estiamtion\n bw=.1, uniform kernels")
 
 par(new=TRUE)
 
 #and for unmythelated
 unmethIPD=m2$ipdRatio
 ipdRatio_pdf1 <- density(unmethIPD, bw=0.1, kernel = "rectangular", from = 0, to = 15)
 plot(ipdRatio_pdf1, xlim=c(0,15), ylim=c(0.00, 2.25), col='blue', xlab = "ipdRatio", main = "Conditional Probability Estiamtion\n bw=.1, uniform kernels")
 
 legend('topright', legend = c("methylated", "unmethylated"), fill = c("green", "blue"), cex = 0.75)