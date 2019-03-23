#Question 2a

#concate
all1= read.csv("/Users/billy/Desktop/Biomi600/known-meth.csv")
k_meth=data.frame(all1)
all2= read.csv("/Users/billy/Desktop/Biomi600/known-unmeth.csv")
 k_unmeth=data.frame(all2)
 concat=left_join(k_unmeth,k_meth)

#clean up data
concat1=unlist(concat)
concat1=na.omit(concat1)

#use density function to plot pdf Testing
initial_ipd_pdf <- density(c, bw = 1.5, kernel = "rectangular")

#plot a density which has the true distribution to help decide bandwidth (optimize)
initial_ipd_pdf <- density(log10(concat1), bw = .5, kernel = "rectangular")

#plot pdf, specify x label and main title
plot(initial_ipd_pdf, xlim=c(0,25), xlab = "%ipd", main="Parzen Density Estimation\n  bw=0.5, unifrom kernels")