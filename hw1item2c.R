##Question2c
source("hw1item2b.R", local = TRUE)

#any number below 3 is unmythelated



#get intersction point of these two pdfs
intersection <- ipdRatio_pdf$x[which(diff((ipdRatio_pdf1$y - ipdRatio_pdf$y) >0) !=0) +60]



threshold <- intersection[1] 
#draw threshold line
abline(v=threshold, col='red')


legend('topleft', legend = c("mythelated", "unmethylated", "Threshold"), fill = c("green", "orange", "red"), cex = 0.75)

 
