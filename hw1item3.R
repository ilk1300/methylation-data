#Question 3

#build a vector of true classifications based on 15% criteria
all1= read.csv("/Users/billy/Desktop/Biomi600/known-meth.csv", header = TRUE)
 k_meth=data.frame(all1)
 all2= read.csv("/Users/billy/Desktop/Biomi600/known-unmeth.csv", header = TRUE)
 k_unmeth=data.frame(all2)
 
 #cleanup data
 concat=left_join(k_unmeth,k_meth)
 m1=filter(k_meth, meth == "TRUE")
 m2=filter(k_unmeth,unmeth=="TRUE")



#calculate accuracy
cal_accuracy <- function(para_model) {
  right_count=0
  for (i in 2:209793) {
    if(para_model[i] == true_class[i]) {right_count = right_count +1}
  }
  accuracy <- right_count / 209793
  return(accuracy)
}

# empty matrix for parameter names and their accuracy
para_rank <- matrix(nrow = 4, ncol = 2)
colnames(para_rank) <- c("parameter", "accuracy")


#parameter1: tpl
para_rank[[1,1]] <- c('tpl')  #append parameter name
tpl <- concat$tpl  #get tpl values
#plot the pdf for tpl values given it has been methylated

M_tpl=unlist(m1["tpl"])
M_tpl_pdf <- density(M_tpl, kernel = "gaussian", from=6000, to=4405641)

plot(M_tpl_pdf, ylim=c(0.0000,0.0000003), col='lawngreen', xlab='tpl', main = "Conditional Probability Estiamtion")
par(new=TRUE)

#plot the pdf for tpl values given it's a unmeth
UM_tpl = unlist(m2["tpl"])
UM_tpl_pdf <- density(UM_tpl, kernel = "gaussian", from=6000, to=4405641)

plot(UM_tpl_pdf, ylim=c(0.0000,0.0000003), col='tomato', xlab='tpl', main = "Conditional Probability Estiamtion")


legend('topright', legend = c("meth", "unmeth", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

#get threshold for classification
threshold_tpl <- UM_tpl_pdf$x[which(diff((UM_tpl_pdf$y - M_tpl_pdf $y) >0) !=0) +1]
abline(v=threshold_tpl, col='royalblue')
#build a vector of threshold classification
tpl_model <- c()
for (i in tpl) {
  if(i >= threshold_tpl) {tpl_model <- c(tpl_model, 'M')}
  else {tpl_model <- c(tpl_model, 'UM')}
}

tpl_accuracy <- cal_accuracy(tpl_model)  #get accuracy from function: cal_accuracy
para_rank[[1,2]] <- c(tpl_accuracy)  #append to matrix with parameter accuracy


#parameter2: strand
 para_rank[[2,1]] <- c('strand')  #append parameter name
strand <- concat$strand  #get strand values
#plot the pdf for strand values given it has been methylated

M_strand=unlist(m1["strand"])
M_strand_pdf <- density(M_tpl, kernel = "gaussian", from=0, to=1)

plot(M_strand_pdf, ylim=c(0.0,3.0), col='lawngreen', xlab='strand', main = "Conditional Probability Estiamtion")
par(new=TRUE)

#plot the pdf for tpl values given it's a unmeth
UM_strand = unlist(m2["strand"])
UM_strand_pdf <- density(UM_strand, kernel = "gaussian", from=0, to=1)

plot(UM_strand_pdf, ylim=c(0.0,3.0), col='tomato', xlab='strand', main = "Conditional Probability Estiamtion")
legend('topright', legend = c("meth", "unmeth", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

#get threshold for classification
threshold_strand <- UM_strand_pdf$x[which(diff((UM_strand_pdf$y - M_strand_pdf $y) >0) !=0) +1]
abline(v=threshold_strand, col='royalblue')
#build a vector of threshold classification
strand_model <- c()
for (i in tpl) {
  if(i >= threshold_strand) {strand_model <- c(strand_model, 'M')}
  else {strand_model <- c(strand_model, 'UM')}
}

strand_accuracy <- cal_accuracy(strand_model)
para_rank[[2,2]] <- c(strand_accuracy)

#---param3: score

para_rank[[3,1]] <- c('score')  #append parameter name
score <- concat$score 

#plot the pdf for strand values given it has been methylated

M_score=unlist(m1["score"])
M_score_pdf <- density(M_score, kernel = "gaussian", from=0, to=100)

plot(M_score_pdf, ylim=c(0.0,100.0), col='lawngreen', xlab='score', main = "Conditional Probability Estiamtion")
par(new=TRUE)

#plot the pdf for values given it's a unmeth
UM_score = unlist(m2["score"])
UM_score_pdf <- density(UM_score, kernel = "gaussian", from=0, to=100)

plot(UM_score_pdf, ylim=c(0.0,100), col='tomato', xlab='score', main = "Conditional Probability Estiamtion")
legend('topright', legend = c("meth", "unmeth", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

#get threshold for classification
threshold_score <- UM_score_pdf$x[which(diff((UM_score_pdf$y - M_score_pdf $y) >0) !=0) +1]
abline(v=threshold_score, col='royalblue')
#build a vector of threshold classification
score_model <- c()
for (i in tpl) {
  if(i >= threshold_score) {score_model <- c(score_model, 'M')}
  else {score_model <- c(score_model, 'UM')}
}

score_accuracy <- cal_accuracy(score_model)
para_rank[[2,2]] <- c(score_accuracy)

#-----param4: ipdRatio
para_rank[[4,1]] <- c('ipd')  #append parameter name
ipd <- concat$ipdRatio
M_ipd=unlist(m1["ipdRatio"])
M_ipd_pdf <- density(M_ipd, kernel = "gaussian", from=0, to=15)

plot(M_ipd_pdf, ylim=c(0.00,1), col='lawngreen', xlab='ipd', main = "Conditional Probability Estiamtion")

par(new=TRUE)
UM_ipd=unlist(m2["ipdRatio"])
UM_ipd_pdf <- density(UM_ipd, kernel = "gaussian", from=0, to=15)
plot(UM_ipd_pdf, ylim=c(0.00,1), col='tomato', xlab='ipd', main = "Conditional Probability Estiamtion")

legend('topright', legend = c("meth", "unmeth", "Threshold"), fill = c("lawngreen", "tomato", "royalblue"), cex = 0.75)

threshold_ipd <- UM_ipd_pdf$x[which(diff((UM_ipd_pdf$y - M_ipd_pdf$y) >0) !=0) +60]
abline(v=threshold_ipd, col='royalblue')

ipd_model <- c()
for (i in ipd) {
  if(i >= threshold_ipd) {ipd_model <- c(ipd_model, 'M')}
  else {ipd_model <- c(ipd_model, 'UM')}
}
ipd_accuracy <- cal_accuracy(ipd_model)
para_rank[[10,2]] <- c(ipd_accuracy)

#Sort 
sorted_para_rank <- para_rank[order(para_rank[,2], decreasing=TRUE),] 