#Question4

source("hw1item4.R", local = TRUE)

#Prediction function when k=1
k1_prediction <- function(v_par, k1_class) {
  k1_class <- c()
  for (i in 2:209793) {
    test <- matrix(v_par[i], nrow=209791, ncol=1)  #take one out as test data
    train <- matrix(v_par[-i], nrow=209791, ncol=1)  #set the rest as training data
    ipd <- matrix(m2[-i]) #build a matrix with the unmethylated values of training set
    diffs <- abs(sweep(test,1,train,"-"))  #get absolute difference value between test data and each training data
    com_train_diffs <- cbind(train,diffs,ipd)  #combine training data, absolute difference and final ipd values into one matrix 
    sorted_com <- com_train_diffs[order(com_train_diffs[,2]),]  #and sort from low to high according to absolute difference
    ipd_nn <- sorted_com[1,3] #get the ipd value from one nearest neighbour
    if(ipdRatio_nn >= 15) {k1_class <- c(k1_class, 'M')}  # classify test data using nearest neighbour based on 15% criteria
    else {k1_class <- c(k1_class, 'UM')}  
  }
  return(k1_class)  #ratio
}

#Prediction function when k=2
k2_prediction <- function(v_par, k2_class) {
  k2_class <- c()
  for (i in 2:209793) {
    test <- matrix(v_par[i], nrow= 209791, ncol=1)
    train <- matrix(v_par[-i], nrow= 209791, ncol=1)
    ipd <- matrix(m2[-i])
    diffs <- abs(sweep(test,1,train,"-"))
    com_train_diffs <- cbind(train,diffs,ipd)
    sorted_com <- com_train_diffs[order(com_train_diffs[,2]),]
    ipd_nn1 <- sorted_com[1,3]
    ipd_nn2 <- sorted_com[2,3]
    all_ipd <- c(ipd_nn1, ipd_nn2)
    m_count <- 0
    um_count <- 0
    for (i in all_ipd) {
      if(i>=15) {m_count=m_count+1}
      else {um_count=um_count+1}
    }
    if(m_count >= um_count) {k2_class <- c(k2_class, 'M')} 
    else {k2_class <- c(k2_class, 'UM')}
  }
  return(k2_class)
}

#Prediction function when k=3
k3_prediction <- function(v_par, k3_class) {
  k3_class <- c()
  for (i in 2:209793) {
    test <- matrix(v_par[i], nrow= 209791, ncol=1)
    train <- matrix(v_par[-i], nrow= 209791, ncol=1)
    ipd <- matrix(m2[-i])
    diffs <- abs(sweep(test,1,train,"-"))
    com_train_diffs <- cbind(train,diffs,ipd)
    sorted_com <- com_train_diffs[order(com_train_diffs[,2]),]
    ipd_nn1 <- sorted_com[1,3]
    ipd_nn2 <- sorted_com[2,3]
    ipd_nn3 <- sorted_com[3,3]
    all_ipd <- c(ipd_nn1, ipd_nn2, ipd_nn3)
    m_count <- 0
    um_count <- 0
    for (i in all_ipd) {
      if(i>=15) {m_count=m_count+1}
      else {um_count=um_count+1}
    }
    if(m_count>=um_count) {k3_class <- c(k3_class, 'M')} 
    else {k3_class <- c(k3_class, 'UM')}
  }
  return(k3_class)
}

#Prediction function when k=5
k5_prediction <- function(v_par, k5_class) {
  k5_class <- c()
  for (i in 2:209793) {
    test <- matrix(v_par[i], nrow= 209791, ncol=1)
    train <- matrix(v_par[-i], nrow= 209791, ncol=1)
    ipd <- matrix(m2[-i])
    diffs <- abs(sweep(test,1,train,"-"))
    com_train_diffs <- cbind(train,diffs,ipd)
    sorted_com <- com_train_diffs[order(com_train_diffs[,2]),]
    ipd_nn1 <- sorted_com[1,3]
    ipd_nn2 <- sorted_com[2,3]
    ipd_nn3 <- sorted_com[3,3]
    ipd_nn4 <- sorted_com[4,3]
    ipd_nn5 <- sorted_com[5,3]
    all_ipd <- c(ipd_nn1, ipd_nn2, ipd_nn3, ipd_nn4, ipd_nn5)
    m_count <- 0
    um_count <- 0
    for (i in all_ipd) {
      if(i>=15) {m_count=m_count+1}
      else {um_count=um_count+1}
    }
    if(m_count>=um_count) {k5_class <- c(k5_class, 'M')} 
    else {k5_class <- c(k5_class, 'UM')}
  }
  return(k5_class)
}

#build four empty matrixes to hold accuracies for each parameter when k=1,2,3,5
k1_para_rank <- matrix(nrow = 4, ncol = 2)
colnames(k1_para_rank) <- c("parameter","k1_accuracy")
k2_para_rank <- matrix(nrow = 4, ncol = 2)
colnames(k2_para_rank) <- c("parameter","k2_accuracy")
k3_para_rank <- matrix(nrow = 4, ncol = 2)
colnames(k3_para_rank) <- c("parameter","k3_accuracy")
k5_para_rank <- matrix(nrow = 4, ncol = 2)
colnames(k5_para_rank) <- c("parameter","k5_accuracy")

#--------param 1: tpl
#calculate accuracy when k=1
k1_tpl_model <- k1_prediction(tpl, k1_class)
k1_tpl_accuracy <- cal_accuracy(k1_tpl_model)
k1_para_rank[[1,1]] <- c('tpl')
k1_para_rank[[1,2]] <- c(k1_tpl_accuracy)
#calculate accuracy when k=2
k2_tpl_model <- k2_prediction(tpl, k2_class)
k2_tpl_accuracy <- cal_accuracy(k2_tpl_model)
k2_para_rank[[1,1]] <- c('tpl')
k2_para_rank[[1,2]] <- c(k2_tpl_accuracy)
#calculate accuracy when k=3
k3_tpl_model <- k3_prediction(tpl, k3_class)
k3_tpl_accuracy <- cal_accuracy(k3_tpl_model)
k3_para_rank[[1,1]] <- c('tpl')
k3_para_rank[[1,2]] <- c(k3_tpl_accuracy)
#calculate accuracy when k=5
k5_tpl_model <- k5_prediction(tpl, k5_class)
k5_tpl_accuracy <- cal_accuracy(k5_tpl_model)
k5_para_rank[[1,1]] <- c('tpl')
k5_para_rank[[1,2]] <- c(k5_tpl_accuracy)

#------param 2: strand
#calculate accuracy when k=1
k1_strand_model <- k1_prediction(strand, k1_class)
k1_strand_accuracy <- cal_accuracy(k1_strand_model)
k1_para_rank[[2,1]] <- c('strand')
k1_para_rank[[2,2]] <- c(k1_strand_accuracy)
#calculate accuracy when k=2
k2_strand_model <- k2_prediction(strand, k2_class)
k2_strand_accuracy <- cal_accuracy(k2_strand_model)
k2_para_rank[[2,1]] <- c('strand')
k2_para_rank[[2,2]] <- c(k2_strand_accuracy)
#calculate accuracy when k=3
k3_strand_model <- k3_prediction(strand, k3_class)
k3_strand_accuracy <- cal_accuracy(k3_strand_model)
k3_para_rank[[2,1]] <- c('strand')
k3_para_rank[[2,2]] <- c(k3_strand_accuracy)
#calculate accuracy when k=5
k5_strand_model <- k5_prediction(strand, k5_class)
k5_strand_accuracy <- cal_accuracy(k5_strand_model)
k5_para_rank[[2,1]] <- c('strand')
k5_para_rank[[2,2]] <- c(k5_strand_accuracy)

#--------param 3: score
#calculate accuracy when k=1
k1_score_model <- k1_prediction(score, k1_class)
k1_score_accuracy <- cal_accuracy(k1_score_model)
k1_para_rank[[3,1]] <- c('score')
k1_para_rank[[3,2]] <- c(k1_score_accuracy)
#calculate accuracy when k=2
k2_score_model <- k2_prediction(score, k2_class)
k2_score_accuracy <- cal_accuracy(k2_score_model)
k2_para_rank[[3,1]] <- c('score')
k2_para_rank[[3,2]] <- c(k2_score_accuracy)
#calculate accuracy when k=3
k3_score_model <- k3_prediction(score, k3_class)
k3_score_accuracy <- cal_accuracy(k3_score_model)
k3_para_rank[[3,1]] <- c('score')
k3_para_rank[[3,2]] <- c(k3_score_accuracy)
#calculate accuracy when k=5
k5_score_model <- k5_prediction(score, k5_class)
k5_score_accuracy <- cal_accuracy(k5_score_model)
k5_para_rank[[3,1]] <- c('score')
k5_para_rank[[3,2]] <- c(k5_score_accuracy)


#  param 4: ipd
#calculate accuracy when k=1
k1_ipd_model <- k1_prediction(ipd, k1_class)
k1_ipd_accuracy <- cal_accuracy(k1_ipd_model)
k1_para_rank[[3,1]] <- c('ipd')
k1_para_rank[[3,2]] <- c(k1_ipd_accuracy)
#calculate accuracy when k=2
k2_ipd_model <- k2_prediction(ipd, k2_class)
k2_ipd_accuracy <- cal_accuracy(k2_ipd_model)
k2_para_rank[[3,1]] <- c('ipd')
k2_para_rank[[3,2]] <- c(k2_ipd_accuracy)
#calculate accuracy when k=3
k3_ipd_model <- k3_prediction(ipd, k3_class)
k3_ipd_accuracy <- cal_accuracy(k3_ipd_model)
k3_para_rank[[3,1]] <- c('ipd')
k3_para_rank[[3,2]] <- c(k3_ipd_accuracy)
#calculate accuracy when k=5
k5_ipd_model <- k5_prediction(ipd, k5_class)
k5_ipd_accuracy <- cal_accuracy(k5_ipd_model)
k5_para_rank[[3,1]] <- c('ipd')
k5_para_rank[[3,2]] <- c(k5_ipd_accuracy)

### Sort the accuracies
sorted_k1_para_rank <- k1_para_rank[order(k1_para_rank[,2], decreasing=TRUE),] 
sorted_k2_para_rank <- k2_para_rank[order(k2_para_rank[,2], decreasing=TRUE),] 
sorted_k3_para_rank <- k3_para_rank[order(k3_para_rank[,2], decreasing=TRUE),] 
sorted_k5_para_rank <- k5_para_rank[order(k5_para_rank[,2], decreasing=TRUE),] 