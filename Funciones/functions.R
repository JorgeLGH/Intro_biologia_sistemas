# Functions for possible motifs

##------- Autoregulation ------

auto_motifs <- function(nodes, edges, reps, obs_pos, obs_neg){
  auto_pos <- 0
  auto_neg <- 0
  for (i in 1:reps) {
    mat1 <- matrix(0, ncol = nodes, nrow = nodes)
    mat1[sample(nodes*nodes,edges/2)] <- 1
    mat1[sample(which(mat1==0),edges/2)] <- -1
    auto_pos[i] <- sum(diag(mat1)==1)
    auto_neg[i] <- sum(diag(mat1)==-1)
  }
  mean_pos <- mean(auto_pos)
  mean_neg <- mean(auto_neg)
  if(obs_pos>mean_pos){
    k <- obs_pos - mean_pos
    res_pos <- k/sd(auto_pos)
    if(res_pos>=1){
      print(paste("Dado a que el valor observado corresponde a",
                  res_pos,"desviaciones estándar de lo esperado, puede que haya motivos de autoregulación positiva"))
    }else{
      print("No parece haber motivos de autoregulación positiva")
    }
  }else {
    print("No parece haber motivos de autoregulación positiva")
  }
  if(obs_neg>mean_neg){
    k <- obs_neg - mean_neg
    res_neg <- k/sd(auto_neg)
    if(res_neg>=1){
      print(paste("Dado a que el valor observado corresponde a",
                  res_neg,"desviaciones estándar de lo esperado, puede que haya motivos de autoregulación negativa"))
    }else{
      print("No parece haber motivos de autoregulación negativa")
    }
  }else {
    print("No parece haber motivos de autoregulación negativa")
  }
}


##------ Directed -----

directed_motifs <- function(nodes, edges, reps, obs){
  direc <- c()
  for (i in 1:reps) {
    mat1 <- matrix(0, nrow = nodes, ncol = nodes)
    mat1[sample(nodes*nodes,edges)] <- 1
    count <- 0
    for (k in 1:nrow(mat1)) {
      for (j in 1:ncol(mat1)) {
        if(j != k){
          if(mat1[j,k]==1 & mat1[k,j]==1){
            count <- count + 1
          }
        }
      }
    }
    direc[i] <- count/2
  }
  m <- mean(direc)
  if(obs>m){
    k <- obs - m
    res <- k/sd(direc)
    if(res>=1){
      print(paste("Dado a que el valor observado corresponde a",
                  res,"desviaciones estándar de lo esperado, puede que haya motivos de regulación recíproca")) 
    }else{
      print(paste("No parece haber motivos de regulación recíproca"))
    }
  }else{
    print(paste("No parece haber motivos de regulación recíproca"))
  }
}



##------ Adjacency matrix from directed graph format -----

adj_mat <- function(df){
  mat_1 <- matrix(0, nrow(df), nrow(df))
  for (i in 1:nrow(df)) {
    mat_1[df[[1]][i], df[[2]][i]] <- 1
  }
  return(mat_1)
}

##------ Detect autoregulation -----

det_auto_reg <- function(df){
  count <- 0
  for (i in 1:length(diag(df))) {
    if(diag(df)[i] != 0){
      count <- count + 1
    }
  }
  return(count)
}