e_coli<-read.table("01_Raw_Data/coliInterFullVec.txt")
#e_coli<-read.csv("01_Raw_Data/1aorinter_st.txt",sep = "\t")
yeast<-read.table("01_Raw_Data/yeastinter_st.txt")

auto_reg<-function(red){
contador<-0
for(i in 1:nrow(red)){
if(red$V1[i]==red$V2[i]){
  contador<-contador + 1
}
}
return (contador)
}

#Emilio's function

list_to_adj<-function(red){
matriz <- matrix(0, nrow(red), nrow(red))
for (i in 1:nrow(red)) {
  matriz[red$V1[i], red$V2[i]] <- 1
  
}
return(matriz)
}

red_ecoli_m<-ah_ah(e_coli)
red_yeast_m<-ah_ah(yeast)

mi_funcion_escalon<-function(x,umbral){
  return(ifelse(x>umbral,1,0))
}

library(igraph)


red1<-graph_from(e_coli)


#----FFL C1

library(BoolNet)


sink("01_Raw_Data/FFLC1.bn")
cat("targets, factors\n")
cat("x, 1\n")
cat("y, x\n")
cat("z, y & x")
sink()

loadNetwork("01_Raw_Data/FFLC1.bn")->FFLC1
