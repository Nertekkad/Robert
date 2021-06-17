library(igraph)
#Generación de las redes----
g1<-erdos.renyi.game(12, 0.5)
g2<-erdos.renyi.game(20, 0.5)
g3<-erdos.renyi.game(9, 0.5)
g4<-erdos.renyi.game(8, 0.5)
g5<-erdos.renyi.game(13, 0.5)
g6<-erdos.renyi.game(16, 0.5)
g7<-erdos.renyi.game(17, 0.5)
g8<-erdos.renyi.game(23, 0.5)
g9<-erdos.renyi.game(13, 0.5)
g10<-erdos.renyi.game(22, 0.5)

V(g1)$name <- LETTERS[1:vcount(g1)]; plot(g1)
V(g2)$name <- LETTERS[1:vcount(g2)]; plot(g2)
V(g3)$name <- LETTERS[1:vcount(g3)]; plot(g3)
V(g4)$name <- LETTERS[1:vcount(g4)]; plot(g4)
V(g5)$name <- LETTERS[1:vcount(g5)]; plot(g5)
V(g6)$name <- LETTERS[1:vcount(g6)]; plot(g6)
V(g7)$name <- LETTERS[1:vcount(g7)]; plot(g7)
V(g8)$name <- LETTERS[1:vcount(g8)]; plot(g8)
muxlist<-list(g1, g2, g3, g4, g5)

#Transformación de las redes en matrices de adyacencia----
matlist<-list()
for(i in 1:length(muxlist)){
  matlist[i]<-as_adjacency_matrix(muxlist[[i]])
}; matlist

#Identificación de la red más grande----
a<-list()
for(i in 1:length(muxlist)){
  a[i]<-vcount(muxlist[[i]])
}; matseed<-a[which.max(a)]; matseed

#Se reorganiza la lista colocando el mayor al último----
matlist2<-matlist[-(which.max(a))]; matlist2
matlist<-c(matlist2, matlist[[which.max(a)]]); matlist
a<-list()
for(i in 1:length(matlist)){
  a[i]<-dim(matlist[[i]])[1]
}; matseed<-a[which.max(a)]; matseed


#Identificación de las diferencias entre las dimensiones de las matrices----
for(i in 1:length(matlist)){
  dif[i]<-as.numeric(matseed)-as.numeric(dim(matlist[[i]])[1])
}; dif

#Se igualan las dimensiones de todas las matrices----
matlist2<-matlist[-(which.max(a))]; matlist2
m2<-list()
for(i in 1:length(matlist2)){
  m2[i]<-cbind(matlist2[[i]], c(1:(length(matlist2[[i]][,1])*dif[i]))*0)
}; m2
for(i in 1:length(matlist2)){
  m2[i]<-rbind(m2[[i]],
               c(1:((dim(m2[[i]])[2]-dim(m2[[i]])[1])*dim(m2[[i]])[2]))*0)
}; m2

#Reunificar la lista en un objeto único----
matlist<-c(matlist[[which.max(a)]], m2); matlist
length(matlist)

matseed<-as.numeric(matseed)
for(i in 1:length(matlist)){
  matlista[i]<-cbind(matlist[[i]], c(1:((matseed^2)*2))*0)
}; matlista

matseed<-as.numeric(matseed)
cbind(matlist[[2]], c(1:((matseed^2)*2))*0)
length(matlist)

for(i in length(matlist):1){
  i<-i-1
  print(i)
}
  
mat1
for(i in 1:dim(matlist)[1]) +
  for(j in 1:dim(matlist[1])[2]) +
    if(colnames(matlist)[i]==rownames(mat1)[j]){
      mat1[i,j]=1
    }
mat1
colnames(mat3)[1]==rownames(mat3)[1]

else {mat3[1,1]<-0}

matlist[[1]]
for(i in 1:dim(matlist[[1]])[1])
  for(j in 1:dim(matlist[[1]])[2])
    if(matlist[[1]][i,]==matlist[[1]][,j]){
      matlist[[1]][i,j]<-1
    }
matlist[[1]]
matlist[[1]][1,]

length(matlist)
r<-matlist[-length(matlist)]; length(r)
r


