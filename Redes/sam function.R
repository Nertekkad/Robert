library(igraph)

#Generación de las redes----
g1<-erdos.renyi.game(4, 0.1)
g2<-erdos.renyi.game(2, 0.5)
g3<-erdos.renyi.game(5, 0.1)
g4<-erdos.renyi.game(4, 0.5)
g5<-erdos.renyi.game(3, 0.5)
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

#Generación de columnas de la matriz de supradyacencia (derecha)----
a<-c()
for(i in 1:length(matlist)){
  a[i]<-i-1
}; a<-rev(a); a

matseed<-as.numeric(matseed)
matlistA<-list()
for(i in 1:length(matlist)){
  matlistA[i]<-cbind(matlist[[i]], c(1:((matseed^2)*a[i]))*0)
}; matlistA

matlistA<-matlistA[-length(matlistA)]
matlistA<-c(matlistA, matlist[length(matlist)]); matlistA

#Generación de columnas de la matriz de supradyacencia (izquierda)----
a<-c()
for(i in 1:length(matlist)){
  a[i]<-i-1
}; a

matlistB<-list()
for(i in 1:length(matlistA)){
  matlistB[i]<-cbind(c(1:((matseed^2)*a[i]))*0, matlistA[[i]])
}; matlistB

matlistB<-matlistB[-1]; matlistB

#Se unen las filas en una única matriz de supradyacencia----
mat<-matlistA[[1]]; dim(mat)
for(i in 1:length(matlistB)){
  mat<-rbind(mat, matlistB[[i]])
}; dim(mat)








colnames(f)<-c(colnames(matlistB[[1]]), colnames(matlistB[[2]]))
a<-colnames(matlistB[[1]]); class(a)
b<-colnames(matlistB[[2]]); class(b)
colnames(f)<-
c<-LETTERS[1:70]; class(c)
f<-rbind(matlistB[[1]], matlistB[[2]])
colnames(f)





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


library(muxViz)
lay <- layoutMultiplex(muxlist, layout="fr", ggplot.format=F, box=T)

# Show the multiplex network
layer.colors <- rainbow(length(muxlist)); layer.colors
plot_multiplex3D(muxlist, layer.layout=lay, layer.colors=layer.colors,
                 layer.shift.x=0.5, layer.space=2,
                 layer.labels="auto", layer.labels.cex=1.5,
                 node.size.values="auto", node.size.scale=0.8,
                 show.aggregate=T)
as_adjacency_matrix(muxlist[[1]])
