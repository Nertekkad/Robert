library(phyloseq)
library(ggplot2)
library(readxl)
#Se simula una tabla de OTUs
#Para ello se genera una matriz
otumat1 = matrix(sample(1:100, 100, replace = TRUE), nrow = 30, ncol = 30)
otumat1
#Se asignan nombres a las filas y columnas
#Las filas corresponden a los OTUs y las columnas a las muestras
rownames(otumat1) <- paste0("OTU", 1:nrow(otumat1))
colnames(otumat1) <- paste0("Sample", 1:ncol(otumat1))
otumat1
class(otumat1)
#Construimos una matriz como base para la tabla taxonomica
#Se asignan los niveles taxonomicos a las columnas y los OTUs a las filas
taxmat1 = matrix(sample(letters, 70, replace = TRUE), nrow = nrow(otumat1), ncol = 7)
rownames(taxmat1) <- rownames(otumat1)
colnames(taxmat1) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
taxmat1
class(otumat1)
#Ambas matrices se transforman a un objeto phyloseq
OTU1 = otu_table(otumat1, taxa_are_rows = TRUE)
TAX1 = tax_table(taxmat1)
OTU1
TAX1
#Luego se combinan en un unico objeto phyloseq
physeqA = phyloseq(OTU1, TAX1)
physeqA
#Generamos un grafico de barras con las abundancias absolutas de cada familia en las muestras
plot_bar(physeqA, fill = "Family")
#Creamos datos muestrales aleatorios y los agregamos a los datos combinados
sampledata1 = sample_data(data.frame(
  Location = sample(LETTERS[1:4], size=nsamples(physeqA), replace=TRUE),
  Depth = sample(50:1000, size=nsamples(physeqA), replace=TRUE),
  row.names=sample_names(physeqA),
  stringsAsFactors=FALSE
))
sampledata1
#Utilizamos el paquete "ape" para generar un arbol aleatorio
library("ape")
random_tree1 = rtree(ntaxa(physeqA), rooted=TRUE, tip.label=taxa_names(physeqA))
plot(random_tree1)
#Ahora se fuciona el objeto physeq con los datos muestrales aleatorios y el arbol aleatorio
physeqA1 = merge_phyloseq(physeqA, sampledata1, random_tree1)
physeqA1
#Se reconstruyen los datos en otro objeto phyloseq y se compara si son identicos 
physeqA2 = phyloseq(OTU1, TAX1, sampledata1, random_tree1)
physeqA2
identical(physeqA1, physeqA2)
#Se construye un arbol a partir de los datos generados
plot_tree(physeqA1, color="Location", label.tips="taxa_names", ladderize="left", plot.margin=0.5)
plot_tree(physeqA1, color="Depth", shape="Location", label.tips="taxa_names", ladderize="right", plot.margin=0.3)
#Se generan los heatmaps
#OTUs
plot_heatmap(physeqA1)
#Phylum (abundancias)
plot_heatmap(physeqA1, taxa.label="Phylum")
plot_richness(physeqA1, x="Depth", color="Location")
#Graficos de barra de las abundancias.
plot_bar(physeqA1, fill="Family")
plot_bar(physeqA1, fill = "Family") + 
  geom_bar(aes(color=Family, fill=Family), stat="identity", position="stack")
#Variables
sample_variables(physeqA1)

physeqData <- merge_samples(physeqA1, "Depth")
plot_bar(physeqData, fill = "Family") + 
  geom_bar(aes(color=Family, fill=Family), stat="identity", position="stack")

#Generacion  de la red y transformacion en objeto igraph
plot_net(physeqA1, distance = "(A+B-2*J)/(A+B)", type = "taxa", 
         maxdist = 0.4, color="Family", point_label="Genus")
library(igraph)
ig1<-make_network(physeqA1, max.dist = 0.4)
plot(ig1)
hist(degree.distribution(ig1))

#Repetimos el procedimiento para generar una segunda red
otumat2 = matrix(sample(1:100, 100, replace = TRUE), nrow = 30, ncol = 30)
otumat2
rownames(otumat2) <- paste0("OTU", 1:nrow(otumat2))
colnames(otumat2) <- paste0("Sample", 1:ncol(otumat2))
otumat2
class(otumat2)
taxmat2 = matrix(sample(letters, 70, replace = TRUE), nrow = nrow(otumat2), ncol = 7)
rownames(taxmat2) <- rownames(otumat2)
colnames(taxmat2) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
taxmat2
class(otumat2)
OTU2 = otu_table(otumat2, taxa_are_rows = TRUE)
TAX2 = tax_table(taxmat2)
OTU2
TAX2
physeqB = phyloseq(OTU2, TAX2)
physeqB
sampledata2 = sample_data(data.frame(
  Location = sample(LETTERS[1:5], size=nsamples(physeqB), replace=TRUE),
  Depth = sample(50:1000, size=nsamples(physeqB), replace=TRUE),
  row.names=sample_names(physeqB),
  stringsAsFactors=FALSE
))
sampledata2
library("ape")
random_tree2 = rtree(ntaxa(physeqB), rooted=TRUE, tip.label=taxa_names(physeqB))
plot(random_tree2)
physeqB1 = merge_phyloseq(physeqB, sampledata2, random_tree2)
physeqB1
plot_net(physeqB1, distance = "(A+B-2*J)/(A+B)", type = "taxa", 
         maxdist = 0.3, color="Family", point_label="Genus")
#Generamos el objeto igraph
ig2<-make_network(physeqB1)
plot(ig2)
hist(degree.distribution(ig2))

#Cargamos el paquete multinet e integramos las redes igraph como capas
#como una red bipartita.
library(multinet)
net<-ml_empty()
l<-layout_circular_ml(net)
add_igraph_layer_ml(net, ig1, "Layer1")
add_igraph_layer_ml(net, ig2, "Layer2")
#Verificamos los elementos de la red
num_layers_ml(net)
layers_ml(net)
num_actors_ml(net)
actors_ml(net)
#Generamos el grafico de la red con sus respectivos clusters
ml_clust<-glouvain_ml(net)
l<-layout_multiforce_ml(net, w_inter = 0, gravity = 1)
plot(net, vertex.labels = "", com = ml_clust,grid = c(1,3), layout = l,
     legend.x = "bottomright", legend.inset = c(.1, .1))
#En la mayoria de iteraciones, posee una transitividad muy alta, cercana
#a la unidad.
transitivity(as.list(net)[[1]])
#Correlacion entre capas
library(corrplot)
#Hay cambios significativos entre el degree y las conexiones entre
#los nodos las dos capas. Ya que se emplearon los mismos nombres para
#ambos objetos igraph, los actores son identicos en ambas capas.
comp<-layer_comparison_ml(net, method = "jaccard.edges")
comp
comp2<-as.matrix(comp)
corrplot(comp2)
comp<-layer_comparison_ml(net, method = "pearson.degree")
comp
comp2<-as.matrix(comp)
corrplot(comp2)