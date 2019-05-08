load("~/Downloads/n_pass.rda")
library(igraph)
library(sna)
library(intergraph)
library(network)
library(dplyr)
i_pass=asIgraph(n_pass)
i_pass=set.edge.attribute(i_pass, 'weight', value = E(i_pass)$passes)
i_pass=set.vertex.attribute(i_pass, 'name', value = V(i_pass)$vertex.names)

edge_density(i_pass)
mean_distance(i_pass, directed = T)

pos_num=as.numeric(as.factor(V(i_pass)$position))
actual=assortativity(i_pass, pos_num)
results=vector(length=1000)
for(i in 1:1000){
  results[i]=assortativity(i_pass, sample(pos_num))
}
ggplot()+geom_histogram(aes(results))+geom_vline(xintercept = actual, color='red')

assortativity.degree(i_pass)
reciprocity(i_pass) #98% of passing is bidirectional

recieved=strength(i_pass, mode='in')
recieved
gave=strength(i_pass, mode='out')
df_p=data_frame(gave, recieved, players=names(recieved))
ggplot(df_p, aes(recieved, gave, label=players))+geom_point()+ geom_label()
degree(i_pass)

pass_close=igraph::closeness(i_pass, weights=1/E(i_pass)$weight, normalized = T, mode='all')
sort(pass_close, decreasing = T)
pass_bet=igraph::betweenness(i_pass, weights=1/E(i_pass)$weight, normalized = T)
sort(pass_bet, decreasing = T)

p_rank=page_rank(i_pass)$vector
sort(p_rank, decreasing = T)

plot(n_pass, vertex.cex=10*pass_bet, displaylabels=T)  

x=(pass_close-mean(pass_close))/sd(pass_close)  
plot(n_pass, vertex.cex=x, displaylabels=T)  
  

