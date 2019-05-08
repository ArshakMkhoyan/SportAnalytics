library('UserNetR')
devtools::install_github("DougLuke/UserNetR")
data(Moreno)
library(network)
library(sna)
library(igraph)
library(intergraph)
set.seed(1)
plot(Moreno)
network.size(Moreno)
#dencity
sna::gden(Moreno)
#Calculate the diameter
lgc=sna::component.largest(Moreno, result='graph')
gd=sna::geodist(lgc)
max(gd$gdist)

mat=rbind(c(0,1,1,0,0),c(0,0,1,1,0),c(0,1,0,0,0),c(0,0,0,0,0),c(0,0,1,0,0))
rownames(mat)=c('A','B','C','D','E')
colnames(mat)=c('A','B','C','D','E')
net=network(mat, matrix.type='adjacency')

net_i=graph_from_adjacency_matrix(mat)
d=shortest.paths(net_i, v=V(net_i), to=V(net_i))
sum(d[upper.tri(d)])
16*2/(5*(5-1))
Moreno_i=asIgraph(Moreno)
mean_distance(Moreno_i, directed = F)

#randomness

i_random=erdos.renyi.game(n=gorder(Moreno_i), p.or.m = edge_density(Moreno_i), type = 'gnp')
plot(i_random)
i_list=list()
for(i in 1:1000){
  i_list[[i]]=erdos.renyi.game(n=gorder(Moreno_i), p.or.m = edge_density(Moreno_i), type = 'gnp')
  
}
avg_pl=unlist(lapply(i_list, mean_distance, directed=F))

ggplot()+geom_histogram(aes(x=avg_pl))+geom_vline(xintercept = mean_distance(Moreno_i, directed = F), size=1.2, color='red')
mean(avg_pl< mean_distance(Moreno_i, directed = F))

#Relationship between vertices
plot(net_i)
E(net_i)
head_of(net_i, E(net_i))
tail_of(net_i, E(net_i))
net_i['A',c('B','C')]
incident(net_i, 'A', mode = c('out'))
incident(net_i, 'A', mode = c('in'))

neighbors(net_i, 'A', mode = c('all'))

A=neighbors(net_i, 'A', mode = c('out'))
D=neighbors(net_i, 'D', mode = c('in'))
intersection(A,D)

#Node's prominence/importance

library(sna)
net%v%'vertex.names'
sna::degree(net)
plot(net, displaylabels=T)
sna::degree(net, cmode = 'indegree')
sna::degree(net, cmode = 'outdegree')

sna::closeness(net, gmode = 'graph')
igraph::closeness(net_i, mode = 'all', normalized=T)

library(intergraph)
network::set.edge.attribute(net, 'weight', c(30,10,5,2,6))
igraph::closeness(asIgraph(net), normalized=T)
network::set.edge.attribute(net, 'weight', 1/c(30,10,5,2,6))
igraph::closeness(asIgraph(net), normalized=T)

igraph::betweenness(net_i)
sna::betweenness(net)

eigen_centrality(net_i)$vector

page_rank(net_i)$vector

#Special relations in network

set.seed(1)
plot(Moreno_i, vertex.color= V(Moreno_i)$gender, vertex.label=V(Moreno_i)$gender)
assortativity(Moreno_i, V(Moreno_i)$gender)

