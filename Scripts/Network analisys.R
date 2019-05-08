library(igraph)
library(sna)
library(intergraph)
library(network)

#Adjacency matrix
mat=rbind(c(0,1,1,0,0),c(0,0,1,1,0),c(0,1,0,0,0),c(0,0,0,0,0),c(0,0,1,0,0))
rownames(mat)=c('A','B','C','D','E')
colnames(mat)=c('A','B','C','D','E')

net=network(mat, matrix.type='adjacency')
summary(net)
plot(net,main='Network')
plot(net,main='Network',label=network.vertex.names(net))
plot(net,main='Network',label=network.vertex.names(net), vertex.cex=3, label.cex=1.5, edge.lwd=10)

#Edge list
mat1=cbind(FEOM=c('A','A',"B","B","C",'E'),TO=c("B",'C','C','D','B','C'))
net1=network(mat1, matrix.type='edgelist')
plot(net1,main='Network',label=network.vertex.names(net1), vertex.cex=3, label.cex=1.5, edge.lwd=10)

net_ed=as.matrix(net,matrix.type='edgelist')
net3=igraph::graph_from_edgelist(mat1)
plot(net3)
#sna social network analysis
sna::gplot(net1,displaylabels = T)

net_i=intergraph::asIgraph(net1)
class(net_i)
net_n=intergraph::asNetwork(net_i)
class(net_n)
network::set.vertex.attribute(net1,'gender',c('F','F','M','F','M'))
summary(net1)
network::get.vertex.attribute(net1,'gender')
net1%v%'gender'
plot(net1,main='Network with node attributes',vertex.col='gender', label=network.vertex.names(net1), vertex.cex=3, label.cex=1.5, edge.lwd=10)

network::set.edge.attribute(net1,'edge.size',c(3,0,5,2,6))
net1%e%'edge.size'

network::list.edge.attributes(net1)
plot(net1,main='Network with node attributes',vertex.col='gender',edge.lwd='edge.size' ,label=network.vertex.names(net1), vertex.cex=3, label.cex=1.5)

#How to keep the graph constant:
#1. set.seed
#2. save the coordinates to a variable
my_coord=plot(net1, lebel=network.vertex.names(net1))
plot(net1, lebel=network.vertex.names(net1),coord=my_coord)
#3. predefined layout
plot(net1, mode='circle')

net5=asIgraph(net1)
coords=layout_in_circle(net5)
coords
plot(net5, layout=coords)
plot(net5, layout=layout_with_fr(net5))

coord1=plot(net1,displaylabels=T,mode='circle',suppress.axes=F)
coord1[3,]=0
plot(net1,displaylabels=T,mode='circle',coord=coord1)

#subseting and ploting
net_f=get.inducedSubgraph(net1, v=which(net1%v%'gender'=='F'))
plot(net_f)
net_e=get.inducedSubgraph(net1, eid=which(net1%e%'edge.size'>=3))
plot(net_e)

net4=graph_from_adjacency_matrix(mat)
net4=set_vertex_attr(net4,'gender',value = c('F','F','M','F','M'))
V(net4)$name 
net4=set_edge_attr(net4,'edge.val',value = c(5,0,3,1,0,3))
E(net4)$edge.val

setwd("~/Downloads/Network analysis-20181017")
marvel=read.csv('marvel.csv')
head(marvel)
m_igraph=igraph::graph_from_edgelist(as.matrix(marvel[,1:2]), directed = F)
m_igraph=set_edge_attr(m_igraph,'characters',value = marvel[,3])
plot(m_igraph, layout=layout_in_circle(m_igraph))
m_net=intergraph::asNetwork(m_igraph)
plot(m_net, mode='circle',displaylabels=T, label.pos=6)
movies=read.csv('marvel_movies.csv', stringsAsFactors = F)
identical(movies$Movie.title,m_net %v% 'vertex.names')

set.vertex.attribute(m_net,attrname = 'BoxOffice', movies$World_gross)
m_igraph=set_vertex_attr(m_igraph, name='BoxOffice', value=movies$World_gross)
plot(m_net,mode='circle',displaylabels=T,label.pos=6,edge.label='characters')
plot(m_net,mode='circle',displaylabels=T,label.pos=6,edge.lwd='characters', label.cex=1.1)
plot(m_net,mode='circle',displaylabels=T,label.pos=6,edge.col='characters')
plot(m_net,mode='circle',displaylabels=T,label.pos=6,edge.lwd='characters', vertex.cex=m_net%v%'BoxOffice')
x=m_net%v%'BoxOffice'
z=(x-mean(x))/sd(x)
plot(m_net,mode='circle',displaylabels=T,label.pos=6,edge.lwd='characters', vertex.cex=z)

as_m=as.matrix(m_net,matrix.type = 'adjacency',attrname='characters')
as_m[upper.tri(as_m)]=0
library('circlize')
chordDiagram(as_m)
as_m[as_m==1]=0
chordDiagram(as_m)

m_net1=get.inducedSubgraph(m_net,eid=which(m_net %e% 'characters'>=2))
plot(m_net1,mode='circle',displaylabels=T,label.pos=6,edge.lwd='characters')

m_net2=get.inducedSubgraph(m_net,v=which(m_net%v%'BoxOffice'>=600))
plot(m_net2,mode='circle',displaylabels=T,label.pos=6,edge.lwd='characters')

#characters
load('marvel_roles.rda')
list.edge.attributes(marvel_roles)
set.seed(1)
plot(marvel_roles, main='network')
marvel_roles1=get.inducedSubgraph(marvel_roles,eid = which(marvel_roles%e%'weight'>1))
set.seed(1)
plot(marvel_roles1, main='network',edge.col='weight')
legend('topleft',fill = unique(marvel_roles1%e%'weight'),legend = unique(marvel_roles1%e%'weight'))


marvel_roles2=get.inducedSubgraph(marvel_roles,eid = which(marvel_roles%e%'weight'==3))
set.seed(1)
plot(marvel_roles2, main='network',displaylabels=T, mode='circle')


