load('usa.rda')
load('usa_i.rda')
library(network)
library(sna)
library(igraph)
library(intergraph)
library(ggplot2)
plot(usa)
#Statistics
get_diameter(usa_i)
edge_density(usa_i)
mean_distance(usa_i, directed = T)

igraph::list.vertex.attributes(usa_i)
table(V(usa_i)$type)
type_num=as.numeric(as.factor(V(usa_i)$type))
assortativity(usa_i, type_num)
assortativity.degree(usa_i, directed = T)
reciprocity(usa_i)
ego(usa_i,1,'JFK', mode = c('out'))
ego(usa_i,2,'JFK', mode = c('out'))

#number of conections
neighbors(usa_i, 'JFK', mode='in')
d_out=igraph::degree(usa_i, mode='out')
ggplot()+geom_histogram(aes(x=d_out))

#number of flights
f_in=strength(usa_i, mode = 'in')
which.max(f_in)
f_in[41]
ggplot()+geom_histogram(aes(x=f_in))+scale_y_log10()

#closness
close=igraph::closeness(usa_i, weights = 1/E(usa_i)$weight, normalized = T, mode='all')
df_c=data.frame(airports=names(close), clossness=close)
df_c=df_c[order(df_c$clossness, decreasing = T),]
head(df_c)
bet=igraph::betweenness(usa_i,weights = 1/E(usa_i)$weight, normalized = T )
sort(bet, decreasing = T)[1:5]
p_rank=page_rank(usa_i)$vector
sort(p_rank, decreasing = T)
#library(SportsAnalytics270)
#View(transfers)
usa_i1=as.undirected(usa_i, mode='collapse')
com=fastgreedy.community(usa_i1)
sizes(com)
plot(com, usa_i1)
