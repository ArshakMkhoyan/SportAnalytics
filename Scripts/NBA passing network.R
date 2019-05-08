library(SportsAnalytics270)
library(dplyr)
library(network)
head(roster)
gsw=getTeamRoster(2018, 1610612744)
head(gsw)
x=gsw$PLAYER_ID
#passing=c()
#for (i in x) {
 # try({
  #  p=getPassing(2018,i,'Regular')
   # passing=rbind(passing,p)
#  })
#}
load("~/Downloads/Network analysis-20181024/passing.rda")
head(passing)
passing=filter(passing, PASS_TYPE=='made')
passing=passing[passing$PASS_TEAMMATE_PLAYER_ID %in% x,]
head(passing)

library(igraph)
i_pass=graph_from_edgelist(as.matrix(passing[,c('PLAYER_NAME','PASS_TEAMMATE_PLAYER_NAME')]), directed = T)
plot(i_pass)
passing=passing[passing$PLAYER_NAME!=passing$PASS_TEAMMATE_PLAYER_NAME,]
i_pass=graph_from_edgelist(as.matrix(passing[,c('PLAYER_NAME','PASS_TEAMMATE_PLAYER_NAME')]), directed = T)
plot(i_pass)
library(intergraph)
n_pass=asNetwork(i_pass)
plot(n_pass)

p=n_pass %v% 'vertex.names'
gsw=gsw[order(match(gsw$PLAYER,p)),]
network::set.vertex.attribute(n_pass, 'position', gsw$POSITION)
n_pass %v% 'position'

ngames= unique(passing[,c('PLAYER_NAME','G')])
ngames=ngames[order(match(ngames$PLAYER_NAME,p)),]
network::set.vertex.attribute(n_pass,'ngames',ngames$G)
network::set.edge.attribute(n_pass,'passes',passing$PASS)
network::set.edge.attribute(n_pass,'FGP',passing$FG_PCT)
summary(n_pass%e%'passes')
n_pass1=get.inducedSubgraph(n_pass, eid=which(n_pass%e% 'passes'>30))
plot(n_pass1, displaylabels=T, mode='circle')
library(circlize)
as_m=as.matrix(n_pass1,matrix.type = 'adjacency',attrname='passes')
chordDiagram(as_m)

x=n_pass1 %v% 'ngames'
z=2*(x-min(x))/(max(x)-min(x))
plot(n_pass1, displaylabels=T, mode='circle', vertex.cex=z, vertex.col='position',
     label= n_pass1 %v% 'position')

lineup= c('Kevin Durant','Zaza Pachulia', 'Draymond Green', 'Stephen Curry', 'Klay Thompson')
n_pass2=get.inducedSubgraph(n_pass1, v=which(n_pass1 %v% 'vertex.names' %in% lineup))
plot(n_pass2, displaylabels=T, usecurve=T, edge.curve=0.025)
coords=plot(n_pass2, displaylabels=T, suppress.axes=F)
coords[1,]=c(-2,-3)
coords[2,]=c(-2,-4.2)
coords[3,]=c(-3.5,-4.5)
coords[4,]=c(-3.6,-3)
coords[5,]=c(-2.7,-3.2)
plot(n_pass2,coord=coords, displaylabels=T, usecurve=T, edge.curve=0.025)
x=n_pass2 %e% 'passes'
z=10*(x-min(x))/(max(x)-min(x))
plot(n_pass2,coord=coords, displaylabels=T, usecurve=T, edge.curve=0.015, edge.lwd=z,
     edge.label= n_pass2%e% 'FGP')



