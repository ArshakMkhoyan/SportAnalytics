library('glmnet')
library('Matrix')
#Rasch model
load("~/Downloads/nba11_12.rda")
player=unlist(nba11_12[,1:10])
num.p=length(unique(player))
prefix=rep(c('off','def'),each=5*nrow(nba11_12))
tag=paste(prefix, player)

tag.factor=as.factor(tag)
i=rep(1:nrow(nba11_12),10)
j=as.numeric(tag.factor)

library('Matrix')
X=sparseMatrix(i,j)

i.new=c(which(nba11_12$Home==1),i)
j.new=c(rep(1,sum(nba11_12$Home)),j+1)
X=sparseMatrix(i.new,j.new)
y=nba11_12$Points

library('glmnet')
model=cv.glmnet(X,y,alpha=0,standardize=F)
coef=coef(model, s='lambda.min')[,1]
names(coef)=c('Intercept','HFA',sort(unique(tag)))

alpha=coef[1]
theta=coef[2]
delta=coef[2+1:num.p]
beta=coef[2+num.p+1:num.p]
#OfFensive
head(sort(beta,decreasing = T),n=10)
#Defensive
head(sort(delta),n=10)


def.rating=100*(alpha+theta/2+delta)
off.rating=100*(alpha+theta/2+beta)
head(sort(def.rating),n=10)
head(sort(off.rating, decreasing = T),n=10)

pp100=100*(crossprod(X,y)/colSums(X))[,1]
names(pp100)=c('Home',sort(unique(tag)))
def.pp100=pp100[1+1:num.p]
off.pp100=pp100[1+num.p+1:num.p]
head(sort(off.pp100, decreasing = T),n=10)
library(ggplot2)
ggplot()+geom_point(aes(x=off.rating,y=def.rating))
ggplot()+geom_point(aes(x=off.rating,y=off.pp100))+ggtitle('Offensive rating vs points scored')
