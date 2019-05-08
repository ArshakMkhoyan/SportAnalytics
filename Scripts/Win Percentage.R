library(devtools)
install_github('HABET/CSE270')
library(SportsAnalytics270)
library(SportsAnalytics)
library(dplyr)
head(mlb_standings)
mlb_standings$Wpct=mlb_standings$W/(mlb_standings$W+mlb_standings$L)
mlb_standings$RD=mlb_standings$R-mlb_standings$RA
library(ggplot2)
ggplot(mlb_standings,aes(x=RD,y=Wpct))+geom_point()+geom_smooth(method='lm',se=F)+labs(x='Run differential', y='Winning percentage')

#Modeling
options(scipen = 999)
mod=lm(Wpct~RD, data=mlb_standings)
summary(mod)

mlb_standings$PW_percentage=mlb_standings$R^1.9/(mlb_standings$R^1.9+mlb_standings$RA^1.9)
errors=mlb_standings$Wpct-mlb_standings$PW_percentage
RMSE=sqrt(mean(errors^2))
sqrt(mean(mod$residuals^2))

mlb_standings$PW=mlb_standings$PW_percentage*(mlb_standings$W+mlb_standings$L)
mlb_standings$PW=round(mlb_standings$PW,0)
mlb_standings%>%
  filter(Season==2017)%>%
  select(Team, W, PW)%>%
  head()

#NBA
data("nba_east")
data('nba_west')
nba_st=rbind(nba_east,nba_west)
head(nba_st)
nba_st$PD=nba_st$PS-nba_st$PA
ggplot(nba_st,aes(x=PD,y=Pct))+geom_point()+geom_smooth(method='lm',se=F)+labs(x='Points differential', y='Winning percentage')

options(scipen = 999)
model2=lm(Pct~PD,data = nba_st)
nba_st$Ratio<-nba_st$PS/nba_st$PA
model2=lm(log(W/L)~0+log(Ratio),data=nba_st)
#find K
summary(model2)
#RMSE
nba_st$PW=nba_st$PS^14.105/(nba_st$PS^14.105+nba_st$PA^14.105)
sqrt(mean((nba_st$PW-nba_st$Pct)^2))

nba_st$PW_G=round(nba_st$PW*82)
nba_st%>%filter(Season==2018)%>%
  ggplot(aes(x=PW_G,y=W))+geom_point()+
  geom_abline(intercept = 0,slope=1,col='red')

#NFL
View(nfl_st)
nfl_st_new=nfl_st[nfl_st$W!=0,]
nfl_st_new=nfl_st_new[nfl_st_new$L!=0,]
rat=nfl_st_new$PF/nfl_st_new$PA
model3=lm(log(W/L)~0+log(rat),data=nfl_st_new)
summary(model3)
