library(devtools)
system('defaults write org.R-project.R force.LANG en_US.UTF-8')
install_github('HABET/CSE270')
library(SportsAnalytics270)
data("nba2009_2017")
library(ggplot2)
ggplot(nba2009_2017, aes(x=home.PTS))+geom_histogram()
library(dplyr)
nba2009_2017%>%
  filter(SEASON_ID=='2017')%>%
  ggplot(aes(x=home.PTS))+geom_histogram()+xlim(c(50,155))
  
#Test hypothesis of normal distribution
nba2009_2017%>%
  filter(SEASON_ID=='2017')%>%
  group_by(home.TEAM_ABBREVIATION)%>%
  summarise(p_value=shapiro.test(home.PTS)$p.value)%>%
  arrange(p_value)


## POISSON Distribution
data("f_data_sm")
# Lyamda 
f_data_sm%>%
  filter(COUNTRY=='Italy', SEASON=='2018')%>%
  summarise(mean(FTTG))

seria= f_data_sm%>%
  filter(COUNTRY=='Italy', SEASON=='2018')%>%
  group_by(FTHG)%>%
  summarise(count=n())%>%
  as.data.frame()



#Bradley-Terry model 
library(BradleyTerry2)
library(SportsAnalytics270)
str(nfl)
library(dplyr)
nfl2 = nfl%>%
  select(team_home,team_away,result)%>%
  filter(result !='T')%>%
  mutate(ht_w= ifelse(result=='H',1,0),
         at_w=ifelse(result=='A',1,0))
head(nfl2)

nfl3=nfl2%>%
  mutate(team_home=as.factor(team_home),
         team_away=as.factor(team_away))%>%
  group_by(team_home,team_away)%>%
  summarise(ht=sum(ht_w),at=sum(at_w))
head(nfl3)

#modeling
model=BTm(cbind(ht,at),team_home, team_away,data=nfl3, id='team_')
summary(model)
coef=model$coefficients
sort(coef,decreasing = T)

length(unique(nfl3$team_home))
length(coef)
# 1 team is taken as a reference level, its ability (alpha) is 0
BTabilities(model)
abilities=as.data.frame(BTabilities(model))
abilities$team=rownames(abilities)
abilities=abilities[order(abilities$ability,decreasing = T),]

#Ploting
library(ggplot2)
ggplot(data = abilities,aes(x=reorder(team,ability),y=ability))+geom_bar(stat='identity',fill=1:32)+coord_flip()+labs(x='Team',y='Ability to Win', title='Ability to win of NFL teams')

abilities_new=abilities
abilities_new$ability=abilities_new$ability-min(abilities$ability)
head(abilities_new)
ggplot(data = abilities_new,aes(x=reorder(team,ability),y=ability))+geom_bar(stat='identity',fill=1:32)+coord_flip()+labs(x='Team',y='Ability to Win', title='Ability to win of NFL teams')

#Predictions
broncos=data.frame(team_home=rep('Denver Broncos',4),
                   team_away=c('Cleveland Browns','Tennessee Titans','Chicago Bears', 'Detroit Lions'))
broncos$team_home<-factor(broncos$team_home, levels(nfl3$team_away))
broncos$team_away<-factor(broncos$team_away, levels(nfl3$team_away))

broncos_prob=predict(model,newdata = broncos,level = 2,type = 'response')
broncos_df=data.frame(broncos,ht_w=broncos_prob, at_w=1-broncos_prob)

broncos_ab=abilities[rownames(abilities)=='Denver Broncos',1]
browns_ab=abilities[rownames(abilities)=='Cleveland Browns',1]
#probability of broncos to win
exp(broncos_ab)/(exp(broncos_ab)+exp(browns_ab))

#EXTENSION 

nfl3$team_home= data.frame(team=nfl3$team_home,at.home=1)
nfl3$team_away= data.frame(team=nfl3$team_away,at.home=0)
str(nfl3)

model2=BTm(cbind(ht,at), team_home, team_away, formula = ~team+at.home,data=nfl3,id='team')
model$coefficients
db=coefficients(model2)['teamDenver Broncos']
cb=coefficients(model2)['teamCleveland Browns']
h=coefficients(model2)['at.home']
#When broncos plays at home
w_h=exp(db+h)/(exp(db+h)+exp(cb))
#Odds
od_h=w_h/(1-w_h)
#When they play at neutral field
w_n=exp(db)/(exp(db)+exp(cb))
od_n=w_n/(1-w_n)

#Predictions
broncos=data.frame(team_home=rep('Denver Broncos',4),
                   team_away=c('Cleveland Browns','Tennessee Titans','Chicago Bears', 'Detroit Lions'))
broncos$team_home<-data.frame(team=factor(broncos$team_home, levels(nfl3$team_home$team)), at.home=1)
broncos$team_away<-data.frame(team=factor(broncos$team_away, levels(nfl3$team_home$team)),at.home=0)

br2=predict(model2,newdata = broncos,level = 2,type = 'response')
