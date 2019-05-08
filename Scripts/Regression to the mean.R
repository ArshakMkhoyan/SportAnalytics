library(SportsAnalytics270)
data("nba_misc")
head(nba_misc)
nb=nba_misc
nb$WR=nb$W/82
voutcome=var(nb$WR)
#Variance of luck
vluck=0.5*0.5/82
vskills=voutcome-vluck

data("nba_east")
data("nba_west")
nba_st=rbind(nba_east,nba_west)
nba_st$PW=nba_st$PS^14.105/(nba_st$PS^14.105+nba_st$PA^14.105)
nba_st$G=nba_st$W+nba_st$L
nba_st$PW_G=round(nba_st$PW*nba_st$G)
nba_st$DF=nba_st$PW_G-nba_st$W

nba_st%>% filter(Team=='Sacramento Kings')%>%
  ggplot(aes(x=Season,y=DF))+geom_point()+geom_line()

means=nba_st%>%group_by(Team)%>%summarise(mean=mean(DF))
head(means)
ggplot(means,aes(x=mean))+geom_histogram()


#COmpetitive balance

library(SportsAnalytics270)
data('seriea_st')
seriea_s=seriea_st%>%
  group_by(Season)%>%
  summarise(SD=sd(Pt))
seriea_s
ggplot(seriea_s, aes(Season,SD))+geom_bar(stat = 'identity')
#NFL
data("nfl_st")
nfl_s=nfl_st%>%group_by(Season)%>%
  summarise(SD=sd(Wpct))
ggplot(nfl_s, aes(Season,SD))+geom_bar(stat = 'identity')

#Noll-Scully number
id_s=0.5/sqrt(16)
sd_wpct=sd(nfl_st$Wpct)
sd_wpct/id_s

nfl_s=nfl_st%>%
  group_by(Season)%>%
  summarise(NS=sd(Wpct)/id_s)

id_s_nba=0.5/sqrt(82)
nba_s=nba_st%>%
  group_by(Season)%>%
  summarise(NS=sd(Pct)/id_s_nba)
ggplot(nba_s, aes(Season,NS))+geom_bar(stat = 'identity')


##CONCENTRATION INDEX
library(SportsAnalytics)
library(SportsAnalytics270)
library(dplyr)
library(ggplot2)
data("seriea_st")
top5= seriea_st%>%
  group_by(Season)%>%
  filter(Rank<6)%>%
  summarize(TopP=sum(Pt))
all=seriea_st%>%
  group_by(Season)%>%
  summarize(Pt=sum(Pt))
C5=data.frame(Season=top5$Season,C5=top5$TopP/all$Pt)
ggplot(C5, aes(Season,C5))+geom_bar(stat = 'identity')
ggplot(C5, aes(Season,1-C5))+geom_bar(stat = 'identity')

unique(f_data_sm$LEAGUE)
Bundes=f_data_sm[f_data_sm$LEAGUE=='Bundesliga 1',]
unique(Bundes$SEASON)
a=data.frame()
for (i in c(1994:2018)){
  fin=final_table(Bundes,country =  'Germany',season = i)
  fin$season=i
  a=rbind(a,fin)
}
top5new= a%>%
  group_by(season)%>%
  filter(POSITION<6)%>%
  summarize(TopP=sum(POINTS))
allnew=a%>%
  group_by(season)%>%
  summarize(Pt=sum(POINTS))
C5new=data.frame(Season=top5new$season,C5=top5new$TopP/allnew$Pt)
ggplot(C5new, aes(Season,1-C5))+geom_bar(stat = 'identity')

#Concentration Index
champ_c=seriea_st%>%
  filter(Rank==1)%>%
  group_by(Team)%>%
  summarize(champ=n())%>%
  arrange(desc(champ))

champ_bundes=a%>%
  filter(POSITION==1)%>%
  group_by(TEAM)%>%
  summarize(champ=n())%>%
  arrange(desc(champ))
100*sum(champ_bundes$champ[c(1:4)])/sum(champ_bundes$champ)

#Herfindahl-Hirschman Index, HHI
HHI=seriea_st%>%
  group_by(Season)%>%
  mutate(Perc=Pt/sum(Pt))%>%
  summarise(HHI=sum(Perc^2))
ggplot(HHI, aes(Season,HHI))+geom_bar(stat = 'identity')+coord_cartesian(ylim = c(0.04,0.065))

dHHI=seriea_st%>%
  group_by(Season)%>%
  mutate(Perc=Pt/sum(Pt))%>%
  summarise(HHI=sum(Perc^2)-1/n())
ggplot(dHHI, aes(Season,HHI))+geom_bar(stat = 'identity')

#HHI defined by DEpken
library(SportsAnalytics270)
mlb_HHI=mlb_standings%>%
  group_by(Season)%>%
  summarise(HHI=sum(W^2)*(4/(30^2*162^2)))
ggplot(mlb_HHI, aes(Season,HHI))+geom_bar(stat = 'identity')+geom_hline(yintercept = 1/30, col='red',size=1.5) +coord_cartesian(ylim = c(0.03,0.035))

#MLB Salary
str(mlb_salary)
mlb_s=mlb_salary%>%
  group_by(Season)%>%
  summarise(Tot.salary=sum(Current))
cor(mlb_s$Tot.salary, mlb_HHI$HHI)

mlb_s=mlb_salary%>%
  group_by(Season)%>%
  summarise(Tot.salary=sum(Current), SD=sd(Current))
cor(mlb_s$SD, mlb_HHI$HHI)

#Short run competiveness
f_2009=f_data_sm%>%
  filter(SEASON>2008, COUNTRY %in% c('Italy', 'Germany', 'Spain', 'England','Netherlands','Portugal'), !is.na(H))
library('odds.converter')
f_2009[,c('H','D','A')]=odds.fv (f_2009[,c('H','D','A')], input = 'dec', output = 'prob')


