# Plus/Minus
library(SportsAnalytics270)
library(dplyr)
library(ggplot2)
library(SportsAnalytics)
colnames(nba12)
oks_g=grepl('OKC', nba12$GameID, fixed = T)
okc=nba12[oks_g,]
okc$line_h=paste(okc$HomePlayer1Name,okc$HomePlayer2Name,okc$HomePlayer3Name,okc$HomePlayer4Name,okc$HomePlayer5Name, sep = ',')
okc$line_a=paste(okc$AwayPlayer1Name,okc$AwayPlayer2Name,okc$AwayPlayer3Name,okc$AwayPlayer4Name,okc$AwayPlayer5Name, sep = ',')
#Home team plus/minus
okc$Home_PM= okc$PointsScoredHome-okc$PointsScoredAway
#Away p/m
okc$Away_PM=-okc$Home_PM
#is Durant playing
okc$D_H=grepl('Durant', okc$line_h,fixed=T)
okc$D_A=grepl('Durant', okc$line_a,fixed=T)

#P/M for Durant
sum(okc$Home_PM[okc$D_H])+sum(okc$Away_PM[okc$D_A])
durant_pm= okc%>%
  mutate(PM=ifelse(D_H==T, Home_PM, ifelse(D_A==T, Away_PM,0)))%>%
  group_by(GameID)%>%
  summarise(PM=sum(PM))
ggplot(durant_pm, aes(x=GameID,y=PM))+geom_bar(stat='identity')

#Roland's rating
okc=okc%>%
  mutate(OKC_PM=ifelse(AwayT=='OKC', Away_PM,ifelse(HomeT=='OKC',Home_PM,0)),
         D_PM=ifelse(D_H==T|D_A==T,T,F))
durant_pm=okc%>%
  group_by(GameID,D_PM)%>%
  summarise(PM=sum(OKC_PM))

durant_pm=okc%>%
  group_by(GameID,D_PM)%>%
  summarise(PM=sum(OKC_PM))%>%
  group_by(GameID)%>%
  mutate(NET=PM-lag(PM,1))%>%
  filter(NET != 'NA')%>%
  select(GameID, NET)
ggplot(durant_pm, aes(x=GameID,y=NET))+geom_bar(stat='identity')

#Adjusted P/M
load('nba5.rda')
nba5=data.frame(PM=nba12$PointsScoredHome-nba12$PointsScoredAway,nba5)

model=lm(PM~.,data=nba5)
coeff=sort(coefficients(model),decreasing = T)
coeff[1:10]

name1=names(coeff)
name1=gsub('X',"",name1)
name1=as.data.frame(name1)
nba12p$PlayerID=as.character(nba12p$PlayerID)
name2=left_join(name1,nba12p[,1:2],by=c('name1'='PlayerID'))
names(coeff)=name2$PlayerTrueName
#Leave only players that has played more than 250 minutes (By Rosenbaum)
summary(nba12p$SimpleMin)
more250=nba12p$PlayerID[nba12p$SimpleMin>250]
length(more250)
colnames(nba5)=gsub('X','',colnames(nba5))
nba6=nba5[,colnames(nba5)%in%more250]
nba6=data.frame(PM=nba5$PM,nba6)
model2=lm(PM~.,data = nba6)
coeff2=sort(coefficients(model2),decreasing = T)

name1=names(coeff2)
name1=gsub('X',"",name1)
name1=as.data.frame(name1)
name2=left_join(name1,nba12p[,1:2],by=c('name1'='PlayerID'))
names(coeff2)=name2$PlayerTrueName

#modification with possessions
nba12$PM_Home=nba12$PointsScoredHome/nba12$PossessionsHome
nba12$PM_Away=nba12$PointsScoredAway/nba12$PossessionsAway
nba12$Margin=nba12$PM_Home-nba12$PM_Away

nba6$PM=nba12$Margin
nba6$Secs=nba12$ElapsedSecs/60
nba7=nba6[is.finite(nba6$PM),]
dim(nba7)

model3=lm(PM~.,data=nba7)
coeff3=sort(coefficients(model3),decreasing = T)
name1=names(coeff3)
name1=gsub('X',"",name1)
name1=as.data.frame(name1)
name2=left_join(name1,nba12p[,1:2],by=c('name1'='PlayerID'))
names(coeff3)=name2$PlayerTrueName
coeff3[1:10]

coeff3=coeff3-mean(coeff3)
ggplot()+aes(coeff3)+geom_histogram()

#Weighted

model4=lm(PM~.,data = nba7, weights = Secs)
coeff4=sort(coefficients(model4), decreasing = T)
name1=names(coeff4)
name1=gsub('X',"",name1)
name1=as.data.frame(name1)
name2=left_join(name1,nba12p[,1:2],by=c('name1'='PlayerID'))
names(coeff4)=name2$PlayerTrueName
coeff[1:10]
