library('PlayerRatings')
library('elo')
library('EloRating')
el=elo.prob(elo.A = 1500,elo.B = 1600)
el/(1-el)
#For A
1500+20*(1-el)
#For B
1600+20*(0-(1-el))
#Through the code
elo.calc(elo.A=1500,elo.B = 1600,wins.A = 1,k=20)
elo.update(elo.A=1500,elo.B = 1600,wins.A = 1,k=20)

##Real data practice
library(SportsAnalytics270)
data("epl_00_18")
str(epl_00_18)

elos=elo.run(score(HG,AG)~Home+Away,data=epl_00_18,k=20)
elos_df=as.data.frame(elos)
head(elos_df)
tail(elos_df)
elo_final=final.elos(elos)
sort(elo_final,decreasing = T)
#Adding home advantage
epl2=elo.run(score(HG,AG)~adjust(Home,200)+Away, data=epl_00_18,k=20)
sort(final.elos(epl2), decreasing = T)  
  
  