rm(list=ls())


setwd("~/sDGLM/Lahman")
Batting = read.csv("Batting.csv", header=T, stringsAsFactors=F)
Master = read.csv("Master.csv", header=T, stringsAsFactors=F)
Fielding = read.csv("Fielding.csv", header=T, stringsAsFactors=F)
Teams = read.csv("Teams.csv", header=T, stringsAsFactors=F)
Salary = read.csv("Salaries.csv",header=T,stringsAsFactors=F)

Teams = Teams[,c("teamID","lgID", "yearID", "park")]
Fielding = Fielding[,c("playerID", "yearID", "POS", "G")]
Fielding = Fielding[order(Fielding$playerID),]
#Fielding = Fielding[-which(Fielding$POS=="P" | Fielding$POS=="OF" | Fielding$POS=="IF"),]



start.Year = 1990
end.Year = 2015
AB.Thresh = 1

Data = Batting[,c("playerID", "yearID","teamID", "AB", "HR")]
bio = Master[,c("playerID", "birthYear","birthMonth")]
#Drop if player has NAs for ABs or if year is <1990
Data = Data[(Data$AB)>=AB.Thresh & !is.na(Data$AB) & Data$yearID>=start.Year & Data$yearID<=end.Year,]
# merge in Age.
Data = merge(Data, bio, by="playerID", all.x=F, all.y=F)
Age = floor( (Data$yearID + 4/12) - (Data$birthYear + Data$birthMonth/12) )
Data = cbind(Data, Age=Age )
Data = subset(Data, select = -c(birthYear,birthMonth) )

#Data = merge(Data, Teams, by=c("yearID", "teamID") )
Data = Data[order(Data$playerID, Data$yearID),]
Players = unique(Data$playerID)
N_players = length(Players)
Data_collapsed = data.frame(playerID = c(), yearID = c(), Age = c(), HR = c(), AB = c())
#collapse over multiple teams in a year. 
for(i in 1:N_players){
  player = Players[i]
  Player = Data[Data$playerID==player,]
  Seasons = unique(Player$yearID)
  Age = unique(Player$Age)
  yearID = unique(Player$yearID)
  nSeasons = length(Seasons)
  tmp = data.frame(playerID = rep(player,nSeasons), yearID = yearID, Age = Age, HR = rep(NA,nSeasons), AB = rep(NA,nSeasons) )
  for(ii in 1:nSeasons){
    tmp[ii,"HR"] = sum(Player$HR[ Player$Age==tmp$Age[ii] ])
    tmp[ii,"AB"] = sum(Player$AB[ Player$Age==tmp$Age[ii] ])
  }
  Data_collapsed = rbind.data.frame(Data_collapsed,tmp) 
}

Data = Data_collapsed
#Data$park[Data$park=="Candlestick Park"]="3Com Park"
#Data$park[Data$park=="The Ballpark at Arlington"]="Ameriquest Field"
#Data$park[Data$park=="Anaheim Stadium" | Data$park =="Angel Stadium" | Data$park == "Edison International Field"]="Angels Stadium of Anaheim"
#Data$park[Data$park=="Bank One Ballpark"]="Chase Field"
#Data$park[Data$park=="Riverfront Stadium"]="Cinergy Field"
#Data$park[Data$park=="Pro Player Stadium" | Data$park=="Dolphin Stadium"]="Joe Robbie Stadium"
#Data$park[Data$park=="Enron Field"] = "Minute Maid Park"
#Data$park[Data$park=="Jack Murphy Stadium"] = "Qualcomm Stadium"
#Data$park[Data$park=="Jacobs Field"] = "Progressive Field"
#Data$park[Data$park=="Royals Stadium"] = "Kauffman Stadium"
#Data$park[Data$park=="Kingdome / Safeco Field"] = "Kingdome"
#Data$park[Data$park=="Oakland Coliseum" | Data$park=="Network Associates Coliseum"] = "McAfee Coliseum"
#Data$park[Data$park=="Skydome"] = "Rogers Centre"
#Data$park[Data$park=="PacBell Park" | Data$park =="SBC Park"]="AT&T Park"
#Data$park[Data$park=="Stade Olympique/Hiram Bithorn Stadium"]="Stade Olympique"
#Data$park[Data$park=="Comiskey Park II"]="U.S. Cellular Field"


Players = unique(Data$playerID)


Data_Online = Data[Data$yearID>=1990,]
#Parks_Online = levels(factor(Data_Online$park))
#Players_Online = unique(Data_Online$playerID)

Data_Train = Data[Data$yearID<=1989,]
#Parks_Train = levels(factor(Data_Train$park))
#Players_Train = unique(Data_Train$playerID)

write.csv(file="Data_modern.csv", Data_Online, row.names=F)
write.csv(file="Data_WW2.csv", Data_Train, row.names=F)



