rm(list=ls())


setwd("~/Dropbox/Baseball/Lahman")
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
end.Year = 2013
AB.Thresh = 1

Data = Batting[,c("playerID", "yearID","teamID", "AB", "HR")]
bio = Master[,c("playerID", "birthYear","height","weight","bats","throws")]
#Drop if player has NAs for ABs or if year is <1990
Data = Data[(Data$AB)>=AB.Thresh & !is.na(Data$AB) & Data$yearID>=start.Year & Data$yearID<=end.Year,]
# merge in Age.
Data = merge(Data, bio, by="playerID", all.x=F, all.y=F)
Data = cbind(Data, Age=(Data$yearID-Data$birthYear) )
Data = Data[,-6]
Data = merge(Data, Teams, by=c("yearID", "teamID") )
#Data = merge(Data, Salary, by=c("playerID","yearID","lgID","teamID"))
#Data = Data[,c("playerID", "yearID", "teamID","lgID", "HR","AB","Age", "park","salary","height","weight","bats")]
Data = Data[,c("playerID", "yearID", "teamID","lgID", "HR","AB","Age", "park","height","weight","bats")]


Data = cbind(Data, POS = rep(NA, dim(Data)[1]))

for(i in 1:dim(Data)[1]){
  year = Data$yearID[i]
  player = Data$playerID[i]
  temp = Fielding[Fielding$playerID==player & Fielding$yearID==year,]
  if( (is.element("OF",temp$POS) | is.element("IF",temp$POS)) & sum(is.element(temp$POS, c("C","1B","2B","3B","SS","LF","CF","RF","DH","P")) )>0 ){
    temp = temp[-which(temp$POS=="OF" | temp$POS=="IF"),]
  }
  Data$POS[i]=temp$POS[temp$G==max(temp$G)][1]
    
}


Data = Data[order(Data$playerID, Data$yearID),]
Rookie.Year = rep(0,dim(Data)[1])
Rookie.AB = Rookie.Year
Career.AB = Rookie.Year
Years.Pro = Rookie.Year
Data = cbind(Data,Rookie.Year,Rookie.AB,Career.AB, Years.Pro)
Players = unique(Data$playerID)
N_players = length(Players)

for(i in 1:N_players){
  player = Players[i]
  Player = Data[Data$playerID==player,]
  nYears = dim(Player)[1]
  Player$Rookie.Year[Player$yearID==Player$yearID[1]]=1
  Player$Rookie.AB[cumsum(c(0,Player$AB[1:nYears-1]))<=130]=1
  Player$Career.AB=sum(Player$AB)
  Player$Years.Pro = cumsum(rep(1,dim(Player)[1]))
  
  Data$Rookie.Year[Data$playerID==player]=Player$Rookie.Year
  Data$Rookie.AB[Data$playerID==player]=Player$Rookie.AB
  Data$Career.AB[Data$playerID==player]=Player$Career.AB
  Data$Years.Pro[Data$playerID==player]=Player$Years.Pro
}

Data = Data[!is.na(Data$POS),]




Data = cbind(Data, E=rep(0,dim(Data)[1]))
Data[Data$HR/Data$AB>=.05,"E"]=1
Data$E = factor(Data$E)

B = as.data.frame(bs(Data$Age, degree=3, intercept=F, Boundary.knots=c(19,48)))
names(B)=c("B1","B2","B3")
Age = Data[,"Age"]
Data = cbind(Data,B)


Data$park[Data$park=="Candlestick Park"]="3Com Park"
Data$park[Data$park=="The Ballpark at Arlington"]="Ameriquest Field"
Data$park[Data$park=="Anaheim Stadium" | Data$park =="Angel Stadium" | Data$park == "Edison International Field"]="Angels Stadium of Anaheim"
Data$park[Data$park=="Bank One Ballpark"]="Chase Field"
Data$park[Data$park=="Riverfront Stadium"]="Cinergy Field"
Data$park[Data$park=="Pro Player Stadium" | Data$park=="Dolphin Stadium"]="Joe Robbie Stadium"
Data$park[Data$park=="Enron Field"] = "Minute Maid Park"
Data$park[Data$park=="Jack Murphy Stadium"] = "Qualcomm Stadium"
Data$park[Data$park=="Jacobs Field"] = "Progressive Field"
Data$park[Data$park=="Royals Stadium"] = "Kauffman Stadium"
Data$park[Data$park=="Kingdome / Safeco Field"] = "Kingdome"
Data$park[Data$park=="Oakland Coliseum" | Data$park=="Network Associates Coliseum"] = "McAfee Coliseum"
Data$park[Data$park=="Skydome"] = "Rogers Centre"
Data$park[Data$park=="PacBell Park" | Data$park =="SBC Park"]="AT&T Park"
Data$park[Data$park=="Stade Olympique/Hiram Bithorn Stadium"]="Stade Olympique"
Data$park[Data$park=="Comiskey Park II"]="U.S. Cellular Field"

Data = Data[-which(Data$POS=="P"),]

Parks = levels(factor(Data$park) )
Data$park = factor(Data$park)
Data$POS = factor(Data$POS, levels=c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH","OF","IF","P"))
Players = unique(Data$playerID)


Data_Online = Data[Data$yearID>=1990,]
Parks_Online = levels(factor(Data_Online$park))
Players_Online = unique(Data_Online$playerID)

Data_Train = Data[Data$yearID<=1989,]
Parks_Train = levels(factor(Data_Train$park))
Players_Train = unique(Data_Train$playerID)

write.csv(file="DLM_data.csv", Data_Online, row.names=F)
write.csv(file="DLM_Prior_data.csv", Data_Train, row.names=F)

write.csv(file="Parks.csv", Parks_Online, row.names=F)
write.csv(file="Players.csv",Players_Online,row.names=F)
write.csv(file="Parks_Prior.csv", Parks_Train, row.names=F)
write.csv(file="Players_Prior.csv",Players_Train,row.names=F)


