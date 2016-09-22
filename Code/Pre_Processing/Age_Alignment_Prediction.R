##Creating the list for Player Alignment.  
## There seem to be players who have two different records for the same year with the same team and park. 
## I can't quite say which this is, but there should only be one.  I need to add some pre-processing to handle that.  
#(yearID, playerID, teamID and park are the same for all players, then consolidate records.)


rm(list=ls())
library(MASS)
library(Matrix)
library(xlsx)
Data = read.csv("~/Dropbox/Baseball/Lahman/DLM_data.csv", header=T,stringsAsFactor=F)
Jensen_118 = read.xlsx("~/Dropbox/Baseball/Lahman/Jensen_118.xlsx", sheetIndex=1)
Jensen_118 = as.character(Jensen_118[,1])

#This eliminates the second team that a player plays for and the players who have more than one entry per year for a single team
Data = Data[!duplicated(Data[,c("playerID","yearID")]) & !duplicated(Data[,c("playerID","yearID","teamID")]),]
Data = Data[!is.na(Data$Age),]
AB_thresh = 40
Year_thresh = 2005

Parks = unique(Data$park)
nParks = length(Parks)
Positions = c("C","1B","2B","3B","SS","LF","CF","RF","DH")

Age.Range = range(Data$Age)
Age.length = Age.Range[2]-Age.Range[1]+1
Age.Alignment = list()
mat = matrix()

for(i in 1:Age.length){
  age = Age.Range[1]-1 + i
  players = Data$playerID[Data$Age==age & Data$AB>AB_thresh & Data$yearID<=Year_thresh ]
  if(length(players) == 0){
    Age.Alignment[[i]] = NULL
    next
  }
  
  Age.Player = list()
  
  for (j in 1:length(players)){
    Player.Prototype = list(Year = NA, Response = list(AB = mat, HR =mat), Predictors = list(F_Park = mat, F_Pos = mat) )
    
    F_Park = rep(0,nParks) 
    F_Park[which(Parks==Data$park[Data$Age==age & Data$playerID==players[j] ] ) ]=1
    F_Pos = rep(0,9)
    F_Pos[which(Positions==Data$POS[Data$Age==age & Data$playerID==players[j] ] ) ]=1
    
    Player.Prototype$Year = Data$yearID[Data$Age==age & Data$playerID==players[j] ]
    Player.Prototype$Response$AB = Data$AB[Data$Age==age & Data$playerID==players[j] ]
    Player.Prototype$Response$HR = Data$HR[Data$Age==age & Data$playerID==players[j] ]
    Player.Prototype$Predictors$F_Park = F_Park
    Player.Prototype$Predictors$F_Pos = F_Pos
    #Player.Prototype$Predictors$Salary = Data$salary.adj[Data$Age == age & Data$playerID==players[j] ]
    
    Age.Player[[j]] = list()
    Age.Player[[j]] = Player.Prototype
  }
  names(Age.Player)=players
  Age.Alignment[[i]] = Age.Player
}

save(file = "~/DGLM/Data/AgeAlignment2005.RData", Age.Alignment)
