rm(list=ls())
library(MASS)
library(Matrix)
library(xlsx)
Data = read.csv("~/sDGLM/Lahman/Data_modern.csv", header=T,stringsAsFactor=F)
Jensen_118 = read.xlsx("~/sDGLM/Lahman/Jensen_118.xlsx", sheetIndex=1)
Jensen_118 = as.character(Jensen_118[,1])

#This eliminates the second team that a player plays for and the players who have more than one entry per year for a single team
Data = Data[!duplicated(Data[,c("playerID","yearID")]) & !duplicated(Data[,c("playerID","yearID")]),]
Data = Data[!is.na(Data$Age),]
AB_thresh = 40
Year_thresh = 2015  #if prediction, change to 2005

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
    Player.Prototype = list(Age = NA, Year = NA, Response = list(AB = mat, HR =mat) )
    
    Player.Prototype$Year = Data$yearID[Data$Age==age & Data$playerID==players[j] ]
    Player.Prototype$Age = Data$Age[Data$Age==age & Data$playerID==players[j] ]
    Player.Prototype$Response$AB = Data$AB[Data$Age==age & Data$playerID==players[j] ]
    Player.Prototype$Response$HR = Data$HR[Data$Age==age & Data$playerID==players[j] ]
        
    Age.Player[[j]] = list()
    Age.Player[[j]] = Player.Prototype
  }
  names(Age.Player)=players
  Age.Alignment[[i]] = Age.Player
}

save(file = "~/sDGLM/Data/AgeAlignment_modern.RData", Age.Alignment)

