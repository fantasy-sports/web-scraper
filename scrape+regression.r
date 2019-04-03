library(htmltab)
library(tidyr)
library(dplyr)
library(stringr)


url <- "https://www.numberfire.com/nhl/daily-fantasy/daily-hockey-projections"
tab <- htmltab(url, which=4)
tab1 <- tab %>% separate(Player, c("Initial","Player", "Position", "Nonsense")) %>% unite("Real", Initial, Player)
tab2 <- tab %>% separate(Player, c("Initial","Player", "Poistion", "Nonsense", "Pos", "Team"))
tab1$Pos <- tab2$Pos
tab1$Team <- tab2$Team
colnames(tab1)[colnames(tab1)=="Stats >> PPG"] <- "PPG"
colnames(tab1)[colnames(tab1)=="Stats >> Ast"] <- "Ast"
colnames(tab1)[colnames(tab1)=="Stats >> Shots"] <- "Shots"
colnames(tab1)[colnames(tab1)=="Stats >> PPA"] <- "PPA"
colnames(tab1)[colnames(tab1)=="Stats >> +/-"] <- "PM"
colnames(tab1)[colnames(tab1)=="Stats >> Blk"] <- "BLK"
colnames(tab1)[colnames(tab1)=="Stats >> Blk"] <- "BLK"
colnames(tab1)[colnames(tab1)=="Stats >> Goals"] <- "Goals"
colnames(tab1)[colnames(tab1)=="Fantasy >> Cost"] <- "Cost"
colnames(tab1)[colnames(tab1)=="Fantasy >> FP"] <- "FP"
colnames(tab1)[colnames(tab1)=="Fantasy >> Value"] <- "Val"
colnames(tab1)[colnames(tab1)=="Stats >> MIN"] <- "Min"

tab1$rCost <- (gsub("\\$", "", tab1$Cost))
tab1$rCost <- gsub( ',', '', tab1$rCost)

tab1$Goals <- as.numeric(as.character(tab1$Goals))
tab1$Ast <-as.numeric(as.character(tab1$Ast))
tab1$FP <- as.numeric(as.character(tab1$FP))
tab1$rCost <- as.numeric(as.character(tab1$rCost))
tab1$PPG <- as.numeric(as.character(tab1$PPG))
tab1$PPA <- as.numeric(as.character(tab1$PPA))
tab1$Val <- as.numeric(as.character(tab1$Val))
tab1$Min<- as.numeric(as.character(tab1$Min))
tab1$BLK<- as.numeric(as.character(tab1$BLK))
tab1$Shots<- as.numeric(as.character(tab1$Shots))
tab1$PM<- as.numeric(as.character(tab1$PM))

regressionOne <- lm(FP~Shots + Goals + Ast + PPG + PPA + PM + BLK + Min, data = regressed)
summary(regressionOne)
