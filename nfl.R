draft <- read.csv(file = "draft.csv", sep= ",")
gameParticipation <- read.csv(file = "gameParticipation.csv", sep= ",")
games <- read.csv(file = "games.csv", sep= ",")
players <- read.csv(file = "players.csv", sep= ",")
plays <- read.csv(file = "plays.csv", sep= ",")
hallOfFame <- read.csv(file = "hallOfFame.csv", sep= ";")
hallOfFame$Player <- as.character(hallOfFame$Player)


for (i in 1:nrow(hallOfFame)) {
  x <- strsplit(as.character(hallOfFame$Player[i]), "\\\\")[[1]]
  hallOfFame$Player[i] <- x[1]
}


#separate in two dataframes, only with qb's
qb <- draft$position == 'QB'
qb <- draft[qb,]

#separate in two dataframes, only with qb's
qbFame <- hallOfFame$Pos == 'QB'
qbFame <- hallOfFame[qbFame,]

for (i in 1:nrow(qbFame)) {
  x <- strsplit(as.character(qbFame$Player[i]), "\\\\")[[1]]
  qbFame$Player[i] <- x[1]
}

colnames(qbFame)[2] <- "nameFull" 

intersect(qbFame$nameFull, qb$nameFull)
intersect(qbFame, qb)

total <- merge(qbFame, qb, by="nameFull")

hist(qb$round)
