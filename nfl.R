draft <- read.csv(file = "draft.csv", sep= ",")
gameParticipation <- read.csv(file = "gameParticipation.csv", sep= ",")
games <- read.csv(file = "games.csv", sep= ",")
players <- read.csv(file = "players.csv", sep= ",")
plays <- read.csv(file = "plays.csv", sep= ",")
hallOfFame <- read.csv(file = "hallOfFame.csv", sep= ";")
hallOfFame$Player <- as.character(hallOfFame$Player)


for (i in 1:length(hallOfFame)) {
  x <- strsplit(as.character(hallOfFame$Player[i]), "\\\\")[[1]]
  hallOfFame$Player[i] <- x[1]
}


x <- c("teste \ teste2")
typeof(x)
x <- as.character(hallOfFame$Player[2])
x
x<-strsplit(x, "\\\\")[[1]];
x[1]
colnames(plays)

#separate in two dataframes, only with qb's
qb <- draft$position == 'QB'
qb <- draft[qb,]

hist(qb$round)
