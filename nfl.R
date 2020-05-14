library(dplyr)


draft <- read.csv(file = "draft.csv", sep= ",")
gameParticipation <- read.csv(file = "gameParticipation.csv", sep= ",")
games <- read.csv(file = "games.csv", sep= ",")
players <- read.csv(file = "players.csv", sep= ",")
plays <- read.csv(file = "plays.csv", sep= ",")
hallOfFame <- read.csv(file = "hallOfFame.csv", sep= ";")
superBowl <- read.csv(file = "Super_Bowl.csv", sep= ",")

hallOfFame$Player <- as.character(hallOfFame$Player)


for (i in 1:nrow(hallOfFame)) {
  x <- strsplit(as.character(hallOfFame$Player[i]), "\\\\")[[1]]
  hallOfFame$Player[i] <- x[1]
}


#separate in two dataframes, only with qb's
qb <- draft$position == 'QB'
qb <- draft[qb,]
x <- unique(qb$draftTeam)

qbUnique <- unique(qb["nameFull"])

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

total <- unique(total["nameFull"])
length(total)

#Create the dataframe to make a relactioship between draft qb and superbolw qbs
intersect(qb$nameFull, superBowl$QB..Winner)
colnames(superBowl)[4] <- "nameFull" 

y <- unique(superBowl$Winner)

total <- merge(superBowl, qb, by="nameFull")

total <- total[, c(1:6, 29 )]

total$nameFull <- as.character(total$nameFull)

count(total, vars="nameFull")

total

table(unique(total["nameFull"]))
total$Winner

hist(qb$round)
