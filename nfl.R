library(dplyr)
createNameTeamTable <- function() {
  abreviation <- c("SD","SL","LA","BLT","LAR","HST","ARI","LAC","BAL","ARZ","MIN","DAL","BUF","NYG","PIT","CHI","CIN","TB","GB","NYJ","DET","SEA","KC","PHI","NE","MIA","CLV","HO","NO","WAS","SF","DEN","ATL","OAK","IND","ARZ","CAR","JAX","TEN")
  
  fullNameTeam <- c("San Diego Chargers","Los Angeles Rams","Los Angeles Rams","Baltimore Ravens","Los Angeles Rams","Houston Texans","Arizona Cardinals","Los Angeles Chargers","Baltimore Ravens","Arizona Cardinals","Minnesota Vikings","Dallas Cowboys","Buffalo Bills","New York Giants","Pittsburgh Steelers","Chicago Bears","Cincinnati Bengals","Tampa Bay Buccaneers","Green Bay Packers","New York Jets","Detroit Lions","Seattle Seahawks","Kansas City Chiefs","Philadelphia Eagles","New England Patriots","Miami Dolphins","Cleveland Browns","Tennessee Titans","New Orleans Saints","Washington Redskins","San Francisco 49ers","Denver Broncos","Atlanta Falcons","Oakland Raiders","Indianapolis Colts","Arizona Cardinals","Carolina Panthers","Jacksonville Jaguars","Tennessee Titans")
  
  teamName <- (data.frame(matrix(ncol = length(abreviation), nrow = 0)))
  teamName <- rbind(teamName, fullNameTeam);
  colnames(teamName) <- abreviation
}

draft <- read.csv(file = "draft.csv", sep= ",")
gameParticipation <- read.csv(file = "gameParticipation.csv", sep= ",")
games <- read.csv(file = "games.csv", sep= ",")
players <- read.csv(file = "players.csv", sep= ",")
plays <- read.csv(file = "plays.csv", sep= ",")
hallOfFame <- read.csv(file = "hallOfFame.csv", sep= ";")
superBowl <- read.csv(file = "Super_Bowl.csv", sep= ",")

################################ QBs ####################################
#separate in two dataframes, only with qb's
qb <- draft$position == 'QB'
qb <- draft[qb,]

qbUnique <- (unique(qb["draftTeam"]))
qbUnique

################################ HALL OF FAME ####################################
#separate in two dataframes, only with qb's
hallOfFame$Player <- as.character(hallOfFame$Player)

qbFame <- hallOfFame$Pos == 'QB'
qbFame <- hallOfFame[qbFame,]

for (i in 1:nrow(qbFame)) {
  aux <- strsplit(as.character(qbFame$Player[i]), "\\\\")[[1]]
  qbFame$Player[i] <- aux[1]
}

colnames(qbFame)[2] <- "nameFull" 

intersect(qbFame$nameFull, qb$nameFull)
intersect(qbFame, qb)

total <- merge(qbFame, qb, by="nameFull")

total <- unique(total["nameFull"])
length(total)

############################# QBs - Super Bowl #################################
#Create the dataframe to make a relactioship between draft qb and superbolw qbs
superBowl <- read.csv(file = "Super_Bowl.csv", sep= ",")

#get the super bolw year
for (i in 1:nrow(superBowl)) {
  aux <- strsplit(as.character(superBowl$Date[i]), "-")[[1]]
  superBowl$Year[i] <- as.integer(aux[3])
}

#put the column year to second columns in the dataframe
superBowl <- superBowl[, c(1, 24, 2:23)]

#a filter to get the super bolws between 77 <> 18 
superBowlYear <- (superBowl$Year >= 77 | superBowl$Year <= 18)
superBowl <- superBowl[superBowlYear,]

#change the column name to make the merge in 2 dataframe
intersect(qb$nameFull, superBowl$QB..Winner)
colnames(superBowl)[5] <- "nameFull" 

#merge the 2 dtaframes
total <- merge(superBowl, qb, by="nameFull")
total <- total[, c(1, 26:28, 30, 2:4, 7)]

total$draftTeam <- as.character(total$draftTeam)

#change the team abreviation name to full name
createNameTeamTable()
for (i in 1:nrow(total)) {
  total$draftTeam[i] <- as.character(teamName[total$draftTeam[i]][[1]])
}

#get the qbs that win the super bowl from the team that selected he from draft
indice <- which(total$draftTeam == total$Winner)

totalDrfatedBySameTeam <- total[indice,]
totalDrfatedByDifferentTeam <- total[-indice,]

#get the qbs that win the super bowl with 1 round pick
indice <- which(total$round == 1)

totalDrfatedOnFirstRound <- total[indice,]
totalNotDrfatedOnFirstRound <- total[-indice,]


########################################## Teams Name #############################
#SD = San Diego Chargers
#SL = Los Angeles Rams
#LA = Los Angeles Rams
#BLT = Baltimore Ravens
#LAR = Los Angeles Rams
#HST = Houston Texans
#ARI = Arizona Cardinals
#LAC = Los Angeles Chargers
#BAL = Baltimore Ravens
#ARZ = Arizona Cardinals
#MIN = Minnesota Vikings
#DAL = Dallas Cowboys
#BUF = Buffalo Bills
#NYG = New York Giants
#PIT = Pittsburgh Steelers
#CHI = Chicago Bears
#CIN = Cincinnati Bengals
#TB = Tampa Bay Buccaneers
#GB = Green Bay Packers
#NYJ = New York Jets
#DET = Detroit Lions
#SEA = Seattle Seahawks
#KC = Kansas City Chiefs
#PHI = Philadelphia Eagles
#NE = New England Patriots
#MIA = Miami Dolphins
#CLV = Cleveland Browns
#HO = Tennessee Titans
#NO = New Orleans Saints
#WAS = Washington Redskins
#SF = San Francisco 49ers
#DEN = Denver Broncos
#ATL = Atlanta Falcons
#OAK = Oakland Raiders
#IND = Indianapolis Colts
#ARZ = Arizona Cardinals
#CAR = Carolina Panthers
#JAX = Jacksonville Jaguars
#TEN = Tennessee Titans

abreviation <- c("SD","SL","LA","BLT","LAR","HST","ARI","LAC","BAL","ARZ","MIN","DAL","BUF","NYG","PIT","CHI","CIN","TB","GB","NYJ","DET","SEA","KC","PHI","NE","MIA","CLV","HO","NO","WAS","SF","DEN","ATL","OAK","IND","ARZ","CAR","JAX","TEN")

fullNameTeam <- c("San Diego Chargers","Los Angeles Rams","Los Angeles Rams","Baltimore Ravens","Los Angeles Rams","Houston Texans","Arizona Cardinals","Los Angeles Chargers","Baltimore Ravens","Arizona Cardinals","Minnesota Vikings","Dallas Cowboys","Buffalo Bills","New York Giants","Pittsburgh Steelers","Chicago Bears","Cincinnati Bengals","Tampa Bay Buccaneers","Green Bay Packers","New York Jets","Detroit Lions","Seattle Seahawks","Kansas City Chiefs","Philadelphia Eagles","New England Patriots","Miami Dolphins","Cleveland Browns","Tennessee Titans","New Orleans Saints","Washington Redskins","San Francisco 49ers","Denver Broncos","Atlanta Falcons","Oakland Raiders","Indianapolis Colts","Arizona Cardinals","Carolina Panthers","Jacksonville Jaguars","Tennessee Titans")

teamName <- (data.frame(matrix(ncol = length(abreviation), nrow = 0)))
teamName <- rbind(teamName, fullNameTeam);
colnames(teamName) <- abreviation
