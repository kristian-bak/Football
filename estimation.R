
data <- read.csv("C:/Users/Kristian/Programmering/R/Football/PL1920.csv", 
                 header = TRUE, stringsAsFactors = FALSE)

head(data)

data <- data[, c("Date", "HomeTeam", "AwayTeam", 
                 "FTHG", "FTAG", "FTR", 
                 "B365H", "B365D", "B365A")]

teams <- unique(data$HomeTeam)
n_teams <- length(teams)
n_games <- nrow(data)

df_map <- data.frame(teams, id = order(teams))

f_map <- function(team) {
  df_map$id[df_map$team == team]
}

data$HomeTeamID <- sapply(X = data$HomeTeam, FUN = f_map)
data$AwayTeamID <- sapply(X = data$AwayTeam, FUN = f_map)

flags <- matrix(NA, nrow = 2 * n_games, ncol = 2 * n_teams)
str_attack <- paste0("attack_", 1:n_teams)
str_defence <- paste0("defence_", 1:n_teams)
colnames(flags) <- c(str_attack, str_defence)

n_obs <- nrow(flags)

head(flags)

f_binary <- function(home_team, away_team, n_teams) {
  binary_attack1 <- rep(0, n_teams)
  binary_attack2 <- binary_attack1
  binary_defence1 <- binary_attack1
  binary_defence2 <- binary_attack1
  binary_attack1[home_team] <- 1
  binary_attack2[away_team] <- 1
  binary_defence1[away_team] <- 1
  binary_defence2[home_team] <- 1
  row1 <- c(binary_attack1, binary_defence1)
  row2 <- c(binary_attack2, binary_defence2)
  mat <- matrix(c(row1, row2), ncol = 2 * n_teams, byrow = TRUE)
  return(mat)
}

j1 <- seq(from = 1, to = n_obs - 1, by = 2)
j2 <- seq(from = 2, to = n_obs, by = 2)

for (i in 1:n_games) {
  flags[j1[i]:j2[i], ] <- f_binary(home_team = data$HomeTeamID[i], 
                                   away_team = data$AwayTeamID[i], 
                                   n_teams = n_teams)
}

flags <- data.frame(flags)

flags$home_effect <- rep(1:0, n_games)
flags$goals <- c(rbind(data$FTHG, data$FTAG))

head(flags)
y <- "goals"
form <- as.formula(paste0(y, " ~ -1 + home_effect + ", 
                          paste0(c(str_attack, str_defence), collapse = " + ")))

m <- glm(formula = form, family = poisson(link = "log"), data = flags)

summary(m)
