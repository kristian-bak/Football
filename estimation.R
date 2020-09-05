
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

#saveRDS(object = m, file = "C:/Users/Kristian/Programmering/R/Football/model.Rda")

#model <- readRDS(file = "C:/Users/Kristian/Programmering/R/Football/model.Rda")

flags$lambda <- predict(m, type = "response")

#saveRDS(object = flags, file = "C:/Users/Kristian/Programmering/R/Football/flags.Rda")

#flags <- readRDS(file = "C:/Users/Kristian/Programmering/R/Football/flags.Rda")

n_goals <- 12
p1 <- dpois(x = 0:n_goals, lambda = flags$lambda[1])
p2 <- dpois(x = 0:n_goals, lambda = flags$lambda[2])
p_mat <- outer(p1, p2, "*")
colnames(p_mat) <- paste0("away_goal", 0:n_goals)
rownames(p_mat) <- paste0("home_goal", 0:n_goals)

sum(p_mat)

f_home_win <- function(mat) {
  sum(lower.tri(mat) * mat)
}

f_away_win <- function(mat) {
  sum(upper.tri(mat) * mat)
}

f_draw <- function(mat) {
  sum(diag(mat))
}

f_home_win(p_mat)
f_draw(p_mat)
f_away_win(p_mat)

homeID <- seq(from = 1, to = n_obs - 1, by = 2)
awayID <- seq(from = 2, to = n_obs, by = 2)

data$LambdaH <- flags$lambda[homeID]
data$LambdaA <- flags$lambda[awayID]

head(data)

f_prob <- function(data, n_goals = 12) {
  
  n <- nrow(data)
  
  for (i in 1:n) {
    
    p1 <- dpois(x = 0:n_goals, lambda = data$LambdaH[i])
    p2 <- dpois(x = 0:n_goals, lambda = data$LambdaA[i])
    p_mat <- outer(p1, p2, "*")
    colnames(p_mat) <- paste0("away_goal", 0:n_goals)
    rownames(p_mat) <- paste0("home_goal", 0:n_goals)
    
    data$pH[i] <- f_home_win(p_mat)
    data$pD[i] <- f_draw(p_mat)
    data$pA[i] <- f_away_win(p_mat)
    
  }
  
  return(data)
  
}

data <- f_prob(data = data, n_goals = 12)

head(data)

f_factor <- function(data) {
  
  data$factorH <- data$B365H * data$pH
  data$factorD <- data$B365D * data$pD
  data$factorA <- data$B365A * data$pA
  
  return(data)
  
}

data <- f_factor(data)

f_bet <- function(data) {
  
  n <- nrow(data)
  
  for (i in 1:n) {
    bet <- names(which.max(data[i, c("factorH", "factorA", "factorD")]))
    str_bet <- gsub("factor", "", bet)
    if (data[i, bet] > 1) {
      data$bet[i] <- str_bet
      if (str_bet == "H") {
        data$odds[i] <- data$B365H[i]
      } else if (str_bet == "A") {
        data$odds[i] <- data$B365A[i]
      } else {
        data$odds[i] <- data$B365D[i]
      }
    } else {
      data$bet[i] <- "No bet"
    }
    
  }
  
  return(data)
  
}

data <- f_bet(data)

head(data)

table(data$bet)

data[data$bet == "No bet", ]

f_outcome <- function(data, bet_amount = 10) {
  data$outcome <- ifelse(data$bet == data$FTR, bet_amount * data$odds, - bet_amount)
  return(data)
}

data <- f_outcome(data)

head(data)

sum(data$outcome)

dd <- substring(data$Date, 1, 2)
mm <- substring(data$Date, 4, 5)
year <- substring(data$Date, 7, 10)
data$date <- as.Date(paste(year, mm, dd, sep = "-"))

head(data)

library(data.table)

dt <- data.table(data)

dt_outcome <- dt[, list("Value" = sum(outcome)), by = date]
dt_plot <- dt_outcome[, list("Date" = date, "Value" = cumsum(Value))]
library(plotly)

plot_ly(data = dt_plot, x = ~Date, y = ~Value, type = "scatter", mode = "lines+markers") %>%
  layout(title = "Poisson performance on PL19-20 training data")

# yield = 56.4 % using bet amount = 10 DKK
100 * dt[, sum(outcome)] / (nrow(dt) * 10)
