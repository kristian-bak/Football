`%notin%` <- Negate(`%in%`)

f_date <- function(data) {
  dd <- substring(data$Date, 1, 2)
  mm <- substring(data$Date, 4, 5)
  year <- substring(data$Date, 7, 10)
  return(as.Date(paste(year, mm, dd, sep = "-")))
}

f_load <- function(league = "PL", season) {
  
  if (league != "PL") {
    stop("league must be PL at the moment")
  }
  
  if (season %notin% c(1617, 1718, 1819, 1920)) {
    stop("season must be 1819 or 1920")
  }
  
  file <- paste0("C:/Users/Kristian/Programmering/R/Football/", league, season, ".csv")
  
  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  data$Date <- f_date(data)
  data$Game <- paste0(data$HomeTeam, "-", data$AwayTeam)
  
  data <- data[, c("Date", "HomeTeam", "AwayTeam", "Game",
                   "FTHG", "FTAG", "FTR", 
                   "B365H", "B365D", "B365A")]
  return(data)
  
}

f_map <- function(team) {
  df_map$id[df_map$team == team]
}

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

f_flags <- function(data) {
  
  teams <- unique(data$HomeTeam)
  n_teams <- length(teams)
  n_games <- nrow(data)
  
  flags <- matrix(NA, nrow = 2 * n_games, ncol = 2 * n_teams)
  str_attack <- paste0("attack_", 1:n_teams)
  str_defence <- paste0("defence_", 1:n_teams)
  colnames(flags) <- c(str_attack, str_defence)
  
  df_map <<- data.frame(teams, id = order(teams))
  
  data$HomeTeamID <- sapply(X = data$HomeTeam, FUN = f_map)
  data$AwayTeamID <- sapply(X = data$AwayTeam, FUN = f_map)
  
  n_obs <- nrow(flags)
  
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
  flags$team <- c(rbind(data$HomeTeam, data$AwayTeam))
  flags$gameID <- rep(1:n_games, each = 2)
  
  return(flags)
  
}

f_fit <- function(data, y = "goals") {
  
  flags <- f_flags(data = data)
  
  n_teams <- length(unique(flags$team))
  str_attack <- paste0("attack_", 1:n_teams)
  str_defence <- paste0("defence_", 1:n_teams)
  
  form <- as.formula(paste0(y, " ~ -1 + home_effect + ", 
                            paste0(c(str_attack, str_defence), collapse = " + ")))
  
  m <- glm(formula = form, family = poisson(link = "log"), data = flags)
  
  return(m)
  
}

f_home_win <- function(mat) {
  sum(lower.tri(mat) * mat)
}

f_away_win <- function(mat) {
  sum(upper.tri(mat) * mat)
}

f_draw <- function(mat) {
  sum(diag(mat))
}

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

f_factor <- function(data) {
  
  data$factorH <- data$home_odds * data$pH
  data$factorD <- data$draw_odds * data$pD
  data$factorA <- data$away_odds * data$pA
  
  return(data)
  
}

f_bet <- function(data) {
  
  n <- nrow(data)
  
  for (i in 1:n) {
    bet <- names(which.max(data[i, c("factorH", "factorA", "factorD")]))
    str_bet <- gsub("factor", "", bet)
    if (data[i, bet] > 1) {
      data$bet[i] <- str_bet
      if (str_bet == "H") {
        data$odds[i] <- data$home_odds[i]
      } else if (str_bet == "A") {
        data$odds[i] <- data$away_odds[i]
      } else {
        data$odds[i] <- data$draw_odds[i]
      }
    } else {
      data$bet[i] <- "No bet"
    }
    
  }
  
  return(data)
  
}

f_outcome <- function(data, bet_amount = 10) {
  data$outcome <- ifelse(data$bet == data$FTR, bet_amount * data$odds - bet_amount, - bet_amount)
  return(data)
}


f_rename <- function(team) {
  ifelse(team == "Crystal P", 
         "Crystal Palace", 
         ifelse(team == "Manchester C", 
                "Man City", 
                ifelse(team == "Manchester U", 
                       "Man United", 
                       ifelse(team == "Wolverhampton", 
                              "Wolves", team))))
}

f_load_odds <- function() {
  
  url <- "https://dswebapp.sb.danskespil.dk/allekampe/den-lange?ev_categories_ids=21&ev_class_id=97&ev_type_id=13304&date_filter=FUT"
  
  webpage <- read_html(url)
  
  game <- webpage %>%
    html_nodes('div[class="eventCont"] span') %>% html_text()
  
  game <- game[game != "Premier League - England"]
  
  special_id <- grep("Over/Under", game)
  
  game <- game[-special_id]
  n_games <- length(game)
  
  home_team <- gsub(" v .*", "", game)
  away_team <- gsub(".* v ", "", game)
  
  odds <- webpage %>%
    html_nodes('td span[class="odds-decimal"]') %>% html_text() %>% str_replace_all('\\n', '')
  
  f_remove_special <- function(odds, special_id) {
    id <- 3 * special_id - 2
    odds <- odds[-c(id, id + 1)]
    return(odds)
  }
  
  odds <- f_remove_special(odds, special_id)
  odds <- gsub(",", ".", odds)
  odds <- as.numeric(odds)
  
  f_linReg <- function(x, a, b) {
    a * x + b
  }
  
  f_odds <- function(id, type) {
    
    if (type == 1) {
      # a and b found as coefficents satisfying linReg with x = c(1, 2, 3) and y = c(1, 4, 7)
      odds[f_linReg(x = id, a = 3, b = -2)]
    } else if (type == 2) {
      # a and b found as coefficents satisfying linReg with x = c(1, 2, 3) and y = c(2, 5, 8)
      odds[f_linReg(x = id, a = 3, b = -1)]
    } else if (type == 3) {
      # a and b found as coefficents satisfying linReg with x = c(1, 2, 3) and y = c(3, 6, 9)
      odds[f_linReg(x = id, a = 3, b = 0)]
    }
    
  }
  
  game_id <- 1:n_games
  
  HomeTeam <- f_rename(team = home_team)
  AwayTeam <- f_rename(team = away_team)
  Game <- paste0(HomeTeam, "-", AwayTeam)
  
  df <- data.frame(Game,
                   HomeTeam, 
                   AwayTeam,
                   home_odds = f_odds(game_id, type = 1), 
                   draw_odds = f_odds(game_id, type = 2), 
                   away_odds = f_odds(game_id, type = 3),
                   stringsAsFactors = FALSE)
  
  return(df)
  
}

f_predict_round <- function(fit_data, model, bet_amount = 10) {
  
  flags <- f_flags(data = fit_data)

  flags$lambda <- predict(model, type = "response")
  
  homeID <- seq(from = 1, to = nrow(flags) - 1, by = 2)
  awayID <- seq(from = 2, to = nrow(flags), by = 2)
  df_lambda <- data.frame(HomeTeam = flags$team[homeID], 
                          AwayTeam = flags$team[awayID], 
                          LambdaH = flags$lambda[homeID], 
                          LambdaA = flags$lambda[awayID], 
                          stringsAsFactors = FALSE)
  
  df_lambda$Game <- paste0(df_lambda$HomeTeam, "-", df_lambda$AwayTeam)
  
  new_data <- f_load_odds()
  
  joined_games <- intersect(df_lambda$Game, new_data$Game)
  
  df_lambda_join <- df_lambda[df_lambda$Game %in% joined_games, ]
  new_data_join <- new_data[new_data$Game %in% joined_games, ]
  
  predict_data <- merge(x = new_data_join, y = df_lambda_join, by = "Game")
  predict_data <- predict_data[, c("HomeTeam.x", "AwayTeam.x",
                                   "home_odds", "draw_odds", "away_odds", "LambdaH", "LambdaA")]
  colnames(predict_data) <- c("HomeTeam", "AwayTeam", 
                              "home_odds", "draw_odds", "away_odds", "LambdaH", "LambdaA")

  predict_data <- f_prob(data = predict_data)
  predict_data <- f_factor(data = predict_data)
  predict_data <- f_bet(data = predict_data)
  
  return(predict_data)
}

f_plot <- function(data, bet_amount = 10, num_season = 1920) {
  
  dt <- data.table(data)
  setorderv(dt, "Date")
  dt_outcome <- dt[, list("Value" = sum(outcome)), by = Date]
  dt_plot <- dt_outcome[, list("Date" = Date, "Value" = cumsum(Value))]
  
  yield <- 100 * dt[, sum(outcome)] / (dt[bet != "No bet", .N] * bet_amount)
  names(yield) <- "yield %"
  
  str_title <- paste0("Poisson performance on PL", num_season, " test data")
  p <- plot_ly(data = dt_plot, x = ~Date, y = ~Value, type = "scatter", mode = "lines+markers") %>%
    layout(title = str_title, yaxis = list(title = "Income"))
  
  out <- list("plot" = p, 
              "yield" = yield)
  
  return(out)
  
}
