
library(data.table)
library(plotly)

source("C:/Users/Kristian/Programmering/R/Football/functions.R")

data1819 <- f_load(season = 1819)

data1920 <- f_load(season = 1920)

flags <- f_flags(data = data1819)

m <- f_fit(flags)

predict_data <- f_predict(flags = flags, model = m, new_season = data1920)

head(predict_data)

plot_res <- f_plot(data = predict_data, bet_amount = 10, num_season = 1920)

plot_res$plot
plot_res$yield
# yield = 8.82 % using bet amount = 10 DKK

dt <- data.table(predict_data)

dt[, list("Sum" = sum(outcome)), by = HomeTeam]
dt[, list("Sum" = sum(outcome)), by = AwayTeam]

dt[, odds_group := cut(odds, quantile(dt$odds, probs = seq(from = 0, to = 1, by = 0.1)), 
                       labels = FALSE, include.lowest = TRUE)]

dt[, odds_label := cut(odds, quantile(dt$odds, probs = seq(from = 0, to = 1, by = 0.1)), 
                       include.lowest = TRUE)]

profit <- dt[, list("Counts" = .N, "Sum" = sum(outcome)), by = c("odds_group", "odds_label")]

setorderv(profit, "odds_group")

profit    

plot_ly(data = profit, x = ~odds_label, y = ~Sum, type = "scatter", mode = "lines+markers") %>%
  layout(title = "PL1920 test data profit by odds group", xaxis = list("title" = "Odds group"))

dt[odds > 8]
