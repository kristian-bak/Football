
library(data.table)
library(plotly)

source("C:/Users/Kristian/Programmering/R/Football/functions.R")

data1819 <- f_load(season = 1819)

data1920 <- f_load(season = 1920)

model <- f_fit(data1920)

## lav en ny predict funktion, der returner, hvad man skal gøre i den nye runde baseret på 
## danske spil odds. 
predict_data <- f_predict_round(fit_data = data1920, model = model)

predict_data

predict_data[!is.na(predict_data$odds), ]

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

dt_profit <- dt[, list("Counts" = .N, "Sum" = sum(outcome)), by = c("odds_group", "odds_label")]

setorderv(dt_profit, "odds_group")

dt_profit    

plot_ly(data = dt_profit, x = ~odds_label, y = ~Sum, type = "scatter", mode = "lines+markers") %>%
  layout(title = "PL1920 test data profit by odds group", xaxis = list("title" = "Odds group"))

dt[odds > 8]

dt[, acc := ifelse(outcome > 0, 1, 0)]

dt_precision <- dt[, list("Counts" = .N, "Accurate" = sum(acc), "Accuracy" = sum(acc) / .N), 
                   by = c("odds_group", "odds_label")]

setorderv(dt_precision, "odds_group")

dt_precision
