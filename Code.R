library(dplyr)
library(tidyr)
library(quantmod)
library(PerformanceAnalytics)
library(readxl)

SP500 <- read_excel("SP500_Symbols.xlsx")

#The following function is defined to add months to a date type object

addMonth <- function(date, n = 1){
  if (n == 0){return(date)}
  if (n %% 1 != 0){stop("Input Error: argument 'n' must be an integer.")}
  if (class(date) == "character"){date = as.Date(date)}
  
  y = as.numeric(substr(as.character(date),1,4))
  m = as.numeric(substr(as.character(date),6,7))
  d = as.numeric(substr(as.character(date),9,10))
  
  i = 0
  if (n > 0){
    while (i < n){
      m = m + 1
      if (m == 13){
        m = 1
        y = y + 1
      }
      i = i + 1
    }
  }
  else if (n < 0){
    while (i > n){
      m = m - 1
      if (m == 0){
        m = 12
        y = y - 1
      }
      i = i - 1
    }
  }
  
  if (d > 28 & m == 2){
    if ((y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0){d = 29}
    else{d = 28}
  }
  else if (d == 31){if (m %in% c(1, 3, 5, 7, 8, 10, 12) == FALSE){d = 30}}
  
  y = as.character(y)
  
  if (m < 10){m = paste('0', as.character(m), sep = '')}
  else{m = as.character(m)}
  
  if (d < 10){d = paste('0', as.character(d), sep = '')}
  else{d = as.character(d)}
  
  return(as.Date(paste(y,'-',m,'-',d, sep = '')))
}

# A list of Yahoo Finance symbols is extracted from xlsx files

symbol_list <- SP500$Symbol

# Use adjusted close prices to calculate returns
prices <- Ad(getSymbols(symbol_list[1], auto.assign = F, 
                        from = "2017-03-11", to = "2019-03-11"))
colnames(prices) <- symbol_list[1]

# Filter out data with NA in selected period
for (symbol in symbol_list[-1]){
  tryCatch({
    new_price <- Ad(getSymbols(symbol, auto.assign = F, 
                               from = "2017-03-11", to = "2019-03-11"))
    colnames(new_price) <- symbol
    if (sum(is.na(as.vector(new_price))) > 0){
      next
    }
    prices <- merge(prices, new_price)},
    warning = {
      
    },
    finally = {
      next
    }
  )
}

returns <- Return.calculate(prices)[-1,]

# The following function is defined to form winner and loser portfolios
# of month t
winner_loser <- function(J, K, t){
  # Calculate average returns of the last J months
  # Notice that date indeces of the returns are all one-day later
  # than the actual date. Thus (t-1) is used
  avg_return <- last(returns[index(returns) < t - 1], 
                     paste(J, "months")) %>%
    lapply(FUN = function(x) mean(x, na.rm = T))
  
  # Find the deciles
  deciles <- quantile(unlist(avg_return), c(0.1,0.9), na.rm = T)
  
  # Generate loser portfolio. Notice that if portfolio is hold
  # a week after the previous returns are calculated, the following
  # (t-1) should be changed to (t+6). The situation is the same for
  # the winner portfolio
  losers <- as.data.frame(returns) %>%
    select(names(avg_return[unlist(avg_return[!is.nan(unlist(avg_return))]) 
                            > deciles[2]])) %>%
    as.xts()
  
  losers <- first(losers[as.Date(index(losers)) >= t - 1], 
                  paste(K+1, "months"))
  losers <- Return.portfolio(na.approx(losers))
  losers <- first(losers, paste(K, "months"))
  names(losers) <- paste("losers:", t)
  
  # Generate winner portfolio
  winners <- as.data.frame(returns) %>%
    select(names(avg_return[unlist(avg_return[!is.nan(unlist(avg_return))]) 
                            < deciles[1]])) %>%
    as.xts()
  
  winners <- first(winners[as.Date(index(winners)) >= t - 1], 
                  paste(K+1, "months"))
  # The NA cells are approximated using na.approx. However, as all
  # data with NA observations are filtered out and t+K+1 month returns
  # are calculated first then t+K month returns are extracted,
  # the only possible NA cells are in periods that are not required to 
  # be considered when calculating returns (probably the last cells 
  # in each column).
  winners <- Return.portfolio(na.approx(winners))
  winners <- first(winners, paste(K, "months"))
  names(winners) <- paste("winners:", t)
  return(list(losers, winners))
}


# Initialise parameters.
# base_month and terminal_month donates the months (actually the beginning
# of the months) when we start and end the practice of the investment strategy
# respectively

J <- 3
K <- 3
base_month <- as.Date("2017-06-01")
terminal_month <- as.Date("2019-01-01")
t <- base_month

# The base component of winner portfolios and loser portfolios,
# which are the winner portfolio and the loser one of the base month
winner_portfolios <- winner_loser(J, K, base_month)[[2]]
loser_portfolios <- winner_loser(J, K, base_month)[[1]]
winner_returns <- first(winner_portfolios[as.Date(index(winner_portfolios)) 
                                          >= base_month-1], "1 month")
loser_returns <- first(loser_portfolios[as.Date(index(loser_portfolios))
                                         >= base_month-1], "1 month")
# As initially we only have one portfolio in both the winner and loser set,
# initial weights of both is (1,)
winner_weights <- c(1)
loser_weights <- c(1)

# The implementation of the strategy
while (t < addMonth(terminal_month, n = -1)) {
  t <- addMonth(t, n = 1)
  if(addMonth(t, n = -K+1) > base_month){
    # End the position of the earliest portfolio monthly after base_month + K
    # The weights are also adjusted for the newly added porfolio, which weights
    # 1/K
    winner_portfolios <- merge(winner_portfolios[,-1], winner_loser(J, K, t)[[2]],
                                fill = 0)
    winner_weights <- c(winner_weights[-1]/(1-winner_weights[1])*(K-1)/K,
                             1/K)
    loser_portfolios <- merge(loser_portfolios[,-1], winner_loser(J, K, t)[[1]],
                               fill = 0)
    loser_weights <- c(loser_weights[-1]/(1-loser_weights[1])*(K-1)/K,
                            1/K)
  }else{
    # Simply add the new portfolio before base_month + K
    # The weights are adjusted with new portfolio weighted 1/K
    winner_portfolios <- merge(winner_portfolios, winner_loser(J, K, t)[[2]],
                                fill = 0)
    winner_weights <- c(winner_weights*(K-1)/K, 1/K)
    loser_portfolios <- merge(loser_portfolios, winner_loser(J, K, t)[[1]],
                                fill = 0)
    loser_weights <- c(loser_weights*(K-1)/K, 1/K)
  }
  # Buy-and-hold portfolios are calculated below
  # Monthly rebalanced portfolios can be calculated by setting
  # rebalance.on to "monthly" in the Return.portfolio
  temps <- Return.portfolio(
    first(winner_portfolios[as.Date(index(winner_portfolios)) >= t-1],
          "31 days"),
    weights = winner_weights, verbose = T)
  winner_returns <- rbind(winner_returns, first(temps$returns, "1 month"))
  winner_weights <- as.vector(last(first(temps$EOP.Weight, "1 month")))
  temps <- Return.portfolio(
    first(loser_portfolios[as.Date(index(loser_portfolios)) >= t-1], 
          "31 days"),
    weights = loser_weights, verbose = T)
  loser_returns <- rbind(loser_returns, first(temps$returns, "1 month"))
  loser_weights <- as.vector(last(first(temps$EOP.Weight, "1 month")))
}

# Zero-cost portfolio returns derived from winner and loser returns
zero_cost_returns <- winner_returns - loser_returns

# Plot time serieses of each portfolio series
lines2 <- function(X, Y, type, xlab, ylab, col, pch, lty, lwd, cex) {
    lines(x=X, y=Y, col=col)
    abline(h=0, col = "blue", lwd=1)}

plot.zoo(cbind(winner_returns, loser_returns, zero_cost_returns),
         col = 1:3, main = paste("J=",J,"K=",K,base_month,"to",
                                 terminal_month,"Result"),
         panel = lines2, ylab = c("buy", "sell", "zero-cost"),
         xlab = paste("average return: buy", round(mean(winner_returns),7),
                      "sell", round(mean(loser_returns),6), "zero-cost",
                      round(mean(zero_cost_returns),6)))

# Export results into csv file
results <- cbind(winner_returns, loser_returns, zero_cost_returns)
names(results) <- c("Buy", "Sell", "Buy-Sell")
write.csv(as.data.frame(results),
          file = 
            paste("J=",J,"K=",K,base_month,"to",terminal_month,"Result.csv"))
