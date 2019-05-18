# Returns-to-Buying-Winners-and-Selling-Losers

## Sourse: 
Jegadeesh, Narasimhan and Sheridan Titman, 1993, ``Returns to buying winners and selling losers: implications for stock market  efficiency", Journal of Finance 48, 65-91.
## Data Sources:
The data are selected stocks from Yahoo Finance, accessed by getSymbol() of quantmod library. As the processing power of my computer is limited. I only selected S&P 500 stocks data from 2017/03/11 to 2019/03/11. The symbol lists is extracted from https://www.slickcharts.com/sp500. Notably data size can be increased by extending the symbol list or the selected period.
## Outputs:
The output will be a r plot and a csv file of the time serieses of returns of buy, sell and zero-cost portfolio serieses for a combination of (J, K, start month, end month).
## Comments:
This project was done a few months ago when I had not got a comprehensive understanding of quantitative trading. It is clearly that the code is subject to several notable issues as my data analysis, programming skills and investment knowledge were not very sufficient at that time. 

## Modified Date 18/05/2019
