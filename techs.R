library(tidyquant)
library(quantmod)
library(tidyverse)
library(zoo)
library(DBI)
library(odbc)
source("functions.r")

# Connection String -------------------------------------------------------

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-EGAJP7S\\SQLEXPRESS",
                 Database = "SineNome",
                 Port = 1433)

# Top holdings table

top_holdings <- dbGetQuery(con, "SELECT * FROM SineNome.")


# List of stock tickers -- to be appended by AM

ticks <- c("SLB")

# Create a dataframe from the list of tickers, stock_call and signals

stock_data <- as.data.frame(do.call(rbind,(lapply(ticks, stock_call))))%>%
            group_by(Ticker)%>%
            arrange(Ticker, Date)%>%
            mutate(Change = Close -lag(Close, default = first(NA)), Gain = if_else(Change > 0, Change, 0),
                Loss = if_else(Change <= 0, abs(Change), 0), 
                Avg_Gain = rollapply(Gain, 14, mean, align='right',fill=NA),
                Avg_Loss = rollapply(Loss, 14, mean, align='right',fill=NA),
                RS = (Avg_Gain / Avg_Loss), RS_14 = if_else(Avg_Loss == 100, 100, 100 - (100 / (1 + RS))),
                Signal = if_else(((RS_14 < 20 & lag(RS_14) > 20 & lag(RS_14, k = 2) > 25 & 
                                       Volume > 2 * rollapply(Volume, 4, mean, aligh = 'right', fill=NA) &
                                       Close / lag(Close) < 1 & Close / lag(Close) < lag(Close) / lag(Close, k = 2))  
                                  
                                    | (RS_14 < 30 & lag(RS_14) > 30 & lag(RS_14, k = 2) > 35 & 
                                        Volume > 2 * rollapply(Volume, 4, mean, aligh = 'right', fill=NA) &
                                        Close / lag(Close) < 1 & Close / lag(Close) < lag(Close) / lag(Close, k = 2))
                                  
                                    | (RS_14 < 50 & lag(RS_14) > 50 & lag(RS_14, k = 2) > 55 & 
                                        Volume > 2 * rollapply(Volume, 4, mean, aligh = 'right', fill=NA) &
                                        Close / lag(Close) < 1 & Close / lag(Close) < lag(Close) / lag(Close, k = 2))), 
                                    
                                    "BUY",
                                 
                        if_else(((RS_14 > 80 & lag(RS_14) < 80 & lag(RS_14, k = 2) < 75 &
                                        Volume > 2 * rollapply(Volume, 4, mean, aligh = 'right', fill=NA) &
                                        Close / lag(Close) > 1 & Close / lag(Close) > lag(Close) / lag(Close, k = 2))
                                 
                                    | (RS_14 > 70 & lag(RS_14) < 70 & lag(RS_14, k = 2) < 65 &
                                        Volume > 2 * rollapply(Volume, 4, mean, aligh = 'right', fill=NA) &
                                        Close / lag(Close) > 1 & Close / lag(Close) > lag(Close) / lag(Close, k = 2))
                        
                                    | (RS_14 > 50 & lag(RS_14) < 50 & lag(RS_14, k = 2) < 45 &
                                        Volume > 2 * rollapply(Volume, 4, mean, aligh = 'right', fill=NA) &
                                        Close / lag(Close) > 1 & Close / lag(Close) > lag(Close) / lag(Close, k = 2))),
                                
                                    "SELL", ""))
                )
