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

ticks <- c("SLB","AMZN","OZRK","DFS","AXP", "MSFT","GOOGL", "AXP", "FB", "FISV", "QCOM")

# Create a dataframe from the list of tickers, stock_call and signals

stock_data <- as.data.frame(do.call(rbind,(lapply(ticks, stock_call))))%>%
            arrange(Ticker, Date)%>%
            group_by(Ticker)%>%
            mutate(Change = Close -lag(Close, default = first(NA)), Gain = if_else(Change > 0, Change, 0),
                Loss = if_else(Change <= 0, abs(Change), 0), 
                Avg_Gain = rollapply(Gain, 14, mean, align='right',fill=NA),
                Avg_Loss = rollapply(Loss, 14, mean, align='right',fill=NA),
                RS = (Avg_Gain / Avg_Loss), RS_14 = if_else(Avg_Loss == 100, 100, 100 - (100 / (1 + RS))),
                Signal = if_else(((RS_14 < 20 & lag(RS_14) > 20 & lag(RS_14, k = 2) > 25 & 
                                       Volume > 2 * lag(rollmean(x = Volume, 4, align = "right", fill = NA)) &
                                       Close / lag(Close) < 1 & Close / lag(Close) < lag(Close) / lag(Close, k = 2))  
                                  
                                    | (RS_14 < 30 & lag(RS_14) > 30 & lag(RS_14, k = 2) > 35 & 
                                        Volume > 2 * lag(rollmean(x = Volume, 4, align = "right", fill = NA)) &
                                        Close / lag(Close) < 1 & (Close / lag(Close)) < (lag(Close) / lag(Close, k = 2)))
                                  
                                    | (RS_14 < 50 & lag(RS_14) > 50 & lag(RS_14, k = 2) > 55 & 
                                        Volume > 2 * lag(rollmean(x = Volume, 4, align = "right", fill = NA)) &
                                        Close / lag(Close) < 1 & Close / lag(Close) < lag(Close) / lag(Close, k = 2))), 
                                    
                                    1,
                                 
                        if_else(((RS_14 > 80 & lag(RS_14) < 80 & lag(RS_14, k = 2) < 75 &
                                        Volume > 2 * lag(rollmean(x = Volume, 4, align = "right", fill = NA)) &
                                        Close / lag(Close) > 1 & Close / lag(Close) > lag(Close) / lag(Close, k = 2))
                                 
                                    | (RS_14 > 70 & lag(RS_14) < 70 & lag(RS_14, k = 2) < 65 &
                                        Volume > 2 * lag(rollmean(x = Volume, 4, align = "right", fill = NA)) &
                                        Close / lag(Close) > 1 & Close / lag(Close) > lag(Close) / lag(Close, k = 2))
                        
                                    | (RS_14 > 50 & lag(RS_14) < 50 & lag(RS_14, k = 2) < 45 &
                                        Volume > 2 * lag(rollmean(x = Volume, 4, align = "right", fill = NA)) &
                                        Close / lag(Close) > 1 & Close / lag(Close) > lag(Close) / lag(Close, k = 2))),
                                
                                    -1, 0))
)


# Performance Table

performance <- stock_data%>%
            filter(Signal == 1 | Signal == -1)%>%
            mutate(Year = substring(Date, 1, 4), 
                   Purchase_Price = if_else((Signal == 1 & lag(Signal) != 1) | row_number() == min(row_number()), Close, 0), 
                   Sold_Price = if_else(Signal == -1 & lag(Signal) != -1, Close, 0) )%>%
            select(Date, Ticker, Year, Close, Signal, Purchase_Price, Sold_Price)%>%
            filter(!(Purchase_Price == 0 & Sold_Price == 0))%>%
            mutate(Returns = (Sold_Price / (lag(Purchase_Price)) -1)  * 100)%>%
            mutate(Returns = if_else(is.na(Returns) | is.nan(Returns), 0, Returns))%>%
            select(Ticker, Returns)%>%
            summarize(Returns = sum(Returns))
            
