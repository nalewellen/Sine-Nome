library(tidyquant)
library(quantmod)
library(tidyverse)
library(zoo)


# This function calls an API for data and cleans the data

stock_call <- function(x){
    df <- as_tibble(getSymbols(x, env = NULL))%>%
        rownames_to_column()%>%
        magrittr::set_colnames(c("Date","Open", "High", "Low", "Close", "Volume", "Adjusted"))%>%
        mutate(Ticker = x)
    
    return(df)
}

# List of stock tickers -- to be appended by AM

ticks <- c("OZRK", "IBM")

# Create a dataframe from the list of tickers and stock_call

stock_data <- as.data.frame(do.call(rbind,(lapply(ticks, stock_call))))

# Make RS and RS_14

stock_data <- stock_data%>%
                group_by(Ticker)%>%
                arrange(Ticker, Date)%>%
                mutate(Change = Close -lag(Close, default = first(NA)), Gain = if_else(Change > 0, Change, 0),
                       Loss = if_else(Change <= 0, abs(Change), 0), 
                       Avg_Gain = rollapply(Gain, 14, mean, align='right',fill=NA),
                       Avg_Loss = rollapply(Loss, 14, mean, align='right',fill=NA),
                       RS = (Avg_Gain / Avg_Loss), RS_14 = if_else(Avg_Loss == 100, 100, 100 - (100 / (1 + RS))))
 