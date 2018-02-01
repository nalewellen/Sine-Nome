library(tidyverse)

# Functions


# This function returns a list of newly filed ETFs

etf_file_tickers <- function(){
    
    etf_file <- "http://www.etf.com/etf-watch-tables/etf-launches"
    
    etf_file <- XML::readHTMLTable(etf_file, header = TRUE, 
                                   stringsAsFactors = FALSE)
    
    
    file_df <- as.data.frame(etf_file$`NULL`)%>%
        filter(Fund != "")%>%
        mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))
    
    tickers <- as.list(file_df$Ticker)
    
}

# This function pulls an ETFs top 10 holdings

top_holdings <- function(ticker){
    
    holds <- paste0("http://etfdb.com/etf/",ticker,"/#etf-holdings&sort_name=weight&sort_order=desc&page=1")
    
    table <- read_html(holds)%>%
        html_node("#etf-holdings")%>%
        html_table()%>%
        slice(2:n()-1)%>%
        mutate(Ticker = ticker, Date = Sys.Date())
    
    return(table)
    
}

# This function calls an API for data and cleans the data

stock_call <- function(stock_ticker){
    df <- as_tibble(getSymbols(stock_ticker, env = NULL))%>%
        rownames_to_column()%>%
        magrittr::set_colnames(c("Date","Open", "High", "Low", "Close", "Volume", "Adjusted"))%>%
        mutate(Ticker = stock_ticker)
    
    return(df)
}
