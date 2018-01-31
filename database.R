library(tidyverse)
library(tidytext)
library(DBI)
library(odbc)
library(dbplyr)
source("functions.r")

# Connection String -------------------------------------------------------

con <- dbConnect(odbc::odbc(),
                  Driver = "SQL Server",
                  Server = "DESKTOP-EGAJP7S\\SQLEXPRESS",
                  Database = "SineNome",
                  Port = 1433)


# Create Tables -----------------------------------------------------------

# Filed ETFs

etf_file <- "http://www.etf.com/etf-watch-tables/etf-launches"

etf_file <- XML::readHTMLTable(etf_file, header = TRUE, 
                               stringsAsFactors = FALSE)

file_table <- as.data.frame(etf_file$`NULL`)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))

word_count <- as.data.frame(file_table)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
            unnest_tokens(word, Fund)%>%
            count(word, sort = TRUE)%>%
            mutate(date = Sys.Date())

bigram_count <- as.data.frame(file_table)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
            unnest_tokens(bigram, Fund, token = "ngrams", n = 2)%>%
            count(bigram, sort = TRUE)%>%
            mutate(date = Sys.Date())



etf_file <- "http://www.etf.com/etf-watch-tables/etf-launches"

etf_file <- XML::readHTMLTable(etf_file, header = TRUE, 
                               stringsAsFactors = FALSE)


file_df <- as.data.frame(etf_file$`NULL`)%>%
    filter(Fund != "")%>%
    mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))

tickers <- etf_file_tickers()

top_holds <- list()

for (i in tickers){
    
    error_check <- tryCatch(
        top_holdings(i),
        error = function(e) e)
    if(inherits(error_check, "error")){
        Sys.sleep(1)
        next 
    } else {
        x <- top_holdings(i)
        top_holds[[i]] <- x
        Sys.sleep(1)
    }
}


top_holds_table <- bind_rows(top_holds)

# Extract String Before Writing Table


dbWriteTable(con, "ETF", file_table)
dbWriteTable(con, "FiledWordCounts", word_count)
dbWriteTable(con, "FiledBigramCounts", bigram_count)



