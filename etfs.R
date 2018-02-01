library(tidyverse)
library(tidytext)
library(XML)
library(rvest)
source("functions.R")

# ETF Registration Clean DF

etf_registration <- "http://www.etf.com/etf-watch-tables/etfs-registration"

reg_table <- XML::readHTMLTable(etf_registration, header = TRUE, 
                                stringsAsFactors = FALSE)

# Word Count

word_count <- as.data.frame(reg_table$`NULL`)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
            unnest_tokens(word, Fund)%>%
            count(word, sort = TRUE)%>%
            mutate(date = Sys.Date())

# write_csv(word_count, "Registration Word Counts.csv")

# 2 Word Combinations

bigram_count <- as.data.frame(reg_table$`NULL`)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
            unnest_tokens(bigram, Fund, token = "ngrams", n = 2)%>%
            count(bigram, sort = TRUE)%>%
            mutate(date = Sys.Date())

# write_csv(bigram_count, "Bigram Word Counts.csv")

# ETF Filing Clean DF

etf_file <- "http://www.etf.com/etf-watch-tables/etf-launches"

etf_file <- XML::readHTMLTable(etf_file, header = TRUE, 
                                stringsAsFactors = FALSE)


file_df <- as.data.frame(etf_file$`NULL`)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))

# List of tickers

tickers <- etf_file_tickers()

# Newly filed ETF top 10 holdings

test <- "HNDL"

# This function pulls an ETFs top 10 holdings

example <- top_holdings(test)

# Top holdings by newly filed tickers

top_holds <- list()

# This loop will bring back a list of the ticker's top stock holdings

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
        browser()
    }
}

# write_csv(top_holds_table, "Stock Holdings by Filed ETFs.csv")

top_holds_table <- bind_rows(top_holds)

# New Filing Word Counts

file_counts <- as.data.frame(etf_file$`NULL`)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
            group_by(Launch)%>%
            unnest_tokens(word, Fund)%>%
            count(word, sort = TRUE)    


# Example Filing
# Similar code will be used for 10-Ks and for Earnings Calls
# Move this to another R program

filing <- "https://www.sec.gov/Archives/edgar/data/1415726/000160900615000184/academy485a10162015.htm"

file_text <- filing%>%
                read_html()%>%
                html_nodes("div font")%>%
                html_text()%>%
                as.tibble()%>%
                unnest_tokens(word, value)%>%
                count(word, sort = TRUE)

bigram_text <- filing%>%
                read_html()%>%
                html_nodes("div font")%>%
                html_text()%>%
                as.tibble()%>%
                unnest_tokens(word, value)

#write_csv(file_text, "Example ETF Filing.csv")