library(tidyverse)
library(tidytext)
library(XML)
library(rvest)

# ETF Registration Clean DF

etf_registration <- "http://www.etf.com/etf-watch-tables/etfs-registration"

reg_table <- XML::readHTMLTable(etf_registration, header = TRUE, 
                                stringsAsFactors = FALSE)

reg_df <- as.data.frame(reg_table$`NULL`)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
            unnest_tokens(word, Fund)%>%
            count(word, sort = TRUE)    


write_csv(reg_df, "Registration Word Counts.csv")

# ETF Filing Clean DF

etf_file <- "http://www.etf.com/etf-watch-tables/etf-launches"

etf_file <- XML::readHTMLTable(etf_file, header = TRUE, 
                                stringsAsFactors = FALSE)

file_df <- as.data.frame(etf_file$`NULL`)%>%
            filter(Fund != "")%>%
            mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
            group_by(Launch)%>%
            unnest_tokens(word, Fund)%>%
            count(word, sort = TRUE)    
    


# Example Filing

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

write_csv(file_text, "Example ETF Filing.csv")










