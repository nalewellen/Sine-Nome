library(tidyverse)
library(tidytext)
library(XML)
library(rvest)

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

# Bar Charts 
    
word_chart <- word_count %>%
            filter(n >= 35)%>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n))+
                geom_col(fill = "steel blue")+
                xlab(" ")+
                coord_flip()+
                ggtitle("Single Word Counts for Registered ETFs")+
                theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank())

ggsave("WordCountChart.png")

bigram_chart <- bigram_count %>%
            filter(n >= 32)%>%
            mutate(bigram = reorder(bigram, n)) %>%
            ggplot(aes(bigram, n))+
                geom_col(fill = "steel blue")+
                xlab(" ")+
                coord_flip()+
                ggtitle("Two Word Combinations for Registered ETFs")+
                theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank())

ggsave("BigramChart.png")

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

#write_csv(file_text, "Example ETF Filing.csv")










