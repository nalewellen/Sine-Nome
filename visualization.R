library(tidyverse)
source("functions.r")

# Data 

word_count <- as.data.frame(reg_table$`NULL`)%>%
    filter(Fund != "")%>%
    mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
    unnest_tokens(word, Fund)%>%
    count(word, sort = TRUE)%>%
    mutate(date = Sys.Date())

bigram_count <- as.data.frame(reg_table$`NULL`)%>%
    filter(Fund != "")%>%
    mutate_all(.funs = function(x) replace(x, which(x == "N/A" | x == ""), NA))%>%
    unnest_tokens(bigram, Fund, token = "ngrams", n = 2)%>%
    count(bigram, sort = TRUE)%>%
    mutate(date = Sys.Date())

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

bigram_chart <- bigram_count %>%
    filter(n >= 32)%>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot(aes(bigram, n))+
    geom_col(fill = "steel blue")+
    xlab(" ")+
    coord_flip()+
    ggtitle("Two Word Combinations for Registered ETFs")+
    theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank())