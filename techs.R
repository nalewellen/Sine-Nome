library(tidyquant)
library(quantmod)
library(tidyverse)



ozrk <- as_tibble(getSymbols("OZRK", env = NULL))%>%
            rownames_to_column()%>%
            magrittr::set_colnames(c("Date","Open", "High", "Low", "Close", "Volume", "Adjusted"))%>%
            mutate(Ticker = "OZRK")