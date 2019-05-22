library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(lubridate)
root <- root <- "https://api.iextrading.com/1.0/stock/"

wiki_table <- url %>%
  read_html() %>%
  html_node(".wikitable") %>%
  html_table()

#Finding double counted values
tst <- wiki_table %>%
  group_by(CIK) %>%
  summarise(double_count = n()) %>%
  arrange(desc(double_count)) %>%
  slice(1:5) %>%
  left_join(wiki_table, by = "CIK") %>%
  select(Symbol, Security)
tst

get_financials <- function(ticker){
  end_url <- "/financials?period=annual"
  temp <- GET(paste(root, ticker, end_url,sep="")) %>%
    content("text") %>%
    fromJSON()
  as.data.frame(temp[2])
}

get_prices <- function(ticker){
  filter <- "?filter=date,close,volume"
  url <- paste(root, ticker, "/chart/5y",filter,sep="") 
  temp <- GET(url) %>%
    content("text") %>%
    fromJSON %>%
    mutate(ticker = ticker)
  temp
}

calc_3month <- function(stock){
 temp_ticker <- stock$ticker[1] 
 temp <- stock %>%
   mutate(yq = quarter(date, with_year=TRUE)) %>%
   group_by(yq) %>%
   summarise(average_close=mean(close))
 temp_1 <- slice(temp, 1:nrow(temp) - 1) %>% rowid_to_column()
 temp_2 <- slice(temp, 2:nrow(temp)) %>% rowid_to_column()
 temp_1_close <- pull(temp_1,average_close)
 temp_2_close <- pull(temp_2,average_close)
 return_3month <- (temp_2_close - temp_1_close) / temp_1_close
 temp_2 <- temp_2 %>% 
   cbind(return_3month) %>%
   select(yq, return_3month) %>%
   mutate(ticker = temp_ticker)
 temp_2
}

df <- calc_3month(get_prices(stock_list[1]))

for(i in 2:length(stock_list)){
  print(stock_list[i])
  temp <- calc_3month(get_prices(stock_list[i]))
  df <- rbind(df, temp)
}

df <- df %>%
  arrange(date)

View(df)


df <- df %>%
  select(date, ticker, close, volume, 'GICS Sector', 'GICS Sub Industry')

