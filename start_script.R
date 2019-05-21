library(tidyverse)
library(rvest)

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
