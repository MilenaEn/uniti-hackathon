library(readr)
library(tidyverse)


# 1. load data frame sced1 ----

sced <- readRDS(file = "sced1.RDS") 


# 2. subset subjects for demographics 

sub <- sced %>%
  group_by(case_id) %>%
  arrange(con_days) %>%
  filter(row_number()==1) %>%
  select(1:3,6:8,17:42) %>% 
  arrange(case_id) %>% 
  ungroup()


