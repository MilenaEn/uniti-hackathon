library(readr)
library(tidyverse)
library(jsonlite)
library(lubridate)

# 1. import dataframe ----
mydf <- read_csv("answers-29.04.22.csv", 
) %>%
  mutate(answers = str_replace_all(answers, '""', '"'),
         answers = str_replace_all(answers, '\\"\\[', '['),
         answers = str_replace_all(answers, '\\]\\"', ']'),
         client = str_replace_all(client, '""', '"'),
         client = str_replace_all(client, '\\"\\{', '{'),
         client = str_replace_all(client, '\\}\\"', '}')) %>% as_tibble()


mydf <- mydf[-c(41020),]
# t(mydf[x, 5]) line to debug the code from comments of app users


json_list<- list()
for(i in 1:nrow(mydf)){
  json_list[[i]] <- pivot_wider(fromJSON(mydf$answers[i]), names_from = "label", values_from = c("value", "collected_at")) %>% as.tibble()
} # should have used purrr inside the previous mutate, but could not make it work :-(


# 2. Track Your Tinnitus ----


tyt <- mydf %>%
  mutate(app_usage = json_list) %>%
  select(everything(), -c(answers, sensordata, flags, deleted_at)) %>%
  filter(questionnaire_id == 17) %>%
  mutate(app_usage = map(app_usage, ~ select(., #"collected_at",
                                             "value_loudness", "value_cumberness", "value_jawbone",  "value_neck", "value_tin_day", "value_tin_cumber", "value_tin_max", "value_movement", "value_stress", "value_emotion",
                                             "collected_at_loudness", "collected_at_cumberness", "collected_at_jawbone",  "collected_at_neck", "collected_at_tin_day", "collected_at_tin_cumber", "collected_at_tin_max", "collected_at_movement", "collected_at_stress", "collected_at_emotion")))

for(i in 1:nrow(tyt)){
  tyt$app_usage[[i]] <-
    tyt$app_usage[[i]] %>%
    mutate(across(where(is.character), as.numeric))
} # should have used purrr inside the previous mutate, but could not make it work :-(


tyt <- tyt[-4681,]

tyt <- tyt %>%
  unnest(app_usage) %>%
  mutate(across(contains("collected_at_"), as_datetime))



# 3. TinEdu ----

tinedu <- mydf %>%
  mutate(app_usage = json_list) %>%
  select(everything(), -c(answers, sensordata, flags, deleted_at)) %>%
  filter(questionnaire_id == 1) %>%
  unnest(app_usage) %>%
  select(everything(), tip = 9, collected_at_tip = 10) %>%
  mutate(across(contains("collected_at_"), as_datetime))

# 4. Shades of Noise ----


son <- mydf %>%
  mutate(app_usage = json_list) %>%
  select(everything(), -c(answers, sensordata, flags, deleted_at)) %>%
  filter(questionnaire_id == 15)

gambiarra <- map_df(son$app_usage, ~ map_chr(.x, ~ if(is.list(.x)){as.character(.x)}else{.x})) %>%
  mutate(across(contains("collected_at_"), as.numeric),
         across(contains("collected_at_"), as_datetime))

son <- cbind(son[,1:8], gambiarra)

# 5. clean environment ----

rm(json_list, i, gambiarra)

#a <-str_replace_all(son$value_beachwaves[[1]], '"', '')


# 6. select UNITI-BIG patients user_id 6120 - 6656

mydf_big <- mydf %>% filter(user_id >= 6120 & user_id <= 6656)
son_big <- son %>% filter(user_id >= 6120 & user_id <= 6656)
tinedu_big <- tinedu %>% filter(user_id >= 6120 & user_id <= 6656)
tyt_big <- tyt %>% filter(user_id >= 6120 & user_id <= 6656)

# 7. merge questionnaire data


# 8. create variables: time factor

mydf_big %>% 
  mutate(date = as_date(collected_at),
         hour = hour(collected_at),
         weekday = weekdays(collected_at),
         weekend = case_when(weekday == "Samstag" | weekday == "Sonntag" ~ "we",
                                  TRUE ~ "udw")) -> mydf_big 

con_days = rep(0, length(mydf_big$date)) #storage vector

for(i in 1:length(mydf_big$date)){ #loop through the vector
  tmp_juldays = julian(as.Date(mydf_big$date))
  tmp_lfdays = tmp_juldays - min(tmp_juldays) + 1
  con_days = tmp_lfdays
  print(i)
}

mydf_big = data.frame(mydf_big, con_days)
mydf_big <- mydf_big %>% relocate(date, hour, weekday, weekend, con_days, .after = collected_at)


# 9. big: merge mydf with tyt, tinedu & son ----

big <- merge(mydf_big, tyt_big, by="id", all = TRUE)
big <- merge(big, tinedu_big, by="id", all = TRUE)
big <- merge(big, son_big, by="id", all = TRUE)


# 10. big: filter relevant variables + rename ----

big <- select(big,1:3,9:14,25:34,52,61:64,69,71,72,75,76,79:83,89,90,93:104,117:120,125,127:130,135,137,138,141,143,145,147,149:159,171,173:175,179,180,183,185,187,189,191)

big <- big %>% 
  rename(user_id = user_id.x) %>%
  rename(questionnaire_id = questionnaire_id.x) %>%
  rename(collected_at = collected_at.x) %>%
  rename(loudness_cur = value_loudness) %>%
  rename(distress_cur = value_cumberness) %>% 
  rename(jaw_tension = value_jawbone) %>% 
  rename(neck_tension = value_neck) %>% 
  rename(tin_thoughts = value_tin_day) %>% 
  rename(distress_tod = value_tin_cumber) %>% 
  rename(loudness_tod = value_tin_max) %>% 
  rename(movement_tod = value_movement) %>% 
  rename(stress_tod = value_stress) %>% 
  rename(tinedu = tip)


# 11. big: create app usage variable 

big <- big %>% mutate(questionnaire_id=recode(questionnaire_id, `1`="tinedu", `15`="son", `17`="diary"))
big <- filter(big, !is.na(questionnaire_id))
big <- big %>% 
  group_by(user_id) %>% 
  add_count(questionnaire_id) %>% 
  relocate(n, .after = questionnaire_id) %>% 
  ungroup ()
big <- rename(big, n_questionnaire = n)
 

# 12. save big ----
saveRDS(big, file = "big.RDS") 


  
  