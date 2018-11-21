library(tidyverse)
library(httr)
library(fs)

# code from midterm2 to get rep advantage forecast and result
# read result data

x <- read_csv("mt_2_results.csv")

# only house races

y <- x %>% filter(district != "sen",
             district != "gov")

# prepare source for id to join

result <- y %>%
  mutate(source = str_c(str_to_lower(state), district, sep = ""))

# calculate the percentages

result <- result %>%  
  mutate(all = dem_votes + rep_votes + other_votes) %>%
  mutate(rep_adv_r = (rep_votes - dem_votes)/all) %>%
  select(source, rep_adv_r)

# get forecast data

GET("https://goo.gl/ZRCBda", 
    user_agent("Mozilla/5.0"), 
    write_disk("2018-live-poll-results-master.zip"))

# unzip

unzip("2018-live-poll-results-master.zip")

# Create a vector with all the file names in data/.

file_names <- dir_ls("2018-live-poll-results-master/data/")

# Read in all the files into one big tibble.

allpoll <- map_dfr(file_names, read_csv, .id = "source")

# create new variables

all <- allpoll %>% 
  
  # wave
  
  mutate(wave = str_sub(source, -5, -5)) %>%
  
  # state
  
  mutate(source = str_replace(source, "2018-live-poll-results-master/data/elections-poll-", "")) %>%
  mutate(source = str_replace(source, "-..csv", "")) %>%
  mutate(state = str_sub(source, 1, 2)) %>%
  mutate(state = str_to_upper(state)) %>%
  
  # district
  
  mutate(district = str_sub(source, 3, -1)) 

# only house races

house <- all %>% 
  filter(district != "sen",
         district != "gov") %>%
  filter(wave == 3) 

# total interviews each district(weighted)

total <- house %>%
  group_by(source) %>%
  summarise(total = sum(final_weight)) %>%
  ungroup()

# clean data recode response = 3/4/5/6

house$response <- recode(house$response, "3" = "Third", "4" = "Third", "5" = "Third", "6" = "Third")

# create df of each district each response

house1 <- house %>% 
  select(source, response, final_weight) %>% 
  group_by(source, response) %>% 
  tally(wt = final_weight) %>% 
  spread(key = response, value = n) %>%
  ungroup()

house1[is.na(house1)] <- 0  

# calculate the percentages

rep_adv <- house1 %>%  
  mutate(all = Dem + Rep + Und + Third) %>%
  mutate(rep_adv_f = (Rep - Dem)/all) %>%
  select(source, rep_adv_f) 

# create df of each district each age_combined

age <- house %>% 
  filter(age_combined != "[DO NOT READ] Don't know/Refused") %>%
  mutate(age_combined = case_when(
    age_combined == "18 to 29" ~ "young",
    age_combined == "65 and older" ~ "old",
    TRUE ~ "middle_aged"
  )) %>%
  select(source, age_combined, final_weight) %>% 
  group_by(source, age_combined) %>% 
  tally(wt = final_weight) %>% 
  spread(key = age_combined, value = n) %>%
  ungroup()

# calculate the percentages

age <- age %>%  
  left_join(total, by = "source") %>%
  mutate(young = young/total) %>%
  mutate(old = old/total) %>%
  mutate(middle_aged = middle_aged/total) %>%
  select(source, young, old, middle_aged) 

# create df of each district each race_eth

race <- house %>% 
  filter(race_eth != "[DO NOT READ] Don't know/Refused") %>%
  select(source, race_eth, final_weight) %>% 
  group_by(source, race_eth) %>% 
  tally(wt = final_weight) %>% 
  spread(key = race_eth, value = n) %>%
  ungroup()

# calculate the percentages

race <- race %>%  
  left_join(total, by = "source") %>%
  mutate(Asian = Asian/total) %>%
  mutate(Black = Black/total) %>%
  mutate(Hispanic = Hispanic/total) %>%
  mutate(White = White/total) %>%
  mutate(Other = Other/total) %>%
  select(-total) 

# create df of each district each approve

approve <- house %>% 
  filter(approve != "[DO NOT READ] Don't know/Refused") %>%
  select(source, approve, final_weight) %>% 
  group_by(source, approve) %>% 
  tally(wt = final_weight) %>% 
  spread(key = approve, value = n) %>%
  ungroup()

# calculate the percentages

approve <- approve %>%  
  left_join(total, by = "source") %>%
  mutate(Approve = Approve/total) %>%
  mutate(Disapp. = Disapp./total) %>%
  mutate(`Don't know` = `Don't know`/total) %>%
  select(-total) 

# create df of each district each educ4

edu <- house %>%
  filter(educ4 != "[DO NOT READ] Don't know/Refused") %>%
  select(source, educ4, final_weight) %>%
  group_by(source, educ4) %>%
  tally(wt = final_weight) %>%
  spread(key = educ4, value = n) %>%
  ungroup()

# calculate the percentages

edu <- edu %>%
  left_join(total, by = "source") %>%
  mutate(`High School Grad. or Less` = `High School Grad. or Less`/total) %>%
  mutate(`4-year College Grad.` = `4-year College Grad.`/total) %>%
  mutate(`Some College Educ.` = `Some College Educ.`/total) %>%
  mutate(`Postgraduate Degree` = `Postgraduate Degree`/total) 


# join forecast and result data

error <- rep_adv %>%
  left_join(result, by = "source") %>%  
  mutate(error = rep_adv_r - rep_adv_f) %>%
  select(source, error)

age <- error %>%
  left_join(age, by = "source") 

race <- error %>%
  left_join(race, by = "source") 

edu <- error %>%
  left_join(edu, by = "source") 

approve <- error %>%
  left_join(approve, by = "source") 

# Write the data to an RDS file to use in the app 

write_rds(error, "Where_Comes_The_Error/error.rds")
write_rds(age, "Where_Comes_The_Error/age.rds")
write_rds(race, "Where_Comes_The_Error/race.rds")
write_rds(edu, "Where_Comes_The_Error/edu.rds")
write_rds(approve, "Where_Comes_The_Error/approve.rds")

