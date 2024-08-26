library(tidyverse)
library(lubridate)

nba.scores.2024 <- read.csv("/Users/awm31/Github/dkustats101fall2024/lectures/Lecture 1.3/www/nba.scores.2024.csv")

nba.scores.2024 <- nba.scores.2024 %>% 
  filter(ymd(date) > "2023-07-01") %>% 
  mutate(at.home = case_when(team == home ~ TRUE,
                          TRUE ~ FALSE),
         div = case_when(team == "BOS" ~ "East",
                         team == "NYK" ~ "East",
                         team == "MIL" ~ "East",
                         team == "CLE" ~ "East",
                         team == "ORL" ~ "East",
                         team == "IND" ~ "East",
                         team == "PHI" ~ "East",
                         team == "MIA" ~ "East",
                         team == "CHI" ~ "East",
                         team == "ATL" ~ "East",
                         team == "BKN" ~ "East",
                         team == "TOR" ~ "East",
                         team == "CHA" ~ "East",
                         team == "WAS" ~ "East",
                         team == "DET" ~ "East",
                         TRUE ~ "West"),
         opponent = case_when(team == home ~ away,
                              TRUE ~ home),
         win = factor(win, levels=c(0,1), labels=c("No", "Yes")))

write.csv(nba.scores.2024, "/Users/awm31/Github/dkustats101fall2024/lectures/Lecture 1.3/www/nbaboxscores.csv")