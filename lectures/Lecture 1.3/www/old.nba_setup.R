library(tidyverse)

western_conf <- c("DAL", "DEN", "GSW", "HOU", "LAC", "LAL", "MEM", "MIN", "NOP", "OKC", "PHX", "POR", "SAC", "SAS", "UTA")
divisions <- as.factor(c("West", "East"))

nbaboxscores <- read.csv("www/nba_games.csv")

nbaboxscores <- nbaboxscores %>% 
  filter(SEASON==2022) %>% 
  mutate(HOME_TEAM_ID = case_match(HOME_TEAM_ID,
                                   1610612737 ~ "ATL",
                                   1610612738 ~ "BOS",
                                   1610612740 ~ "NOP",
                                   1610612741 ~ "CHI",
                                   1610612742 ~ "DAL",
                                   1610612743 ~ "DEN",
                                   1610612745 ~ "HOU",
                                   1610612746 ~ "LAC",
                                   1610612747 ~ "LAL",
                                   1610612748 ~ "MIA",
                                   1610612749 ~ "MIL",
                                   1610612750 ~ "MIN",
                                   1610612751 ~ "BKN",
                                   1610612752 ~ "NYK",
                                   1610612753 ~ "ORL",
                                   1610612754 ~ "IND",
                                   1610612755 ~ "PHI",
                                   1610612756 ~ "PHX",
                                   1610612757 ~ "POR",
                                   1610612758 ~ "SAC",
                                   1610612759 ~ "SAS",
                                   1610612760 ~ "OKC",
                                   1610612761 ~ "TOR",
                                   1610612762 ~ "UTA",
                                   1610612763 ~ "MEM",
                                   1610612764 ~ "WAS",
                                   1610612765 ~ "DET",
                                   1610612766 ~ "CHA",
                                   1610612739 ~ "CLE",
                                   1610612744 ~ "GSW")) %>% 
  mutate(VISITOR_TEAM_ID = case_match(VISITOR_TEAM_ID,
                                   1610612737 ~ "ATL",
                                   1610612738 ~ "BOS",
                                   1610612740 ~ "NOP",
                                   1610612741 ~ "CHI",
                                   1610612742 ~ "DAL",
                                   1610612743 ~ "DEN",
                                   1610612745 ~ "HOU",
                                   1610612746 ~ "LAC",
                                   1610612747 ~ "LAL",
                                   1610612748 ~ "MIA",
                                   1610612749 ~ "MIL",
                                   1610612750 ~ "MIN",
                                   1610612751 ~ "BKN",
                                   1610612752 ~ "NYK",
                                   1610612753 ~ "ORL",
                                   1610612754 ~ "IND",
                                   1610612755 ~ "PHI",
                                   1610612756 ~ "PHX",
                                   1610612757 ~ "POR",
                                   1610612758 ~ "SAC",
                                   1610612759 ~ "SAS",
                                   1610612760 ~ "OKC",
                                   1610612761 ~ "TOR",
                                   1610612762 ~ "UTA",
                                   1610612763 ~ "MEM",
                                   1610612764 ~ "WAS",
                                   1610612765 ~ "DET",
                                   1610612766 ~ "CHA",
                                   1610612739 ~ "CLE",
                                   1610612744 ~ "GSW")) %>% 
  mutate(DIV_HOME = ifelse(HOME_TEAM_ID %in% western_conf, "West", "East")) %>%
  mutate(DIV_AWAY = ifelse(VISITOR_TEAM_ID %in% western_conf, "West", "East")) %>%
  mutate(across(c(DIV_HOME, DIV_AWAY), as.factor)) %>% 
  mutate(HOME_TEAM_WINS = factor(HOME_TEAM_WINS, levels=c(0, 1), labels=c("Loss", "Win")))

write.csv(nbaboxscores, file="www/nba_games_short.csv")