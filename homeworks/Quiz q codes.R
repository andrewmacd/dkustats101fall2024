library(tidyverse)
library(gridExtra)
library(ggthemes)
library(lubridate)
library(broom)

theme_set(theme_clean())

# Quiz 2.1

## Q1

q1nba <- nba.scores.2024 %>% 
  filter(ymd(date) < ymd("2023-12-1") & ymd(date) > ymd("2023-10-31"))

c1 <- cor(q1nba$PTS, q1nba$STL)
c2 <- cor(q1nba$PTS, q1nba$AST)
c3 <- cor(q1nba$PTS, q1nba$REB)
c4 <- cor(q1nba$PTS, q1nba$TOV)
c5 <- cor(q1nba$PTS, q1nba$FG.)
c6 <- cor(q1nba$PTS, q1nba$X3P.)
c7 <- cor(q1nba$PTS, q1nba$PF)


p1 <- ggplot(q1nba, aes(x=TOV, y=PTS)) + geom_jitter(color="blue") + labs(y="Points", x="Turnovers") 
p2 <- ggplot(q1nba, aes(x=AST, y=PTS)) + geom_jitter(color="blue") + labs(y="Points", x="Assists")
p3 <- ggplot(q1nba, aes(x=REB, y=PTS)) + geom_jitter(color="blue") + labs(y="Points", x="Rebounds")
p4 <- ggplot(q1nba, aes(x=FG., y=PTS)) + geom_jitter(color="blue") + labs(y="Points", x="Field goal percentage") 

grid.arrange(p1, p2, p3, p4, ncol=2, top="Data from the NBA in November 2024")

# Quiz 2.2

## Q1

model <- summary(lm(PTS ~ AST, data=nba.scores.2024))

## Q3

ggplot(nba.scores.2024, aes(x = model$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

# Quiz 2.3

weather <- Rainier_Weather %>% 
  mutate(temp.c = (Temperature.AVG-32)*(5/9)) %>% 
  mutate(wind.spd.kph = Wind.Speed.Daily.AVG * 1.609) %>% 
  filter(wind.spd.kph != 0)

temp.reg <- lm(data=weather, temp.c ~ wind.spd.kph)

summary(temp.reg)

temp.reg.aug <- augment(temp.reg, weather)

ggplot(temp.reg.aug, aes(x=.fitted, y=.resid)) + 
  geom_point() +
  geom_hline(yintercept=0) +
  labs(x="Fitted", y="Residual", title="Residual plot", subtitle="Temperature vs. wind speed")

# Quiz 2.4

library(tidyverse)
library(modelsummary)

boats <- Boats_Cleaned_dataset %>% 
  mutate(condition)

#boats.sample <- boats %>% 
#  slice_sample(n=100)

boats.sample <- boats.sample %>% 
  mutate(condition = as.factor(condition))

model <- lm(data=boats.sample, price ~ length_ft + totalHP)

models <- list(
  "Boat price model" = model
)

summary(model)

model <- lm(data=boats.sample, price ~ length_ft + totalHP + condition)

library(broom)

mod.aug <- augment(model) 

options(scipen=999)


ggplot(mod.aug, aes(.resid)) +
  geom_histogram() +
  geom_vline(xintercept=0) +
  labs(x="Residual", title="Histogram of residuals")
