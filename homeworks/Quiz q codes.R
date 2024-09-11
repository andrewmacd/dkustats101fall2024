library(tidyverse)
library(gridExtra)
library(ggthemes)
library(lubridate)

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