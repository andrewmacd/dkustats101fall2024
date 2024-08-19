library(dplyr)

men100 <- Athletics %>% 
  filter(event_name=="Men's 100m")

hist(as.numeric(men100$result))

medals_total <- medals_total %>% 
  mutate(continent = case_when(country_code == "USA" ~ "NAM",
                               country_code == "CHN" ~ "ASI",
                               country_code == "JPN" ~ "ASI",
                               country_code == "AUS" ~ "OCE",
                               country_code == "FRA" ~ "EUR",
                               country_code == "NED" ~ "EUR",
                               country_code == "GBR" ~ "EUR",
                               country_code == "KOR" ~ "ASI",
                               country_code == "ITA" ~ "EUR",
                               country_code == "GER" ~ "EUR",
                               country_code == "NZL" ~ "OCE",
                               country_code == "CAN" ~ "NAM",
                               TRUE ~ "PAN"))

medals.by.cont <- medals_total %>% 
  group_by(continent) %>% 
  summarise(Total = sum(Total))

medals.by.team <- medals %>% 
  mutate(is.team = ifelse(grepl("TEAM", event_type), "TEAM", "IND"))

athletes.total <- athletes %>% 
  group_by(country_code) %>% 
  summarise(Athletes = n())

medals_total_new <- medals_total %>% 
  left_join(athletes.total)

ggplot(medals_total_new, aes(x=log(Total), y=log(Athletes))) +
  geom_point()

write.csv(medals_total_new, "medals_total.csv")