library(tidyverse)
library(lubridate)

kc.houses <- read.csv("www/kingcountysales.csv")

kc.houses$sale_date <- ymd(kc.houses$sale_date) 

kc.houses.recent <- kc.houses %>% 
  filter(sale_date > ymd("2022-01-01"))

write.csv(kc.houses.recent, "www/king.county.sales.recent")
