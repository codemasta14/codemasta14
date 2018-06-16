library(tidyverse)
library(lubridate)
library(stringr)

fastfood <- c("TACO BELL|PITA PIT|CHICK-FIL-A|PANDA EXPRESS|MCDONALD\'S|WENDY\'S|SWIG|WENDYS|DON PEDRO\`S|SUBWAY|CARL'S JR|WHSE|Dominos|TERIYAKI")
gas <- c("GAS|CHEVRON")

bank <- read_csv("./other/secret.csv")%>%
  mutate(date = mdy(`Posting Date`),
         type = case_when(
           str_detect(Description,fastfood) ~ "Fast Food",
           str_detect(Description,gas)|
             (str_detect(Description,"WM") & str_detect(Description,"VANCOUVER") & Balance > 10) ~ "Gas"
         )
         )%>%
  select(-`Posting Date`)%>%
  filter(date > ymd("2018-04-13"))


bank%>%
  summarize(total =sum(Amount))
