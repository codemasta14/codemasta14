library(tidyverse)
library(lubridate)
coke <- read_csv("./coke/coke.csv")

coke%>%
  mutate(Date = mdy(Date))


  
dietcoke <- coke%>%
  mutate(Date = paste(Date,Time, sep ="/"),
         Date = mdy_hms(Date),
         Person = str_to_title(Person)
         )%>%
  select(-Time)
  
dietcoke%>%
  ggplot()+
  geom_point(aes(x= Date, y = -hour(Date), color = amount))+
  theme_bw()+
  facet_wrap(~Person)+
  scale_color_gradient(low = "Blue", high = "Yellow")+
  scale_x_datetime(breaks= date_breaks("1 day"), labels = date_format("%d"))+
  theme(axis.text.x = element_text(angle = 0, hjust = .5))  

lubridate::h

lubridate::date(dietcoke$Date)
