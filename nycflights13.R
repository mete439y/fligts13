
install.packages("tidyverse")
install.packages("nycflights13")

suppressPackageStartupMessages(library(tidyverse))

suppressPackageStartupMessages(library(nycflights13))

Q1 <- flights%>%
  group_by(carrier)%>%
  summarise(dis_mean =round(mean(distance), 2))%>%
  filter(carrier %in% c("AA", "EV", "FL"))%>%
  as.data.frame()
Q1

Q2 <- flights %>% 
  group_by(month) %>% 
  summarize(total = sum(flight))%>%
  arrange(desc(total))%>%
  head(1) %>%
  as.data.frame()
Q2

Q3 <- flights%>%
  group_by(origin, dest)%>%
  summarise(min_dist = max(distance), .groups = 'drop')%>%
  arrange((min_dist), head = FALSE)%>%
  as.data.frame()%>%
  head(5)
Q3

Q4 <- flights%>%
  group_by(month, day)%>%
  filter(origin == "JFK")%>%
  summarise(mean_destance = round(mean(distance), 2), .groups = 'drop')%>%
  arrange((mean_destance))%>%
  head(5)%>%
  as.data.frame()
Q4

Q5 <- flights%>%
  group_by(dest)%>%
  filter(!is.na(arr_delay))%>%
  summarise(max_arr_delay = max(arr_delay))%>%
  filter(dest == "BOS" | dest == "ATL")
Q5
