library(readxl)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(plotly)

#f1 <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1.xlsx")
f1 <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1.xlsx")

View(f1)

names(f1)

f1 %>% 
  #filter(!is.na(standing_driver_points)) %>% 
  filter(year != 2021) %>% 
  summarise(champion_points = max(standing_driver_points)) %>% 
  arrange(desc(year)) %>%
    ggplot(aes(x = year, y = champion_points, color = champion_points)) +
    geom_point(size = 2) +
    labs(x = "Year", y = "Points", title = "Championship") +
    theme(legend.title = element_blank())


f1 %>% 
  filter(year == 2020) %>% 
  ggplot(aes(x = code, y = wins)) +
  geom_col() +
  labs(x = "Year", y = "Points", title = "Championship") +
  theme(legend.title = element_blank())
  