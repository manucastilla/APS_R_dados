library(readxl)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(plotly)
library(ggplot2)

f1 <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1.xlsx")
# f1 <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1.xlsx")

f1_18_20 <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1_18_20.xlsx")
# f1_18_20 <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1_18_20.xlsx")

constructor <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1_constructor.xlsx")
# constructor <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1_constructor.xlsx")

View(f1)

names(f1)

############## 2018 - 2020 ##############
##############  Separado  ##############

# Tabela do campeontato -> sugestao: cores iguais para pilotos da mesma equipe (fill = constructor)

# 2020
f1_18_20 %>% 
  filter(year == 2020, round == 17) %>% 
  arrange(standing_driver_points) %>% 
  ggplot(aes(x = reorder(code, -standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2020") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

f1_18_20 %>% 
  filter(year == 2020, round == 17) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, -wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2020") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

# Each drivers gets his own
f1_18_20 %>% 
  filter(year == 2020, code == c("HAM", "BOT", "VER", "PER", "RIC")) %>% 
  ggplot(aes(x = round, y = standing_driver_points, group = code, colour = code)) + 
    geom_line(show.legend = FALSE) + facet_wrap(~ code) +
    scale_x_discrete(breaks=c('R01', 'R06', 'R11', 'R16', 'R21')) +
    labs(title = 'F1 race results 2020, top 5 drivers')

f1_18_20 %>% 
  filter(year == 2020, code == c("HAM", "BOT", "VER", "PER", "RIC")) %>%
  group_by(code) %>% 
  arrange(desc(standing_driver_points)) %>% 
  # head(5) %>% 
  ggplot(aes(x = round, y = position, group = code, colour = code)) + 
  geom_line() +
  scale_x_discrete(breaks=c(1, 6, 11, 16, 21)) +
  labs(title = 'F1 race results 2020, top 5 drivers')


# 2019
f1_18_20 %>% 
  filter(year == 2019, round == 21) %>% 
  arrange(standing_driver_points) %>% 
  ggplot(aes(x = reorder(code, -standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2019") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

f1_18_20 %>% 
  filter(year == 2019, round == 21) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, -wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2019") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")


# 2018
f1_18_20 %>% 
  filter(year == 2018, round == 21) %>% 
  arrange(standing_driver_points) %>% 
  ggplot(aes(x = reorder(code, -standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2018") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

f1_18_20 %>% 
  filter(year == 2018, round == 21) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, -wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2018") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")


############## 2018 - 2020 ##############
##############    Junto    ##############

#melhorar como ta feito esse grafico
# ChampionShip
f1_18_20 %>% 
  filter(code == c("HAM", "BOT", "VER", "PER", "RIC", "VET", "SAI", "LEC")) %>% 
  ggplot(aes(x=year, y=position, color = code, group = code)) +
  geom_point() +
  geom_line() +
  labs(y="Position")+
  facet_grid(code ~ .)

# Nationality
plt1 <- f1_18_20 %>%
  group_by(code) %>% 
  ggplot(aes(code, wins, color = nationality)) +
  geom_point(size = 2) +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  labs(x = "Drivers", y = "Wins", 
       title = "Total Wins around the World", 
       subtitle = "Seasons 2018 to 2020") +
  theme(legend.title = element_blank())

ggplotly(plt1, tooltip = 'wins')


# Nationality
plt2 <- f1_18_20 %>%
  group_by(nationality) %>% 
  ggplot(aes(wins, nationality, color = nationality)) +
  geom_point(size = 2) +
  labs(x = "Wins", y = "Nationalities", 
       title = "Total Wins around the World", 
       subtitle = "Seasons 2018 to 2020") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggplotly(plt2, tooltip = 'wins')



############## Geral ##############

# Nationality
plt3 <- f1 %>%
  group_by(nationality) %>% 
  ggplot(aes(wins, nationality, color = nationality)) +
  geom_point(size = 2) +
  labs(x = "Wins", y = "Nationalities", 
       title = "Total Wins around the World", 
       subtitle = "Seasons 1950 to 2021") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggplotly(plt3, tooltip = 'wins')

f1 %>% 
  #filter(!is.na(standing_driver_points)) %>% 
  filter(year != 2021) %>% 
  summarise(champion_points = max(standing_driver_points)) %>% 
  arrange(desc(year)) %>%
  ggplot(aes(x = year, y = champion_points, color = champion_points)) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Points", title = "Championship") +
  theme(legend.title = element_blank())


############## Constructor ############## 
constructor %>% 
  filter(year > 2015) %>% 
  ggplot(aes(x=year, y=position, color = constructorRef, group = constructorRef)) +
  geom_point() +
  geom_line() +
  labs(y="Position")+
  facet_grid(constructorRef ~ .)



