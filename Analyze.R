library(readxl)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(plotly)
library(ggplot2)
library(dplyr)

# qualifying <- read_csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\qualifying.csv")
qualifying <- read.csv("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\qualifying.csv")

# f1 <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1.xlsx")
f1 <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1.xlsx")

# f1_18_20 <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1_18_20.xlsx")
f1_18_20 <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1_18_20.xlsx")

# constructor <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1_constructor.xlsx")
constructor <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1_constructor.xlsx")

View(f1)

names(constructor)

############## 2018 - 2020 ##############
##############  Separado  ##############

# Tabela do campeontato -> sugestao: cores iguais para pilotos da mesma equipe (fill = constructor)

# 2020
f1_18_20 %>% 
  filter(year == 2020, round == 17) %>% 
  #arrange(desc(standing_driver_points)) %>% 
  ggplot(aes(x = reorder(code, standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2020") +
  #scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()
    

f1_18_20 %>% 
  filter(year == 2020, round == 17) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2020") +
  #scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()

# Each drivers gets his own
f1_18_20 %>% 
  filter(year == 2020, code == c("HAM", "BOT", "VER", "PER", "RIC")) %>% 
  ggplot(aes(x = round, y = standing_driver_points, group = code, colour = code)) + 
    geom_line(show.legend = FALSE) + 
    facet_wrap(~ code) +
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


########## OVERTAKES ###########

View(qualifying)

names(qualifying)
names(f1)

library(hablar)
# install.packages("hablar")

f1 <- f1 %>% 
  convert(int(raceId))

f1_ot <- qualifying %>% 
  rename(grid = position) %>% 
  select(c("grid", "driverId", "raceId")) %>% 
  inner_join(f1, by = c("driverId", "raceId"))


#5 PRIMEIROS
f1_ot %>%
  filter(year == "2020", code %in% c("HAM", "BOT", "VER", "PER", "RIC")) %>% 
  mutate(overtakes = (grid - position)) %>% 
  ggplot(aes(x = reorder(TrackName, round), y = overtakes, fill = code)) +
  geom_col() +
  labs(x = "Race", y = "Overtakes", 
       title = "Overtakes - 1º ao 5º colocado", 
       subtitle = "Season 2020") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank())


#6-10
f1_ot %>%
  filter(year == "2020", code %in% c("SAI", "ALB", "LEC", "NOR", "GAS")) %>% 
  mutate(overtakes = (grid - position)) %>% 
  ggplot(aes(x = reorder(TrackName, round), y = overtakes, fill = code)) +
  geom_col() +
  labs(x = "Race", y = "Overtakes", 
       title = "Overtakes - 6º ao 10º colocado", 
       subtitle = "Season 2020") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank())


#11-15
f1_ot %>%
  filter(year == "2020", code %in% c("STR", "OCO", "VET", "KVY", "RAI")) %>% 
  mutate(overtakes = (grid - position)) %>% 
  ggplot(aes(x = reorder(TrackName, round), y = overtakes, fill = code)) +
  geom_col() +
  labs(x = "Race", y = "Overtakes", 
       title = "Overtakes - 11º ao 15º colocado", 
       subtitle = "Season 2020") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank())


#16-20
f1_ot %>%
  filter(year == "2020", code %in% c("GIO", "RUS", "GRO", "MAG", "LAT")) %>% 
  mutate(overtakes = (grid - position)) %>% 
  ggplot(aes(x = reorder(TrackName, round), y = overtakes, fill = code)) +
  geom_col() +
  labs(x = "Race", y = "Overtakes", 
       title = "Overtakes - 16º ao 20º colocado", 
       subtitle = "Season 2020") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank())




# 2019
f1_18_20 %>% 
  filter(year == 2019, round == 21) %>% 
  #arrange(standing_driver_points) %>% 
  ggplot(aes(x = reorder(code, standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2019") +
  #scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none") +
    coord_flip()

f1_18_20 %>% 
  filter(year == 2019, round == 21) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2019") +
  #scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()


# 2018
f1_18_20 %>% 
  filter(year == 2018, round == 21) %>% 
  #arrange(standing_driver_points) %>% 
  ggplot(aes(x = reorder(code, standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2018") +
  #scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none") +
    coord_flip()

f1_18_20 %>% 
  filter(year == 2018, round == 21) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2018") +
  #scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()


############## 2018 - 2020 ##############
##############    Junto    ##############

#TENTATIVA DE PLOTAR AS TABELAS DO CAMPEONATO JUNTO
#obs: tentar mudar a ordem do ano de 2019 (deixar na ordem)
f1_18_20 %>% 
  filter(round == 21) %>% 
  #arrange(desc(standing_driver_points)) %>% 
  ggplot(aes(x = reorder(code, standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  facet_wrap(~ year) +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Seasons 2018-19") +
  #scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()


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

View(f1_18_20)
#GR?FICO QUE MOSTRA QUAL PILOTO VENCEU MAIS EM CADA PISTA
plt3 <- f1_18_20 %>% 
  group_by(TrackName) %>% 
  filter(position == 1) %>% #, code %in% c("HAM", "BOT", "VER", "PER", "RIC")) %>% 
  arrange(wins) %>%
  ggplot(aes(x = reorder(TrackName, -position), y = position, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Seasons 2018-2020") +
  # scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank()) +
  coord_flip()


f1_18_20 %>% filter(TrackName == "Italian Grand Prix", year == "2020", position == 1)

ggplotly(plt3, tooltip = 'wins')

# IDEIA: ACRESCENTAR A POSICAO DE GRID DO QUALLIFYING E CRIAR UMA COLUNA QUE 
# CALCULA QUANTAS ULTRAPASSAGENS FORAM FEITAS NA CORRIDA:
# POSITION_RACE - POSITION_GRID (EM QUE SER NUMERO PRA FAZER A CONTA)
# PLOTAR UM GRAFICO QUE MOSTR AS ULTRAPASSAGENS (PILOTO QUE MAIS ULTRAPASSOU AO
# LONGO DO ANO -> CHEQUITO CTZ RS)
# PROBLEMA: N?O TA RODANDO O ARQUIVO "CLEANING" 

############## Geral ##############

# Nationality
plt4 <- f1 %>%
  group_by(nationality) %>% 
  ggplot(aes(wins, nationality, color = nationality)) +
  geom_point(size = 2) +
  labs(x = "Wins", y = "Nationalities", 
       title = "Total Wins around the World", 
       subtitle = "Seasons 1950 to 2021") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggplotly(plt4, tooltip = 'wins')

f1 %>% 
  #filter(!is.na(standing_driver_points)) %>% 
  filter(year != 2021) %>% 
  summarise(champion_points = max(standing_driver_points)) %>% 
  arrange(desc(year)) %>%
  ggplot(aes(x = year, y = champion_points, color = champion_points)) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Points", title = "Championship") +
  theme(legend.title = element_blank())

#GR?FICO QUE MOSTRA QUAL PILOTO VENCEU MAIS EM MONACO
f1 %>% 
  filter(position == 1, TrackName == "Monaco Grand Prix") %>%
  ggplot(aes(x = driverRef, y = position, fill = driverRef)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Monaco Grand Prix", 
       subtitle = "All Seasons") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()


#GR?FICO QUE MOSTRA QUAL PILOTO VENCEU MAIS NA HISTÓRIA
f1 %>% 
  filter(position == 1) %>% 
  ggplot(aes(x = reorder(driverRef, position), y = position, fill = driverRef)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Seasons 1950-2020") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()



############## Constructor ############## 
constructor %>% 
  filter(year.x > 2015) %>% 
  ggplot(aes(x=year.x, y=position, color = constructorRef, group = constructorRef)) +
  geom_point() +
  geom_line() +
  labs(x = "", y="Position", title = "Resultado final de cada construtora")+
  facet_grid(constructorRef ~ .)


#TENTATIVA
constructor %>% 
  filter(year.x > 2015) %>% 
  group_by(raceId) %>%
  ggplot(aes(x = year.x, y = position, group = constructorRef, colour = constructorRef)) + 
  geom_line() + 
  theme(legend.title = element_blank()) +
  #facet_wrap(~ constructorRef) +
  #scale_x_discrete(breaks=c('R01', 'R06', 'R11', 'R16', 'R21')) +
  labs(title = "Resultado final de construtores ao longo dos últimos 5 anos")

names(constructor)
