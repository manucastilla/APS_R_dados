library(readxl)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(plotly)
library(ggplot2)
library(dplyr)

qualifying <- read_csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\qualifying.csv")
# qualifying <- read.csv("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\qualifying.csv")

f1 <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1.xlsx")
# f1 <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1.xlsx")

f1_18_20 <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1_18_20.xlsx")
# f1_18_20 <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1_18_20.xlsx")

constructor <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1_constructor.xlsx")
# constructor <- read_excel("C:\\Users\\Victor Habib\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\f1_constructor.xlsx")

View(f1)

names(constructor)

############## 2018 - 2020 ##############
##############  Separado  ##############

##################
###### 2020 ######
##################

# Classifica??o Cameponato
f1_18_20 %>% 
  filter(year == 2020, round == 17) %>% 
  ggplot(aes(x = reorder(code, standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2020") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()
    

# Quantidade de vit?rias por piloto
f1_18_20 %>% 
  filter(year == 2020, round == 17) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2020") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()


# # Each drivers gets his own
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

# install.packages("hablar")
library(hablar)

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
       title = "Overtakes - 1? ao 5? colocado", 
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
       title = "Overtakes - 6? ao 10? colocado", 
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
       title = "Overtakes - 11? ao 15? colocado", 
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
       title = "Overtakes - 16? ao 20? colocado", 
       subtitle = "Season 2020") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank())



##################
###### 2019 ######
##################

# Classificacao Campeonato
f1_18_20 %>% 
  filter(year == 2019, round == 21) %>% 
  ggplot(aes(x = reorder(code, standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2019") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
    coord_flip()


# Quantidade de vit?rias por piloto
f1_18_20 %>% 
  filter(year == 2019, round == 21) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2019") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()


##################
###### 2018 ######
##################


# Classifica??o Campeonato
f1_18_20 %>% 
  filter(year == 2018, round == 21) %>% 
  ggplot(aes(x = reorder(code, standing_driver_points), y = standing_driver_points, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Points", 
       title = "Championship", 
       subtitle = "Season 2018") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
    coord_flip()


# Quantidade de vit?rias por piloto
f1_18_20 %>% 
  filter(year == 2018, round == 21) %>% 
  arrange(wins) %>% 
  ggplot(aes(x = reorder(code, wins), y = wins, fill = code)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Championship", 
       subtitle = "Season 2018") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()


#########################################
############## 2018 - 2020 ##############
##############    Junto    ##############


#melhorar como ta feito esse grafico
# ChampionShip
# f1_18_20 %>% 
#   filter(code == c("VET", "SAI", "LEC", "ALB", "GAS", "NOR", "RUS")) %>% 
#   ggplot(aes(year, position, color = code, group = code)) +
#   geom_point() +
#   geom_line() +
#   labs(y="Position")+
#   facet_grid(code ~ .)

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
  geom_point(size = 4) +
  labs(x = "Wins", y = "Nationalities", 
       title = "Total Wins around the World", 
       subtitle = "All Seasons") +
  scale_x_discrete(guide = guide_axis(angle = 70)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggplotly(plt4, tooltip = 'wins')


#GR?FICO QUE MOSTRA QUAL PILOTO VENCEU MAIS EM MONACO
f1 %>% 
  filter(position == 1, TrackName == "Monaco Grand Prix") %>%
  group_by(driverRef) %>% 
  ggplot(aes(x = reorder(driverRef, position),  y = position, fill = driverRef)) +
  geom_col() +
  labs(x = "Driver", y = "Wins", 
       title = "Monaco Grand Prix", 
       subtitle = "All Seasons") +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()


#GR?FICO QUE MOSTRA QUAL PILOTO VENCEU MAIS NA HIST?RIA
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
  labs(title = "Resultado final de construtores ao longo dos ?ltimos 5 anos")

names(constructor)
