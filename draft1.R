circuits <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\circuits.csv")
constructor_results <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\constructor_results.csv")
constructor_standings <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\constructor_standings.csv")
constructors <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\constructors.csv")
driver_standings <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\driver_standings.csv")
drivers <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\drivers.csv")
lap_times <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\lap_times.csv")
pit_stops <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\pit_stops.csv")
qualifying <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\qualifying.csv")
races <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\races.csv")
results <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\results.csv")
seasons <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\seasons.csv")
status <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\status.csv")

View(circuits)
View(constructor_results)
View(constructor_standings)
View(constructors)
View(driver_standings)
View(drivers)
View(lap_times)
View(pit_stops)
View(qualifying)
View(races)
View(results)
View(seasons)
View(status)

names(circuits)
names(constructor_results)
names(constructor_standings)
names(constructors)
names(driver_standings)
names(drivers)
names(lap_times)
names(pit_stops)
names(qualifying)
names(races)
names(results)
names(seasons)
names(status)


library("tidyverse")

########### CONSTRUCTOR ############

constructor <- constructor_results %>% 
  rename(race_constructor_points = points) %>% 
  inner_join(constructors, by = "constructorId")


constructor <- constructor_standings %>% 
  rename(standing_constructor_points = points) %>% 
  inner_join(constructor, by = c("constructorId", "raceId"))

# constructor <- results %>% 
#   inner_join(constructor, by = "constructorId")

############# DRIVERS ###############

driver <- driver_standings %>% 
  rename(standing_driver_points = points) %>% 
  inner_join(drivers, by = "driverId")

driver <- lap_times %>% 
  rename(lap_time = time, 
         lap_position = position, 
         lap_milliseconds = milliseconds) %>% 
  inner_join(driver, by = "driverId")

driver <- pit_stops %>% 
  rename(pit_time = time, 
         pit_lap = lap, 
         pit_duration = duration, 
         pit_milliseconds = milliseconds) %>% 
  inner_join(driver, by = "driverId")

driver <- qualifying %>% 
  rename(grid = position) %>% 
  mutate(number = fct_reorder(as.character(number), number)) %>%
  inner_join(driver, by = c("driverId", "number"))