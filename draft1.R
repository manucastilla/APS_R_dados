circuits <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\circuits.csv")
constructor_results <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\constructor_results.csv")
constructor_standings <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\constructor_standings.csv")
constructors <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\constructors.csv")
driver_standings <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\driver_standings.csv")
drivers <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\drivers.csv")
lap_times <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\lap_times.csv")
pit_stops <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\pit_stops.csv")
qualifying <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\qualifying.csv")
races <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\races.csv")
results <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\results.csv")
seasons <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\seasons.csv")
status <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS\\status.csv")

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

# CONSTRUCTOR

constructor <- constructors %>% 
                  inner_join(constructor_results, by = "constructorId")
constructor <- constructor %>% 
                  inner_join(constructor_standings, by = c("constructorId", "raceId", "points"))
View(constructor)

# DRIVERS

driver <- drivers %>% 
  inner_join(driver_standings, by = "driverId")

View(driver)
