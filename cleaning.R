# circuits <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\circuits.csv")
# constructor_results <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\constructor_results.csv")
# constructor_standings <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\constructor_standings.csv")
# constructors <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\constructors.csv")
# driver_standings <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\driver_standings.csv")
# drivers <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\drivers.csv")
# lap_times <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\lap_times.csv")
# pit_stops <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\pit_stops.csv")
# qualifying <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\qualifying.csv")
# races <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\races.csv")
# results <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\results.csv")
# seasons <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\seasons.csv")
# status <- read.csv("C:\\Users\\VICTOR HABIB\\Documents\\INSPER\\7_semestre\\R_para_dados\\APS_R_dados\\status.csv")

circuits <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\circuits.csv")
constructor_results <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\constructor_results.csv")
constructor_standings <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\constructor_standings.csv")
constructors <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\constructors.csv")
driver_standings <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\driver_standings.csv")
drivers <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\drivers.csv")
lap_times <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\lap_times.csv")
pit_stops <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\pit_stops.csv")
qualifying <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\qualifying.csv")
races <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\races.csv")
results <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\results.csv")
seasons <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\seasons.csv")
status <- read.csv("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\status.csv")


# View(circuits)
# View(constructor_results)
# View(constructor_standings)
# View(constructors)
# View(driver_standings)
# View(drivers)
# View(lap_times)
# View(pit_stops)
# View(qualifying)
# View(races)
# View(results)
# View(seasons)
# View(status)

names(circuits)
names(constructor_results)
names(constructor_standings)
names(constructors)

names(lap_times)
names(pit_stops)

names(seasons)
names(status)

names(driver_standings)
names(drivers)
names(qualifying)
names(races)
names(results)


library("tidyverse")

races_year <- races %>% 
  select(year, raceId)


########### CONSTRUCTOR ############
## para renomear usar select em vez de usar rename
constructor_results <- select(constructor_results, -c(constructorResultsId, status))
constructors <- select(constructors, -c(name, url))

constructor <- constructor_results %>% 
  rename(race_constructor_points = points) %>% 
  inner_join(constructors, by = "constructorId")

constructor_standings <- select(constructor_standings, -c(constructorStandingsId, positionText ))
constructor <- constructor_standings %>% 
  rename(standing_constructor_points = points) %>% 
  inner_join(constructor, by = c("constructorId", "raceId"))

constructor <- constructor %>% 
  filter(raceId <= 1073)

constructor <- races_year %>% 
  inner_join(constructor, by = c("raceId"))

# constructor <- results %>% 
#   inner_join(constructor, by = "constructorId")

############# DRIVERS ###############

names(drivers)
names(driver_standings)

driver <- driver_standings %>% 
  rename(standing_driver_points = points) %>% 
  inner_join(drivers, by = "driverId")

driver <- select(driver, -c(url, number, forename, surname, dob,
                            driverStandingsId, positionText))

driver <- driver %>% 
  filter(raceId <= 1073)

names(qualifying)

# qualifying <-  select(qualifying, -c(q1, q2, q3, qualifyId, number, 
#                                      constructorId)) 

# driver <- qualifying %>% 
#   rename(grid = position) %>% 
#   filter(raceId <= 1073) %>% 
#   inner_join(driver, by = "raceId")

names(races)

races <- select(races, -c(url, time, date))
                
driver <- races %>% 
  rename(TrackName = name) %>% 
  filter(raceId <= 1073) %>% 
  inner_join(driver, by = "raceId")

View(driver)

driver  <- select(driver, -driverId.x)

driver <- driver %>% 
  rename(driverId = driverId.y)

#install.packages("writexl")
library(writexl)
write_xlsx(driver, "f1.xlsx")

f1_18_20 <- driver %>% filter(year >= 2018, year < 2021)
View(f1_18_20)

write_xlsx(f1_18_20, "f1_18_20.xlsx")

write_xlsx(constructor, "f1_constructor.xlsx")
