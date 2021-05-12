library(readxl)
library(tidyverse)

f1 <- read_excel("C:\\Users\\manu-\\Documents\\INSPER\\7_semestre\\R_dados\\APS_R_dados\\f1.xlsx")
f1


f1_ <- f1 %>% filter(!is.na(standing_driver_points))


summarize(f1)
