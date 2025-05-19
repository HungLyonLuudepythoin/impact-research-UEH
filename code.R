setwd("D:/UEH/impact research/code R")
library(MatchIt)
library(readxl)
library(nortest)

data <- read_xls("data_2.xls", sheet = "Q3")
modelling <- matchit(insurance ~ income + famsize + genderhead + agehead +
                        married + network + internet + educhead + exptot_health + # nolint # nolint: indentation_linter.
                        sickness + urban + job + house_size + water + saving + distance + free_day,  # nolint
                        data = data, method = "nearest", distance = "glm", ratio=1) # nolint
summary(modelling)
m.data <- match.data(modelling, distance = "distance_data")
View(m.data)

# y nghia la bac bo, phan phoi chuan
# H0 : gia thuyet khong mong muon, neu p-value y nghia thi bac bo H0
sf.test(m.data$exptot_health[m.data$insurance ==1])
sf.test(m.data$exptot_health[m.data$insurance ==0])

wilcox.test(m.data$exptot_health ~ insurance,data= m.data)
 
mean(m.data$exptot_health[m.data$insurance ==1])
mean(m.data$exptot_health[m.data$insurance ==1])

