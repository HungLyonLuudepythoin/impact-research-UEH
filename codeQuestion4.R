setwd("D:/UEH/impact research/code R")
install.packages("dplyr")
install.packages("knitr")
install.packages("kableExtra")
install.packages("psych")
  library(readxl)
  library(dplyr)
  library(knitr)
  library(kableExtra)
  library(psych)

data <- read_xls("data_2_4.xls", sheet = "Q4")

str(data)


## Thong ke mo ta du lieu 
describe(data, skew=F) -> sum 
sum
summary(data)

## Khong co missing value
colSums(is.na(data))
any(is.na(data))

## Chua cac du lieu ngoai lai
boxplot(data$profit)
boxplot(data$revenue)
quantile(data$profit, 0.99)
uv_profit = 3*quantile(data$profit, 0.99)
data$profit[data$profit > uv_profit] <- uv_profit

quantile(data$revenue, 0.99)
uv_revenue = 3*quantile(data$revenue, 0.99)
data$revenue[data$revenue > uv_revenue] <- uv_revenue


## Loai bo cac du lieu bang 0
summary(data$profit)
summary(data$revenue)

data <- data[data$revenue != 0, ]
data <- data[data$profit != 0, ]

## So sanh tham gia va ko tham gia esg

cols <- c("profit", "firm_size", "age_firm", 
          "gender", "hh_firm", "location", "export")

colMeans(data[data$esg == 0, cols], na.rm = TRUE)
nrow(data[data$esg == 0, ])
nrow(data[data$esg == 1, ])
data %>%
  filter(esg == 0) %>%
  summarise(
    count(),
    mean_profit = mean(profit, na.rm = TRUE),
    sd_profit = sd(profit, na.rm = TRUE),
    mean_firm_size = mean(firm_size, na.rm = TRUE),
    sd_firm_size = sd(firm_size, na.rm = TRUE),
    mean_age_firm = mean(age_firm, na.rm = TRUE),
    sd_age_firm = sd(age_firm, na.rm = TRUE),
    mean_gender = mean(gender, na.rm = TRUE),
    sd_gender = sd(gender, na.rm = TRUE),
    mean_hh_firm = mean(hh_firm, na.rm = TRUE),
    sd_hh_firm = sd(hh_firm, na.rm = TRUE),
    mean_location = mean(location, na.rm = TRUE),
    sd_location = sd(location, na.rm = TRUE),
    mean_export = mean(export, na.rm = TRUE),
    sd_export = sd(export, na.rm = TRUE)
  ) %>%
  print(width = Inf)

data %>%
  filter(esg == 1) %>%
  summarise(
    mean_profit = mean(profit, na.rm = TRUE),
    sd_profit = sd(profit, na.rm = TRUE),
    mean_firm_size = mean(firm_size, na.rm = TRUE),
    sd_firm_size = sd(firm_size, na.rm = TRUE),
    mean_age_firm = mean(age_firm, na.rm = TRUE),
    sd_age_firm = sd(age_firm, na.rm = TRUE),
    mean_gender = mean(gender, na.rm = TRUE),
    sd_gender = sd(gender, na.rm = TRUE),
    mean_hh_firm = mean(hh_firm, na.rm = TRUE),
    sd_hh_firm = sd(hh_firm, na.rm = TRUE),
    mean_location = mean(location, na.rm = TRUE),
    sd_location = sd(location, na.rm = TRUE),
    mean_export = mean(export, na.rm = TRUE),
    sd_export = sd(export, na.rm = TRUE)
  ) %>%
  print(width = Inf)
