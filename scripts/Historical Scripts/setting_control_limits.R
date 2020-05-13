#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(here)


#### Functions -----------------------------


#### Data Input -----------------------------
here::here()

data <- read_excel("data/MICP01_IRM001B.xlsx", 
                   col_types = c("numeric", "date", "text", 
                                 "text", "text", "text", "text", "numeric", 
                                 "text", "text"))

#### Data Cleaning -----------------------------

control_limits <- data %>% 
        group_by(SAMPLING_POINT, REPORTED_NAME, UNITS) %>% 
        mutate(ENTRY = outliers(ENTRY)) %>% 
        na.omit() %>% 
        summarise(n=n(), Mean = mean(ENTRY), LCL = Mean - 3*sd(ENTRY), LWL = Mean - 2*sd(ENTRY), UWL = Mean + 2*sd(ENTRY), UCL = Mean + 3*sd(ENTRY))
control_limits

write.csv(control_limits, "MICP01_IRM001_Limits.csv")

#### Visualising Data -----------------------------

x <- shapiro.test(control_limits$Mean)
str(x)
