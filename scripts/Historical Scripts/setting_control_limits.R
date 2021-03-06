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



if("Darwin" %in% Sys.info()['sysname'] == TRUE){ 
        data <- read_excel("~/Documents/GitHub/zData_Files/SRM_Package_Data/Inputs/ICPM01_IRMSRM.xlsx", 
                           col_types = c("numeric", "date", "text", 
                                         "text", "text", "text", "text", "numeric", 
                                         "text", "text")) 
} else { 
        data <- read_excel("data/ICPM01_IRM001B.xlsx", 
                           col_types = c("numeric", "date", "text", 
                                         "text", "text", "text", "text", "numeric", 
                                         "text", "text")) 
}



#### Data Cleaning -----------------------------

control_limits <- data %>% 
        group_by(SAMPLING_POINT, REPORTED_NAME, UNITS) %>% 
        mutate(ENTRY = outliers(ENTRY)) %>% 
        na.omit() %>% 
        summarise(n=n(), Mean = mean(ENTRY), LCL = Mean - 3*sd(ENTRY), LWL = Mean - 2*sd(ENTRY), UWL = Mean + 2*sd(ENTRY), UCL = Mean + 3*sd(ENTRY))
control_limits

write.csv(control_limits, "~/Documents/GitHub/zData_Files/SRM_Package_Data/Outputs/ICPM01_IRMSRM_Limits.csv")

#### Visualising Data -----------------------------

x <- shapiro.test(control_limits$Mean)
str(x)
