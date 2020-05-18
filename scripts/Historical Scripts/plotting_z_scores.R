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

data1 <- data %>%
        filter(SAMPLING_POINT == "SRM45G_CL")

z_scores <- data1 %>% 
        filter(REPORTED_NAME %in% c("Lead", "Cadmium", "Arsenic", "Chromium", "Tin")) %>% 
        group_by(REPORTED_NAME) %>% 
        mutate(ENTRY = outliers(ENTRY)) %>% 
        na.omit() %>% 
        mutate(z_score = (ENTRY - mean(ENTRY))/sd(ENTRY))

testz <- as.data.frame(z_scores[, c(1,6,11)])

testz_wide <- pivot_wider(testz, names_from = "REPORTED_NAME", values_from = "z_score")
testz_wide <- testz_wide[,-1]
plot(testz_wide)

#### Visualising Data -----------------------------

z_plot <- ggplot(z_scores, aes(x = LOGIN_DATE, y = z_score, fill = REPORTED_NAME))+
        geom_point(size = 3, shape = 21)+
        geom_line()+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1)) 

z_plot
