#### Clean Up environment -------------------------------------
rm(list=ls())

#### Packages -------------------------------------------------
library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(lubridate)
library(here)


#### Functions ------------------------------------------------


#### Data Input -----------------------------------------------
here::here()



if("Darwin" %in% Sys.info()['sysname'] == TRUE){ 
        data <- read_excel("~/Documents/GitHub/zData_Files/SRM_Package_Data/Inputs/Vitafast.xlsx", 
                           col_types = c("numeric", "date", "text", 
                                         "text", "text", "text", "text", "numeric", 
                                         "text", "text")) 
} else { 
        data <- read_excel("data/ICPM01_IRM001B.xlsx", 
                           col_types = c("numeric", "date", "text", 
                                         "text", "text", "text", "text", "numeric", 
                                         "text", "text")) 
}



#### Data Cleaning --------------------------------------------
#### Collecting ID data ---------------------------------------
data1 <- data %>%
        #filter(SAMPLING_POINT == "IRM001B_CL") %>% 
        filter(LOGIN_DATE > "2020-01-01")

#### Keeping a copy of the raw data ---------------------------
data1$RAW_ENTRY <- data1$ENTRY


#### Creating z Scores on raw data based on cleaned data ------

z_scores <- data1 %>% 
        group_by(ANALYSIS, REPORTED_NAME) %>% 
        mutate(ENTRY = outliers(ENTRY)) %>% 
        #na.omit() %>% 
        mutate(z_score = (RAW_ENTRY - mean(ENTRY, na.rm = TRUE))/sd(ENTRY, na.rm = TRUE)) 


#### Assigning ranges to z scores -----------------------------

z_scores <- z_scores %>% mutate(CC_group = case_when(z_score > 3  ~ '>3',
                                                    z_score > 2  & z_score <= 3 ~ '2 to 3',
                                                    z_score > -2  & z_score <= 2 ~ '-2 to 2',
                                                    z_score > -3  & z_score <= -2 ~ '-2 to -3',
                                                    z_score  < 3 ~ '<3'))

#### Converting ranges to factors------------------------------
z_scores$CC_group <- factor(z_scores$CC_group, levels = c('>3', '2 to 3', '-2 to 2', '-2 to -3', '<3'))

#### Visualising Data -----------------------------------------

multi_plot <- ggplot(z_scores, aes(x = LOGIN_DATE, y = REPORTED_NAME, group = 1)) +
        geom_point(aes(shape = CC_group, fill = CC_group), size = 4) +
        scale_shape_manual(values = c(24, 24, 21, 25, 25)) +
        scale_fill_manual(values = c("red", "cornflowerblue", "white", "cornflowerblue", "red")) +
         theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1)) 

multi_plot

# ggsave("Multiple_Control_Chart_MICP01.png", width = 14, height = 6, dpi = 100)
