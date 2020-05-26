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
        data <- read_excel("~/Documents/GitHub/zData_Files/SRM_Package_Data/Inputs/BIOA01.xlsx", 
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
        filter(LOGIN_DATE > "2019-01-01")

#### Keeping a copy of the raw data ---------------------------

control_date <- "2020-06-01"
CD_line <- ymd(control_date)


data1$RAW_ENTRY <- data1$ENTRY
data1$control <- data1$ENTRY

data1$control[(data1$LOGIN_DATE > control_date) == TRUE] <- NA


#### Creating z Scores on raw data based on cleaned data ------

z_scores <- data1 %>% 
        group_by(ANALYSIS, REPORTED_NAME) %>% 
        mutate(ENTRY = outliers(ENTRY)) %>% 
        #na.omit() %>% 
        mutate(z_score = (RAW_ENTRY - mean(control, na.rm = TRUE))/sd(control, na.rm = TRUE)) 



#### Assigning ranges to z scores -----------------------------

z_scores <- z_scores %>% mutate(CC_group = case_when(z_score > 3  ~ '>3',
                                                    z_score > 2  & z_score <= 3 ~ '2 to 3',
                                                    z_score > 0  & z_score <= 2 ~ '0 to 2',
                                                    z_score >-2 & z_score <= 0 ~ '0 to -2',
                                                    z_score > -3  & z_score <= -2 ~ '-2 to -3',
                                                    z_score  < 3 ~ '<3'))

#### Converting ranges to factors------------------------------
z_scores$CC_group <- factor(z_scores$CC_group, levels = c('>3', '2 to 3', '0 to 2', '0 to -2', '-2 to -3', '<3'))

#### Visualising Data -----------------------------------------
#### Make y = ANALYSIS or REPORTED_NAME as needs be.

multi_plot <- ggplot(z_scores, aes(x = NAME, y = REPORTED_NAME, group = 1)) +
        geom_point(aes(shape = CC_group, fill = CC_group), size = 4) +
        scale_shape_manual(values = c(24, 24, 21, 21, 25, 25)) +
        scale_fill_manual(values = c("red", "royalblue1", "cornflowerblue", "darkgoldenrod1", "forestgreen", "red")) +
         theme_bw() +
        labs(title = "PEST07_PCBs", subtitle = paste("Control Lines based on data pre ", control_date,sep=""), y = "", x = "") +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1)) 

multi_plot

# ggsave("Multiple_Control_Chart_Vitafast.png", width = 14, height = 6, dpi = 100)
