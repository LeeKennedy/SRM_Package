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
        data <- read_excel("~/Documents/GitHub/zData_Files/SRM_Package_Data/Inputs/MICP01_IRM001B.xlsx", 
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
        filter(SAMPLING_POINT == "IRM001B_CL") %>% 
        filter(LOGIN_DATE > "2020-04-01")

z_scores <- data1 %>% 
        group_by(REPORTED_NAME) %>% 
        #mutate(ENTRY = outliers(ENTRY)) %>% 
        na.omit() %>% 
        mutate(z_score = (ENTRY - mean(ENTRY))/sd(ENTRY))

#### Visualising Data -----------------------------


z_scores <- z_scores %>% mutate(z_group = case_when(z_score > 3  ~ '>3',
                                                    z_score > 2  & z_score <= 3 ~ '2 to 3',
                                                    z_score > -2  & z_score <= 2 ~ '-2 to 2',
                                                    z_score > -3  & z_score <= -2 ~ '-2 to -3',
                                                    z_score  < 3 ~ '<3'))

z_scores$z_group <- factor(z_scores$z_group, levels = c('>3', '2 to 3', '-2 to 2', '-2 to -3', '<3'))

multi_plot <- ggplot(z_scores, aes(x = NAME, y = REPORTED_NAME, group = 1)) +
        geom_point(aes(shape = z_group, size = z_group, fill = z_group)) +
        scale_fill_manual(values = c("red", "coral", "green3", "coral", "red")) +
        scale_shape_manual(values = c(24, 24, 21, 25, 25)) +
        scale_size_manual(values = c(4,3,1,3,4)) +
        labs(title = "MICP01_IRM001B", subtitle = "Since April 1st", y = "", x="")+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1)) 

multi_plot

ggsave("Multiple_Control_Chart.png", width = 14, height = 4, dpi = 100)
