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

irm_data <- read_excel("data/VITA12_SRM.xlsx", 
                   col_types = c("numeric", "date", "text", 
                                 "text", "text", "text", "text", "numeric", 
                                 "text", "text"))

#### Data Cleaning -----------------------------

### Filter by start date

irm_data <- irm_data %>% 
        filter(LOGIN_DATE > "2020-01-01")


### Remove extreme outliers (Z Score > 5)

irm_data <- irm_data %>% 
        mutate(z_score = (ENTRY - mean(ENTRY))/sd(ENTRY)) %>% 
        filter(z_score < 5)

### Statistical Characteristics

Mean <- mean(irm_data$ENTRY)
SD <- sd(irm_data$ENTRY)
UCL <- Mean + 3*SD
UWL <- Mean + 2*SD
LWL <- Mean - 2*SD
LCL <- Mean - 3*SD
MU <- 2*SD
pct_MU <- MU*100/Mean

normal_shape_pvalue <- as.numeric(shapiro.test(irm_data$ENTRY)[2])

#### Visualising Data -----------------------------

Test <- paste("Control Chart:", irm_data$ANALYSIS[1],"; plotting ", irm_data$SAMPLING_POINT[1], sep = "")
Units <- irm_data$UNITS[1]

irm_plot <- ggplot(irm_data, aes(x = LOGIN_DATE, y = ENTRY))+
        geom_point(size = 4, shape = 21, fill = "cornflowerblue", col = "black")+
        geom_hline(yintercept = Mean, lty = 2, col = "black")+
        geom_hline(yintercept = UWL, lty = 2, col = "blue")+
        geom_hline(yintercept = LWL, lty = 2, col = "blue")+
        geom_hline(yintercept = UCL, lty = 2, col = "red")+
        geom_hline(yintercept = LCL, lty = 2, col = "red")+
        labs(title = Test, y = Units, x="")+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1))
irm_plot

bw <- 2 * IQR(irm_data$ENTRY) / length(irm_data$ENTRY)^(1/3)

irm_hist <- ggplot(irm_data, aes(x=ENTRY))+
        geom_histogram(binwidth = bw, fill="cornflowerblue", col="black")+
        geom_vline(xintercept = Mean, lty = 2, col = "black")+
        geom_vline(xintercept = UWL, lty = 2, col = "blue")+
        geom_vline(xintercept = LWL, lty = 2, col = "blue")+
        geom_vline(xintercept = UCL, lty = 2, col = "red")+
        geom_vline(xintercept = LCL, lty = 2, col = "red")+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1))
irm_hist