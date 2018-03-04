#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(here)



#### Functions -----------------------------
outliers <- function (x, b = FALSE) {
xx <- sapply(x, as.numeric)

#xx <- sort(xx)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

yy <- remove_outliers(xx)
ww <- remove_outliers(yy)
zz <- remove_outliers(ww)

diff.out <- data.frame(xx, yy, ww, zz)

if(b == TRUE){
boxplot(diff.out)
}

return(zz)
}



#### Data Input -----------------------------
setwd(here())
srm_raw <- read_csv("data/srmdata.csv")
testname <- substr(srm_raw$ANALYSIS[1],1,6)
Units <- tolower(sub("_P_","/",(srm_raw$UNITS[1])))

#### Data Cleaning -----------------------------

srm_data <- srm_raw[, c(1,2,6,7:9,13)]

srm_data <- srm_data %>% 
        separate(TEXT_ID, into = paste("V", 1:3, sep = "_"))
srm_data <- srm_data[,c(8,1:6)]
colnames(srm_data)[1] <- 'SRM'

srms <- unique(srm_data$SRM)
n <- length(srms)

#### Visualising Data -----------------------------

srm_data <- srm_data %>% 
        filter(SRM == srms[1])  # Choose SRM to plot

srm_n <- nrow(srm_data)

if (srm_n>200) {
        srm_data = srm_data[(srm_n-200):srm_n,]
        
}

srm_data <- srm_data %>% 
        mutate(ENTRY = outliers(ENTRY)) %>% 
        na.omit()

Centre <- mean(srm_data$ENTRY)
LCL <- Centre - 3*sd(srm_data$ENTRY)
LWL <- Centre - 2*sd(srm_data$ENTRY)
UWL <- Centre + 2*sd(srm_data$ENTRY)
UCL <- Centre + 3*sd(srm_data$ENTRY)

srm_plot <- ggplot(srm_data, aes(x=LOGIN_DATE, y=ENTRY)) +
        geom_point(size=4,shape=21, col="black", fill = "cornflowerblue") +
        geom_hline(yintercept = Centre, lty=2, col="black" ) +
        geom_hline(yintercept = UCL, lty=2, col="red" ) +
        geom_hline(yintercept = LCL, lty=2, col="red" ) +
        geom_hline(yintercept = UWL, lty=2, col="darkgreen" ) +
        geom_hline(yintercept = LWL, lty=2, col="darkgreen" ) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
srm_plot
