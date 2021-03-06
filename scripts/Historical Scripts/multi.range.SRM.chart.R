#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(lubridate)
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
here::here()

data <- read_excel("data/VITD_SRM.xlsx", 
                   col_types = c("numeric", "date", "text", 
                                 "text", "text", "text", "text", "numeric"))

colnames(data)[1] <- "Sample"
#data$LOGIN_DATE <- dmy_hm(data$LOGIN_DATE)

data2 <- data %>%
        group_by(SAMPLING_POINT)%>%
        mutate(ENTRY = outliers(ENTRY))%>%
        na.omit()%>%
        summarise(Mean = mean(ENTRY), 
                              UWL = Mean+2*sd(ENTRY),
                              UCL = Mean+3*sd(ENTRY),
                              LWL = Mean-2*sd(ENTRY),
                              LCL = Mean-3*sd(ENTRY),
                                SD = sd(ENTRY))
data2

n <- unique(data$SAMPLING_POINT)

data3 <- data %>%
        group_by(SAMPLING_POINT)%>%
        arrange(LOGIN_DATE)%>%
        mutate(count=row_number(),
               first_SRM = count==1)%>%
        filter(first_SRM==TRUE)
       
data3

data4 <- data %>%
        group_by(SAMPLING_POINT)%>%
        arrange(desc(LOGIN_DATE))%>%
        mutate(count=row_number(),
               last_SRM = count==1)%>%
        filter(last_SRM==TRUE)

data4

plot <- ggplot(data, aes(x=LOGIN_DATE, y=ENTRY, fill = SAMPLING_POINT)) +
        geom_point(size=4, shape = 21, alpha=0.7) +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[2], y = data2$Mean[1], yend = data2$Mean[1]), colour = "red", lty=2, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[1], y = data2$Mean[2], yend = data2$Mean[2]), colour = "red", lty=2, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[2], y = data2$UCL[1], yend = data2$UCL[1]), colour = "grey50", lty=5, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[1], y = data2$UCL[2], yend = data2$UCL[2]), colour = "grey50", lty=5, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[2], y = data2$LCL[1], yend = data2$LCL[1]), colour = "grey50", lty=5, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[1], y = data2$LCL[2], yend = data2$LCL[2]), colour = "grey50", lty=5, lwd=0.5) +
        scale_color_brewer(palette="Set1") +
        theme_bw() +
       theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
       axis.line = element_line(size = 0.7, color = "black"), 
       text = element_text(size = 14))

        
plot