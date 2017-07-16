library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)

remove.outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}

data <- read_csv("VITD_SRM.csv")
colnames(data)[1] <- "Sample"
data$LOGIN_DATE <- dmy_hm(data$LOGIN_DATE)

data2 <- data %>%
        group_by(SAMPLING_POINT)%>%
        mutate(ENTRY = remove.outliers(ENTRY))%>%
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

plot <- ggplot(data, aes(x=LOGIN_DATE, y=ENTRY, colour = SAMPLING_POINT)) +
        geom_point(size=4, alpha=1.0) +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[1], y = data2$Mean[1], yend = data2$Mean[1]), colour = "red", lty=2, lwd=1) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[2], y = data2$Mean[2], yend = data2$Mean[2]), colour = "red", lty=2, lwd=1) +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[1], y = data2$UCL[1], yend = data2$UCL[1]), colour = "grey50", lty=5, lwd=1) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[2], y = data2$UCL[2], yend = data2$UCL[2]), colour = "grey50", lty=5, lwd=1) +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[1], y = data2$LCL[1], yend = data2$LCL[1]), colour = "grey50", lty=5, lwd=1) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[2], y = data2$LCL[2], yend = data2$LCL[2]), colour = "grey50", lty=5, lwd=1) +
        scale_color_brewer(palette="Set1") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))

        
plot