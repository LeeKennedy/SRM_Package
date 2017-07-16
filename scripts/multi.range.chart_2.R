library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)

# Functions -----------------------------------------------------------------------------
remove.outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}

# SRM Data ------------------------------------------------------------------------------
data <- read_csv("VB12_SRM.csv")
colnames(data)[1] <- "Sample"
data$LOGIN_DATE <- dmy_hm(data$LOGIN_DATE)

data2 <- data %>%
        arrange(Sample) %>%
        mutate(ENTRY = remove.outliers(ENTRY)) %>%
        na.omit()

data2$Row <- as.numeric(rownames(data2))

data3 <- data %>%
        summarise(Mean = mean(ENTRY), 
                              UWL = Mean+2*sd(ENTRY),
                              UCL = Mean+3*sd(ENTRY),
                              LWL = Mean-2*sd(ENTRY),
                              LCL = Mean-3*sd(ENTRY),
                                SD = sd(ENTRY))
data3

n <- as.numeric(nrow(data2))

data2 <- data2[, c(9,3,8)]

# Validation Data ----------------------------------------------------------------------

data_x <- read.csv("B12_Validation.csv", as.is = TRUE, header = TRUE)
data_x$LOGIN_DATE <- dmy(data_x$LOGIN_DATE)

data4 <- data_x %>%
        arrange(LOGIN_DATE) 

data4a <- transform(data4,id=as.numeric(factor(LOGIN_DATE)))

data5 <- data4a %>%
        mutate(Row = n + id)

data5 <- data5[, c(9,3,7)]

val_sd <- 0.25

data6 <- data5 %>%
        summarise(Mean = mean(ENTRY))
data6

# Combine the two data sets -------------------------------------------------------------

data2 <- rbind(data2, data5)

m <- as.numeric(nrow(data2))

# Plotting the graph --------------------------------------------------------------------
plot <- ggplot(data2, aes(x=Row, y=ENTRY, fill = ANALYSIS)) +
        geom_point(size=4, shape = 21, colour = "black") +
        
        geom_segment(aes(x = data2$Row[1],
                         xend = data2$Row[n], 
                         y = data3$Mean[1], 
                         yend = data3$Mean[1]), 
                        colour = "red", 
                        lty=2, lwd=1) +
        
        geom_segment(aes(x = data2$Row[1],
                         xend = data2$Row[n], 
                         y = data3$UCL[1], 
                         yend = data3$UCL[1]), 
                        colour = "grey50", 
                        lty=1, lwd=1) +
        
        geom_segment(aes(x = data2$Row[1],
                         xend = data2$Row[n],  
                         y = data3$LCL[1], 
                         yend = data3$LCL[1]), 
                        colour = "grey50", 
                        lty=1, lwd=1) +
        
        geom_vline(xintercept=n, lty=2) +

        geom_segment(aes(x = data2$Row[n+1],
                         xend = data2$Row[m],  
                         y = data6$Mean[1], 
                         yend = data6$Mean[1]), 
                     colour = "red", 
                     lty=2, lwd=1) +
        
        geom_segment(aes(x = data2$Row[n+1],
                         xend = data2$Row[m],  
                         y = data6$Mean[1]+3*val_sd, 
                         yend = data6$Mean[1]+3*val_sd), 
                     colour = "grey50", 
                     lty=1, lwd=1) +
        
        geom_segment(aes(x = data2$Row[n+1],
                         xend = data2$Row[m],  
                         y = data6$Mean[1]-3*val_sd, 
                         yend = data6$Mean[1]-3*val_sd), 
                     colour = "grey50", 
                     lty=1, lwd=1) +
        
        scale_fill_brewer(palette="Set1") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14)) +
        scale_y_continuous(limits = c(1.5,7)) +
        labs(x="Result", y = "ug/100g", title = "IRM001A - Vitamin B12 tested by Vitafast and UPLC\n", fill = "Method")

plot
