#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(dts.quality)

#### Functions -----------------------------

shaptest <- function (x) {
        y = shapiro.test(x)
        return(y$p.value)
}

#### Data Input -----------------------------

data1 <- read_excel("~/Desktop/FATS01_2017/FATS01_IRM001B.xlsx")

data.in <- data1
#Tidying up column names.
colnames(data1)[1] <- 'Sample'
colnames(data1)[8] <- 'Result'
colnames(data1)[4] <- 'Operator'

testname <- substr(data1$ANALYSIS[1],1,6)

# checking that only one srm present.
srm_no <- unique(data1$SAMPLING_POINT)
if (length(srm_no) >1){print("More than one SRM present")}

mid_line <- mean(data1$Result)

####### Plot by Operators #######
p <- ggplot(data1, aes(x = Sample,y = Result, fill = Operator)) + 
        geom_point(size=5, alpha = 1, shape=21, colour="black") +
        geom_hline(yintercept = mid_line, lty=2) +
        theme_bw()
p

ggsave(p, device = NULL, file = paste("~/Documents/GitHub/SRM_Package/graphs/", testname,"_Operators_", Sys.Date(),".png", sep=""))

dev.off()
p + facet_wrap(~ Operator, ncol=2) # individual panels



### ------------------------------------------------

df4 <- data.in %>% 
        group_by(ASSIGNED_OPERATOR) %>% 
        mutate(outliers(ENTRY)) %>% 
        na.omit() %>% 
        filter(n() > 2) %>% 
        summarise(n=n(), Mean= round(mean(ENTRY),2), SD = round(sd(ENTRY),2), p.Value = round(shaptest(ENTRY),3))
df4

df5 <- data.in %>% 
        group_by(ASSIGNED_OPERATOR) %>% 
        mutate(outliers(ENTRY)) %>% 
        na.omit() %>% 
        filter(n() > 2) 



### Histogram of all data ----------------------------------------------------------------

h <- hist(df5$ENTRY, breaks = 50, density = 50,
          col = "cornflowerblue", xlab = "% Fat", main = "FATS01 IRM001B Results") 
xfit <- seq(min(df5$ENTRY), max(df5$ENTRY), length = 40) 
yfit <- dnorm(xfit, mean = mean(df5$ENTRY), sd = sd(df5$ENTRY)) 
yfit <- yfit * diff(h$mids[1:2]) * length(df5$ENTRY) 

lines(xfit, yfit, col = "black", lwd = 2)

### --------------------------------

df6 <- data.in %>% 
        filter(ASSIGNED_OPERATOR == "BCHAKMA") %>% 
        mutate(outliers(ENTRY)) %>% 
        na.omit() 


h <- hist(df6$ENTRY, breaks = 50, density = 50,
          col = "cornflowerblue", xlab = "% Fat", main = "BCHAKMA FATS01 IRM001B Results") 
xfit <- seq(min(df6$ENTRY), max(df6$ENTRY), length = 40) 
yfit <- dnorm(xfit, mean = mean(df6$ENTRY), sd = sd(df6$ENTRY)) 
yfit <- yfit * diff(h$mids[1:2]) * length(df6$ENTRY) 

lines(xfit, yfit, col = "black", lwd = 2)

