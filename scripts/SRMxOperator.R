#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(here)

#### Functions -----------------------------

shaptest <- function (x) {
        y = shapiro.test(x)
        return(y$p.value)
}

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
data1 <- read_excel("C:/Users/leekennedy/Desktop/In Progress/FATS01.xlsx", 
                    col_types = c("numeric", "date", "text", 
                                  "text", "text", "text", "text", "numeric", 
                                  "text", "text"))

###  Tidying the data affter first screen through the plot ---------------
# data1 <- data1 %>% filter(ENTRY <100) %>%  filter(SAMPLING_POINT == "SRM21O")


### Reducing to the LW control chart limits of 50 data points -------------
# data1 <- tail(data1, 50)

data.in <- data1


#Tidying up column names.
colnames(data1)[1] <- 'Sample'
colnames(data1)[8] <- 'Result'
colnames(data1)[4] <- 'Operator'

testname <- substr(data1$ANALYSIS[1],1,6)
analyte <- data1$REPORTED_NAME[1]
test_units <- tolower(sub("_P_","/",(data1$UNITS[1])))

# checking that only one srm present.
srm_no <- unique(data1$SAMPLING_POINT)
if (length(srm_no) >1){print("More than one SRM present")}

mid_line <- mean(data1$Result)

####### Plot by Operators #######
p <- ggplot(data1, aes(x = LOGIN_DATE,y = Result, fill = Operator)) + 
        geom_point(size=4, alpha = 1, shape=21, colour="black") +
        labs(title = testname, y=test_units, x="Sample Number")+
        geom_hline(yintercept = mid_line, lty=2) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14), axis.text.x = element_text(angle = 0, hjust = 1))
p

#ggsave(p, device = NULL, file = paste("~/Documents/GitHub/SRM_Package/graphs/", testname,"_Operators_", Sys.Date(),".png", sep=""))

dev.off()
p + facet_wrap(~ Operator, ncol=2) # individual panels

# ggsave("graphs/Operators_CHLN03.png", width=12, height=8, dpi=100)


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
          col = "cornflowerblue", xlab = test_units, main = testname) 
xfit <- seq(min(df5$ENTRY), max(df5$ENTRY), length = 40) 
yfit <- dnorm(xfit, mean = mean(df5$ENTRY), sd = sd(df5$ENTRY)) 
yfit <- yfit * diff(h$mids[1:2]) * length(df5$ENTRY) 

lines(xfit, yfit, col = "black", lwd = 2)

### --------------------------------

