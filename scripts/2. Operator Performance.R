#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(LK.Toolbox)
library(here)

#### Functions -----------------------------

shaptest <- function (x) {
        y = shapiro.test(x)
        return(y$p.value)
}


#### Data Input -----------------------------
here::here()
irm_ops <- read_excel("data/VITA12_SRM.xlsx", 
                    col_types = c("numeric", "date", "text", 
                                  "text", "text", "text", "text", "numeric", 
                                  "text", "text"))


#### Data Cleaning -----------------------------

### Filter by start date

irm_ops <- irm_ops %>% 
        filter(LOGIN_DATE > "2020-01-01")


### Remove extreme outliers (Z Score > 5)

irm_ops <- irm_ops %>% 
        mutate(z_score = (ENTRY - mean(ENTRY))/sd(ENTRY)) %>% 
        filter(z_score < 5)

data.in <- irm_ops


#Tidying up column names.
colnames(irm_ops)[1] <- 'Sample'
colnames(irm_ops)[8] <- 'Result'
colnames(irm_ops)[4] <- 'Operator'

testname <- substr(irm_ops$ANALYSIS[1],1,6)
analyte <- irm_ops$REPORTED_NAME[1]
test_units <- tolower(sub("_P_","/",(irm_ops$UNITS[1])))

# checking that only one srm present.
srm_no <- unique(irm_ops$SAMPLING_POINT)
if (length(srm_no) >1){print("More than one SRM present")}

mid_line <- mean(irm_ops$Result)

####### Plot by Operators #######
p <- ggplot(irm_ops, aes(x = LOGIN_DATE,y = Result, fill = Operator)) + 
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

