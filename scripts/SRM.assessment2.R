#### Clean Up environment -----------------------------
rm(list=ls())

### ---------------------------------------------------
library(tidyverse)
library(readxl)
library(psych)
library(here)


## Functions -------------------
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

### Data Input -------------------------------------------------------------------
here()
data.raw <- read_excel("data/PHOS01.xlsx", col_types = c("numeric", 
                                                       "date", "text", "text", "text", "text", 
                                                       "text", "numeric", "text", "text"))


# Input parameters ----------------------------------------------------------------

max.pts <- 50 # Maximum points plotted
points <- 50   # How many points used to set control lines

# ---------------------------------------------------------------------------------

testname <- substr(data.raw$ANALYSIS[1],1,6)

Name <- data.raw$REPORTED_NAME[1]
Units <- tolower(sub("_P_","/",(data.raw$UNITS[1])))

# Clean the data
data.in <- select(data.raw, everything())%>%
        arrange(SAMPLE_NUMBER)%>%
        filter(SAMPLE_NUMBER > 4000000)%>%
        select(ASSIGNED_OPERATOR, SAMPLING_POINT, ENTRY)%>%
        mutate(ENTRY = as.numeric(as.character(ENTRY)))%>%
        na.omit


boxplot(data.in$ENTRY~data.in$SAMPLING_POINT,
        main = paste("Comparative ",testname," SRM/IRM Results"),
        ylab = paste(Name," ", Units))

j <- length(unique(data.in$SAMPLING_POINT))


srm1 <- unique(data.in[2])

srm <- srm1[j,]

data.in3 <- data.in[data.in$SAMPLING_POINT==srm,]
names(data.in3) <- c("Operator", "SRM","A")
data.in3 <- select(data.in3, everything())%>%
        filter( between(row_number(), n()-max.pts, n()))

clean <- outliers(data.in3$A[1:points])
xx <- describe(clean)

UCL <- xx$mean + 3*xx$sd
UWL <- xx$mean + 2*xx$sd
Centre <- xx$mean 
LWL <- xx$mean - 2*xx$sd
LCL <- xx$mean - 3*xx$sd
MU <- 2*xx$sd

CC <- c(UCL, UWL, Centre, LWL, LCL ,MU)
Labels <- c("UCL", "UWL", "Centre", "LWL", "LCL", "MU +/-")
Clines <- cbind(Labels, round(CC,3))
Clines <- as.data.frame(Clines)

# Calculate outliers -----------------------------------------------------------
data.in3$outliers = NA
j <- nrow(data.in3)

for (i in 1:j) {
        if(between(data.in3$A[i], LCL, UCL)==TRUE)
           data.in3$outliers[i]=data.in3$A[i]
}

# Calculate trend on one side of mean ------------------------------------------
data.in3$T1 <- 1
data.in3$T2 <- 1

for (i in 2:j) {
        if(data.in3$A[i] < Centre)
                data.in3$T1[i] = -1
}
n=1
for (i in 2:j) {
        if(data.in3$T1[i] == data.in3$T1[i - 1]) {
                data.in3$T2[i] = n +1
                n = n + 1
        } else {  
                data.in3$T2[i] = 1
                n = 1
        }
}

data.in3$T3 <- NA
for (i in 1:j) {
        if(data.in3$T2[i] > 6)
                data.in3$T3[i] = data.in3$A[i]
}

# Trending up or down -----------------------------------------------------

data.in3$T4 <- 1
data.in3$T5 <- 1

for (i in 2:j) {
        if(data.in3$A[i] < data.in3$A[i-1])
                data.in3$T4[i] = -1
}
n=1
for (i in 2:j) {
        if(data.in3$T4[i] == data.in3$T4[i - 1]) {
                data.in3$T5[i] = n +1
                n = n + 1
        } else {  
                data.in3$T5[i] = 1
                n = 1
        }
}

data.in3$T6 <- NA
for (i in 1:j) {
        if(data.in3$T5[i] > 6)
                data.in3$T6[i] = data.in3$A[i]
}


# Plotting ----------------------------------------------------------------

data_new <- data.in3
data_new$T6 <- as.numeric(data_new$T6)
data_new$T3 <- as.numeric(data_new$T3)
data_new$row_n <- as.numeric(rownames(data_new))



plot_new <- ggplot(data_new, aes(x=row_n, y=A)) +
        geom_line(aes(y=outliers), size=0.5, colour = "gray50") +
        geom_point(size=4, shape = 21, fill ="red", colour = "black") +
        geom_point(aes(y=outliers), size = 4, shape = 21, fill ="gray50", colour = "black") +
        geom_point(aes(y=T3), size = 4, shape = 21, fill ="green", colour = "black") +
        geom_point(aes(y=T6), size = 4, shape = 21, fill ="orange", colour = "black") +
        geom_hline(yintercept = Centre, lty = 2,lwd=1, colour = "black") +
        geom_hline(yintercept = UWL, lty = 2, lwd=1,colour = "blue") +
        geom_hline(yintercept = LWL, lty = 2, lwd=1,colour = "blue") +
        geom_hline(yintercept = UCL, lty = 2,lwd=1, colour = "red") +
        geom_hline(yintercept = LCL, lty = 2, lwd=1,colour = "red") +
        geom_vline(xintercept = points, lty = 1, lwd=0.5,colour = "gray50") +
        theme_bw() +
        labs(x="", y=Name,  title= paste(testname," Control Chart: ", srm,"\n")) +
        theme(plot.title = element_text(size=16)) +
        scale_y_continuous(limits = c(0.97*LCL, 1.03*UCL)) +
        annotate("text", x = points+2, y = 1.02*UCL, label = "Control Limits Data") 
plot_new
ggsave(file.path('graphs', paste(testname,'_', srm,'_Control_Chart.png')), width = 8, height = 5, dpi=200)

# Histogram of control chart ---------------------------------------------
hist(data.in3$outliers, 
     breaks=20, 
     xlab=srm,
     main = paste("Control Chart",srm,"data distribution"))
abline(v = Centre, col = "red", lty = 2)

print(describe(data.in3$outliers))
print (Clines)

xx <- boxplot(data.in3$A~data.in3$Operator, las=2, cex.axis = 0.8)
abline(h=Centre, col = "blue", lty=2, lwd=2)
mytable <- xx$stats
colnames(mytable)<-xx$names
rownames(mytable)<-c('min','lower quartile','median','upper quartile','max')
mytable 

table(data.in3$Operator)



