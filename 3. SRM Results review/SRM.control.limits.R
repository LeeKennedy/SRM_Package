# Rename incoming data file:
data.raw <- read.csv("SRM_data.csv", as.is=TRUE, header=TRUE)
colnames(data.raw)[1] <- "SAMPLE_NUMBER"
library(dplyr)
library(psych)
library(ggplot2)

testname <- substr(data.raw$ANALYSIS[1],1,6)

Name <- data.raw$REPORTED_NAME[1]
Units <- tolower(sub("_P_","/",(data.raw$REPORTED_UNITS[1])))

# Clean the data
data.in <- select(data.raw, everything())%>%
        arrange(SAMPLE_NUMBER)%>%
        select(ASSIGNED_OPERATOR, SAMPLING_POINT, ENTRY)%>%
        mutate(ENTRY = as.numeric(as.character(ENTRY)))%>%
        na.omit


boxplot(data.in$ENTRY~data.in$SAMPLING_POINT,
        main = paste("Comparative ",testname," SRM/IRM Results"),
        ylab = paste(Name," ", Units))

j <- length(unique(data.in$SAMPLING_POINT))

remove.outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}
srm1 <- unique(data.in[2])

srm <- srm1[j,]

data.in3 <- data.in[data.in$SAMPLING_POINT==srm,]
names(data.in3) <- c("Operator", "SRM","A")
data.in3 <- select(data.in3, everything())%>%
        filter( between(row_number(), n()-300, n()))

clean <- remove.outliers(data.in3$A)
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

plot(data.in3$A, 
        type="p", 
        pch= 19, 
        cex=1.0, 
        col ="red", 
        ylim = c(LCL*0.9,UCL*1.1),
        ylab = paste(Name," ", Units),
        xlab="")

par(new=T)

plot(clean, 
        type="o", 
        pch=19, 
        cex=1.0, 
        col="grey40", 
        ylim = c(LCL*0.9,UCL*1.1), 
        ylab = paste(Name," ", Units), 
        main = paste(srm, "(", Name,") Control Chart"),
        xlab="")

abline(h=Centre, col = "blue", lty=2, lwd=2)
abline(h=UCL, col = "red", lty=2, lwd=2)
abline(h=UWL, col = "darkgreen", lty=3, lwd=2)
abline(h=LWL, col = "darkgreen", lty=3, lwd=2)
abline(h=LCL, col = "red", lty=2, lwd=2)

hist(clean, breaks=20, xlab=srm,
     main = paste("Control Chart",srm,"data distribution"))


print(describe(clean))
print (Clines)

boxplot(data.in3$A~data.in3$Operator, las=2)

xx <- boxplot(data.in3$A~data.in3$Operator, las=2)
mytable <- xx$stats
colnames(mytable)<-xx$names
rownames(mytable)<-c('min','lower quartile','median','upper quartile','max')
mytable 

table(data.in3$Operator)

