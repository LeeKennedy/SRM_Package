# Rename incoming data file:
data.in <- read.csv("data/SRM_data.csv", as.is=TRUE, header=TRUE)
library(dplyr)
library(psych)

data.in <- select(data.in, everything())%>%
        filter(SAMPLE_NUMBER > 4000000)%>%
        arrange(SAMPLE_NUMBER)%>%
        select(SAMPLING_POINT, ENTRY)%>%
        mutate(ENTRY = as.numeric(as.character(ENTRY)))%>%
        na.omit

#If more than one IRM, choose desired one and change srm value.  Default = 1
boxplot(data.in$ENTRY~data.in$SAMPLING_POINT)

j <- length(unique(data.in$SAMPLING_POINT))

remove.outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}
srm1 <- unique(data.in[1])

for(i in 1:j){

srm <- srm1[i,]

data.in3 <- data.in[data.in$SAMPLING_POINT==srm,]
names(data.in3) <- c("SRM","A")

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

plot(data.in3$A, type="p", pch= 19, cex=0.6, col ="red",ylim = c(LCL*0.8,UCL*1.3), ylab = "", xlab=srm)
par(new=T)
plot(clean, type="o", pch=19, cex=0.75, col="grey40",ylim = c(LCL*0.8,UCL*1.3), ylab = "", xlab=srm)

abline(h=Centre, col = "blue", lty=2, lwd=2)
abline(h=UCL, col = "red", lty=2, lwd=2)
abline(h=UWL, col = "darkgreen", lty=3, lwd=2)
abline(h=LWL, col = "darkgreen", lty=3, lwd=2)
abline(h=LCL, col = "red", lty=2, lwd=2)

hist(clean, breaks=20, xlab=srm)

print(srm)
print(describe(clean))
print (Clines)
}

