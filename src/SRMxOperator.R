library("ProjectTemplate")
load.project()

data1 <- read.csv("data/VIB203.csv", as.is=TRUE, header = TRUE)
#Tidying up column names.
colnames(data1)[1] <- 'Sample'
colnames(data1)[8] <- 'Result'
colnames(data1)[4] <- 'Operator'

# checking that only one srm present.
srm_no <- unique(data1$SAMPLING_POINT)
if (length(srm_no) >1){print("More than one SRM present")}

mid_line <- mean(data1$Result)

####### Plot by Operators #######
p <- ggplot(data1, aes(x = Sample,y = Result, color = Operator)) + 
        geom_point(size=5, alpha = 1) +
        geom_hline(yintercept = mid_line, lty=2) +
        theme_bw()
p

p + facet_wrap(~ Operator, ncol=2) # individual panels

data.in <- read.csv("data/VIB203.csv", as.is=TRUE, header=TRUE)


f3 <- split(data.in$ENTRY, data.in$ASSIGNED_OPERATOR)
f4 <- lapply(f3, outliers)
f5 <- lapply(f4, outliers)
boxplot(f5, las=2)
hint <- median(data.in$ENTRY)
abline(h = hint)

f6 <- unsplit(f5, data.in$ASSIGNED_OPERATOR)
f7 <- cbind(data.in,f6)
f7a <- na.omit(f7)

b1 <- tapply(f7a$ENTRY, f7a$ASSIGNED_OPERATOR, length)
b2 <- tapply(f7a$ENTRY, f7a$ASSIGNED_OPERATOR, mean)
b3c <- tapply(f7a$ENTRY, f7a$ASSIGNED_OPERATOR, sd)
b4 <- cbind(b1, b2, b3c)
b4 <- as.data.frame(b4)

b4