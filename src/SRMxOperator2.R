data.in <- read.csv("data/srm_data.csv", as.is=TRUE, header=TRUE)


f3 <- split(data.in$ENTRY, data.in$ASSIGNED_OPERATOR)
boxplot(f3)
f4 <- lapply(f3, outliers)
boxplot(f4)
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




