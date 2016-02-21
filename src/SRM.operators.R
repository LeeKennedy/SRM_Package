data.in <- read.csv("fatsirm001a.csv", as.is=TRUE, header=TRUE)

remove.outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

f3 <- split(data.in$ENTRY, data.in$ASSIGNED_OPERATOR)
boxplot(f3)
f4 <- lapply(f3, remove.outliers)
boxplot(f4)
f5 <- lapply(f4a, remove.outliers)
boxplot(f5, las=2)
abline(h=27.44063)

f6 <- unsplit(f5, data.in$ASSIGNED_OPERATOR)
f7 <- cbind(data.in,f6)
f7a <- na.omit(f7)

b1 <- tapply(f7a$ENTRY, f7a$ASSIGNED_OPERATOR, length)
b2 <- tapply(f7a$ENTRY, f7a$ASSIGNED_OPERATOR, mean)
b3c <- tapply(f7a$ENTRY, f7a$ASSIGNED_OPERATOR, sd)
b4 <- cbind(b1, b2, b3c)
b4 <- as.data.frame(b4)

b4



write.csv(b4, file = "Operators.csv", row.names = TRUE)
