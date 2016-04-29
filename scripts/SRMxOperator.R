library("ProjectTemplate")
load.project()

data1 <- read.csv("data/VITB6.csv", as.is=TRUE, header = TRUE)
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
colnames(b4) <- c("n", "Mean", "sd")

b4

