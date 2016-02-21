#Extract data from LIMS using the guide xxx

data1 <- read.csv("data/PVAL05SRM2.csv", as.is=TRUE, header = TRUE)
#Tidying up column names.
colnames(data1)[1] <- 'Sample'
colnames(data1)[7] <- 'Result'
colnames(data1)[4] <- 'Operator'

# Splitting the TEXT_ID field to extract the IRM/SRM numbers.
data.in.srm <- strsplit(data1$TEXT_ID, split="-")
SRM <- sapply(data.in.srm,function(x) x[2])
SRM <- as.data.frame(SRM)
data1 <- cbind(data1, SRM)

####### Plot by Operators #######
p <- ggplot(data1, aes(x = Sample,y = Result, color = Operator)) + geom_point(size=5, alpha = 1)
p

p + facet_wrap(~ Operator, ncol=2) # individual panels

