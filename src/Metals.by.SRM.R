data1 <- read.csv("CUSRM.csv", header = TRUE)


library(ggplot2)
p <- ggplot(data1, aes(x = data1$LOGIN_DATE,y = data1$ENTRY , color = data1$ANALYSIS)) + geom_point(size=4, alpha = 1) 
p

p + facet_wrap(~ SAMPLING_POINT, ncol=1) # individual panels
