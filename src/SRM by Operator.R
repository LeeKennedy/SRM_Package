library(dplyr)
data1 <- read.csv("Taurine_SRM_Ops.csv", header = TRUE)
colnames(data1)[1] <- 'Sample'
colnames(data1)[8] <- 'Result'
colnames(data1)[4] <- 'Operator'

data1 <- select(data1, everything())%>%
      filter(SAMPLING_POINT=="IRM001A")%>%
      mutate(Result = as.numeric(as.character(Result)))%>%
      na.omit

ave <- median(data1$Result)


library(ggplot2)
p <- ggplot(data1, aes(x = Sample,y = Result, color = Operator)) + 
        geom_point(size=5, alpha = 1) +
        geom_abline(intercept = ave, slope = 0) +
        ggtitle("SRM11E")
p

p + facet_wrap(~ Operator, ncol=2) # individual panels
