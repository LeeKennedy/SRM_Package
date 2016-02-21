metals <- read.csv("Metals2.csv", as.is=TRUE, header=TRUE)
library(dplyr)
library(ggplot2)

metal1 <- "Iron"

metal2 <-filter(metals,Analyte==metal1)


p <- ggplot(metal2, aes(x = Sample,y = Result, color = Method)) + geom_point(size=3, alpha = 1) 

q <- p + facet_wrap(~ Control, ncol=2)
q