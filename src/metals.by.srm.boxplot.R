metals <- read.csv("Metals2.csv", as.is=TRUE, header=TRUE)

#Use Control-Shift-C to toggle SRM
srm <- "SRM75F_CL"
# srm <- "IRM001A_CL"
metal <- "Phosphorus"

with(subset(metals, Analyte == metal & Control==srm),
     boxplot(Result ~ Method,
      col = "bisque",
      ylab = "Result",
      main = paste(srm,"by two ICP methods, for", metal)))