# Needs work
data.raw <- read.csv("data/VITB6.csv", as.is=TRUE, header=TRUE)

srm_report <- function (x, max.pts = 100, points = 20) {
        library(rmarkdown)
        code <- substr(x$ANALYSIS[1],1,6)
        rmarkdown::render("reports/Control_Chart_Review.Rmd", output_file = paste(code, " Control Chart Review", ".docx", sep=""))
}


srm_report (data.raw)


