library(rmarkdown)

data.raw <- read.csv("~/Documents/GitHub/SRM_Package/data/VITB6.csv", as.is=TRUE, header=TRUE)

srm_report <- function (x, max.pts = 100, points = 20, doc_type = "html") {
        code <- substr(x$ANALYSIS[1],1,6)
        print(doc_type)
        if(doc_type == "docx") {
                rmarkdown::render("~/Documents/GitHub/SRM_Package/reports/Control_Chart_Review.Rmd", 
                                  output_format = "word_document",
                                  output_file = paste(code, " Control Chart Review", ".docx", sep=""),
                                  output_dir = "~/Documents/GitHub/SRM_Package/reports/Output")
                
        } else if(doc_type == "pdf") {
                rmarkdown::render("~/Documents/GitHub/SRM_Package/reports/Control_Chart_Review.Rmd", 
                                  output_format = "pdf_document",
                                  output_file = paste(code, " Control Chart Review", ".pdf", sep=""),
                                  output_dir = "~/Documents/GitHub/SRM_Package/reports/Output")
                
        } else {
                rmarkdown::render("~/Documents/GitHub/SRM_Package/reports/Control_Chart_Review.Rmd", 
                        output_format = "html_document",
                        output_file = paste(code, " Control Chart Review", ".html", sep=""),
                        output_dir = "~/Documents/GitHub/SRM_Package/reports/Output")
        }
}



srm_report (data.raw, max.pts = 200, points = 30, doc_type="docx")




