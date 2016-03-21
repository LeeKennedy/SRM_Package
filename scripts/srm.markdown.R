library(rmarkdown)

# Data input --------------------------------------------------
data.raw <- read.csv("C:/Users/lkennedy/GitHub/Git_Projects/SRM_Package/data/SRM115B.csv", as.is=TRUE, header=TRUE)

# Function (don't change) -------------------------------------
srm_report <- function (x, max.pts = 200, points = 20, doc_type = "docx") {
        code <- substr(x$ANALYSIS[1],1,6)
        print(doc_type)
        if(doc_type == "docx") {
                rmarkdown::render("C:/Users/lkennedy/GitHub/Git_Projects/SRM_Package/reports/Control_Chart_Review.Rmd", 
                                  output_format = "word_document",
                                  output_file = paste(code, " Control Chart Review", ".docx", sep=""),
                                  output_dir = "C:/Users/lkennedy/GitHub/Git_Projects/SRM_Package/reports/Output")
                
        } else if(doc_type == "pdf") {
                rmarkdown::render("C:/Users/lkennedy/GitHub/Git_Projects/SRM_Package/reports/Control_Chart_Review.Rmd", 
                                  output_format = "pdf_document",
                                  output_file = paste(code, " Control Chart Review", ".pdf", sep=""),
                                  output_dir = "C:/Users/lkennedy/GitHub/Git_Projects/SRM_Package/reports/Output")
                
        } else {
                rmarkdown::render("C:/Users/lkennedy/GitHub/Git_Projects/SRM_Package/reports/Control_Chart_Review.Rmd", 
                        output_format = "html_document",
                        output_file = paste(code, " Control Chart Review", ".html", sep=""),
                        output_dir = "C:/Users/lkennedy/GitHub/Git_Projects/SRM_Package/reports/Output")
        }
}


# Run report--------------------------------------------------------------
srm_report (data.raw, max.pts = 200, points = 10, doc_type="pdf")




