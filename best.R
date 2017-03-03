best <- function(state, outcome) {
        data <- read.csv("C:/users/cclerc/documents/github/datasciencecoursera/outcome-of-care-measures.csv", colClasses = "character")
        statelist <- data[,7]
        outcomelist <- c("heart attack", "heart failure", "pneumonia")
        
        if (state %in% statelist & outcome %in% outcomelist) {
                
        }
        else {
                print("please check your input")
        }
                        
}
        
        
        
        
