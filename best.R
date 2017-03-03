best <- function(state, outcome) {
        data <- read.csv("C:/users/christian/github/datasciencecoursera/outcome-of-care-measures.csv", colClasses = "character")
        statelist <- data[,7]
        
        if (state %in% statelist & outcome == "heart attack") {
                newdata <- subset(data, data[,7] == state)
                x <- newdata[which.min(newdata[,11]), 2]
                x
        }
        
        else if (state %in% statelist & outcome == "heart failure") {
                newdata <- subset(data, data[,7] == state)
                x <- newdata[which.min(newdata[,17]), 2]
                x
        }
        
        else if (state %in% statelist & outcome == "pneumonia") {
                newdata <- subset(data, data[,7] == state)
                x <- newdata[which.min(newdata[,23]), 2]
                x
        }
        
        else if (!(state %in% statelist)) {
                stop("invalid state")
        }
        else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
}
                

        
        
