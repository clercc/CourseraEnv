rankhospital <- function(state, outcome, num = "best") {
        data <- read.csv("C:/users/cclerc/my documents/github/datasciencecoursera/outcome-of-care-measures.csv", colClasses = "character")
        
        death <- if (outcome == "heart attack") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == 'pneumonia') {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        statelist <- data[data$State == state, c("Hospital.Name", death)]
        
        if(nrow(statelist) == 0) {
                stop("invalid state")
        }
        
        statelist[,2] <- as.numeric(statelist[,2])
        ordered_statelist <- order(statelist[death], statelist$Hospital.Name, na.last=NA)
        
        if (num == "best") {
                as.character(statelist$Hospital.Name[ordered_statelist[1]])
        } else if (num == "worst") {
                as.character(statelist$Hospital.Name[ordered_statelist[length(ordered_statelist)]])
        } else if (is.numeric(num)) {
                as.character(statelist$Hospital.Name[ordered_statelist[num]])
        } else {
                stop('invalid num')
        }
}
