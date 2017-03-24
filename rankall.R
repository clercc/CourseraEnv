rankall <- function(outcome, num = "best") {
        data <- read.csv("C:/users/cclerc/my documents/github/datasciencecoursera/outcome-of-care-measures.csv", colClasses = "character")
        
        outcome <- if (outcome == "heart attack") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if (outcome == 'pneumonia') {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        data[,outcome] <- suppressWarnings(as.numeric(data[,outcome]))
        data <- data[order(data$"State", data[outcome], data$"Hospital.Name", na.last = NA),]
        data <- data[!is.na(outcome)]
        
        l <- split(data[,c("Hospital.Name")], data$State)
        
        rankhopspitals <- function(x, num) {
                if (num == "best") {
                        head(x,1)
                } else if (num == "worst") {
                        tail(x,1)
                } else {
                        x[num]
                }
        }
        
        result <- lapply (l,rankhopspitals, num)
        data.frame(hospital = unlist(result), state = names(result), row.names = names(result))        
        }