# Reads the outcome-of-care-measures.csv file and returns a character vector
#   with the name of the hospital that has the ranking specified by the num argument.
# Takes three arguments:
#   1) the 2-character abbreviated name of a state (state),
#   2) an outcome (outcome),
#   3) The ranking of a hospital in that state for that outcome (num).
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeCol <- 1
    if(outcome == "heart attack") outcomeCol <- 11
    if(outcome == "heart failure") outcomeCol <- 17
    if(outcome == "pneumonia") outcomeCol <- 23
    hospitals <- data[data$State == state, c(2, outcomeCol)]
    rows <- nrow(hospitals)
    
    # Check that the state and outcome are valid
    if(rows == 0) stop("invalid state")
    if(outcomeCol == 1) stop("invalid outcome")
    
    ## Return hospital name in that state with the given rank
    names(hospitals) <- c("name", "outcome")
    hospitals[, 2] <- as.numeric(hospitals[, 2]) #imported as character
    hospitals <- na.omit(hospitals)
    rows <- nrow(hospitals)
    hospitals <- hospitals[order(hospitals[, 2], hospitals[, 1]), ]
    #hospitals[45,]
    
    hospital <- NA # assume the hospital doesn't exist
    if (num == "best") hospital <- hospitals[1, "name"]
    else if (num == "worst") hospital <- hospitals[rows, "name"]
    else if (num > 0 & num <= rows) hospital <- hospitals[num, "name"]
    hospital
}

## SAMPLE CALLS
#> source("rankhospital.R")
#> rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
#> rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
#> rankhospital("MN", "heart attack", 5000)
#[1] NA