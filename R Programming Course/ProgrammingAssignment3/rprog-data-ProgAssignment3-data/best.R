# Function reads the outcome-of-care-measures.csv file and returns a character 
#   vector with the name of the hospital that has the best (i.e. lowest) 30-day
#   mortality for the specified outcome in that state.
# Ties are handled alphabetically.
# Arguments: 1) 2-character abbreviated name of a state; 2) an outcome name.
best <- function(state, outcome){
    # Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeCol <- 1
    if(outcome == "heart attack") outcomeCol <- 11
    if(outcome == "heart failure") outcomeCol <- 17
    if(outcome == "pneumonia") outcomeCol <- 23
    hospitals <- data[data$State == state, c(2, outcomeCol)]
    
    # Check that the state and outcome are valid
    if(nrow(hospitals) == 0) stop("invalid state")
    if(outcomeCol == 1) stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death
    names(hospitals) <- c("name", "outcome")
    hospitals[, 2] <- as.numeric(hospitals[, 2]) #imported as character
    hospitals <- na.omit(hospitals)
    lowest <- min(hospitals$outcome)
    hospitals <- hospitals[hospitals$outcome == lowest, "name"]
    hospitals <- sort(hospitals)
    hospitals[1]
}

## SAMPLE CALLS
#> source("best.R")
#> best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
#> best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
#> best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
#> best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
#> best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
#> best("NY", "hert attack")
#Error in best