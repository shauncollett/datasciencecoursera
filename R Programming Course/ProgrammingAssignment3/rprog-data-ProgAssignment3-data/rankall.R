# Reads the outcome-of-care-measures.csv file and returns a 2-column data frame
#   containing the hospital in each state that has the ranking specified in num.
# Takes two arguments:
#   1) an outcome name (outcome)
#   2) a hospital ranking (num).
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    state <- unique(data[order(data[,"State"]),"State"])
    
    ## For each state, find the hospital of the given rank
    hospital <- lapply(state, function(x) rankhospital(data, x, outcome, num))
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    as.data.frame(cbind(hospital, state))
}

# Accepts a data fram of the outcome-of-care-measures.csv file and returns a character vector
#   with the name of the hospital that has the ranking specified by the num argument.
# Takes four arguments:
#   1) Data frame of the outcome-of-care-measures.csv file
#   2) the 2-character abbreviated name of a state (state),
#   3) an outcome (outcome),
#   4) The ranking of a hospital in that state for that outcome (num).
rankhospital <- function(data, state, outcome, num = "best") {
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
#> source("rankall.R")
#> head(rankall("heart attack", 20), 10)
#hospital state
#AK <NA> AK
#AL D W MCMILLAN MEMORIAL HOSPITAL AL
#AR ARKANSAS METHODIST MEDICAL CENTER AR
#AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
#CA SHERMAN OAKS HOSPITAL CA
#CO SKY RIDGE MEDICAL CENTER CO
#CT MIDSTATE MEDICAL CENTER CT
#DC <NA> DC
#DE <NA> DE
#FL SOUTH FLORIDA BAPTIST HOSPITAL FL
#> tail(rankall("pneumonia", "worst"), 3)
#hospital state
#WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
#WV PLATEAU MEDICAL CENTER WV
#WY NORTH BIG HORN HOSPITAL DISTRICT WY
#> tail(rankall("heart failure"), 10)
#hospital state
#TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
#TX FORT DUNCAN MEDICAL CENTER TX
#UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
#VA SENTARA POTOMAC HOSPITAL VA
#VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
#VT SPRINGFIELD HOSPITAL VT
#WA HARBORVIEW MEDICAL CENTER WA
#WI AURORA ST LUKES MEDICAL CENTER WI
#WV FAIRMONT GENERAL HOSPITAL WV
#WY CHEYENNE VA MEDICAL CENTER WY