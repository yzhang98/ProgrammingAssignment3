##
## read outcome-of-care-measures.csv into a data frame and convert key columns to numeric
##
readOutcome <- function() {
        outcome.data = read.csv("outcome-of-care-measures.csv", colClasses = "character")
        suppressWarnings(outcome.data[, 11] <- as.numeric(outcome.data[, 11])) # Motality rate from Heart Attack
        suppressWarnings(outcome.data[, 17] <- as.numeric(outcome.data[, 17])) # Motality rate from Heart Failure
        suppressWarnings(outcome.data[, 23] <- as.numeric(outcome.data[, 23])) # Motality rate from Penumonia
        outcome.data        
}

##
## checkParameters() check if the state and outcome are valid parameters
## if state = NA, checkParameters() will not check state
## if outcome = NA, checkParameters() will not check state
##
checkParameters <- function(outcome.data, state = NA, outcome = NA) {
        validStates = unique(outcome.data[, 7])
        validOutcomes = c("heart attack", "heart failure", "pneumonia")
        if(!is.na(state) && !(state %in% validStates)) {
                stop("invalid state")
        }
        
        if(!is.na(outcome) && !(outcome %in% validOutcomes)) {
                stop("invalid outcome")
        }
}

##
## Ranking hospitals by outcome in a state.
##
## This function returns the hospital name in the given state that has the ranking specified
## by the num argument. When there is a tie, rank by the alphabetical order of the hospital names.
##
## The state is a two-characters string, e.g. "TX", "CA", "MD" etc.
## The outcome can be one of "heart attack", "heart failure", or "pneumonia".
## The num can take values "best", "worst", or an integer indicating the raning (smaller 
## numbers are better).
##
rankhospital <- function(state, outcome, num = "best") {
        ## read outcome data
        outcome.data <- readOutcome()
        
        
        ## Check that state and outcome are valid
        checkParameters(outcome.data, state, outcome)
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        ods = outcome.data[outcome.data[, 7] == state, ]
        if(outcome == "heart attack") {
                # Note: including ods[, 2] in order() below to handle tie
                ods.sorted <- ods[order(ods[, 11], ods[, 2], na.last = NA), ] 
        } else if(outcome == "heart failure") {
                ods.sorted <- ods[order(ods[, 17], ods[, 2], na.last = NA), ]
        } else {
                ods.sorted <- ods[order(ods[, 23], ods[, 2], na.last = NA), ]
        }
        
        hospital = NA
        if(num == "best") {
                hospital = ods.sorted[1, 2]
        } else if(num == "worst") {
                hospital = ods.sorted[nrow(ods.sorted), 2]
        } else if(num <= nrow(ods.sorted) && num >= 1) {
                hospital = ods.sorted[num, 2]
        } 
        
        hospital
}

##
## Perform unit tests with the given cases in the assignment pdf.
##
library(RUnit)
unitTest_rankhospital <- function() {
        print(checkEquals(c("DETAR HOSPITAL NAVARRO"), rankhospital("TX", "heart failure", 4)))
        print(checkEquals(c("HARFORD MEMORIAL HOSPITAL"), rankhospital("MD", "heart attack", "worst")))
        print(checkEquals(c("FORT DUNCAN MEDICAL CENTER"), rankhospital("TX", "heart failure", "best")))
        print(checkEquals(c("FORT DUNCAN MEDICAL CENTER"), rankhospital("TX", "heart failure")))
        print(checkEquals(NA, rankhospital("MN", "heart attack", 5000)))
        print(checkException(best("BB", "heart attack")))
        print(checkException(best("NY", "hert attack")))
}