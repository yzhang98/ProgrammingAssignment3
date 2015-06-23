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
## Find the best hosptical in a state.
##
## The function returns the name of the hospital that has the best (i.e. lowest)
## 30-day mortality for the specified outcome in that state.
##
## The state is a two-characters string, e.g. "TX", "CA", "MD" etc.
## The outcome can be one of "heart attack", "heart failure", or "pneumonia".
##
best <- function(state, outcome) {
        ## read outcome data
        outcome.data <- readOutcome()
        
        ## Check that state and outcome are valid
        checkParameters(outcome.data, state, outcome)
        
        ## Return hospital name in that state with lowest 30-day death rate
        ods = outcome.data[outcome.data[, 7] == state, ]
        bestId = 
                if(outcome == "heart attack") {
                        which.min(ods[, 11])
                } else if(outcome == "heart failure") {
                        which.min(ods[, 17])
                } else {
                        which.min(ods[, 23])
                }
 
        ods[bestId, 2]   # best hospital name
}

##
## Perform unit tests with the given cases in the assignment pdf.
##
library(RUnit)
unitTest_best <- function() {
        print(checkEquals(c("CYPRESS FAIRBANKS MEDICAL CENTER"), best("TX", "heart attack")))
        print(checkEquals(c("FORT DUNCAN MEDICAL CENTER"), best("TX", "heart failure")))
        print(checkEquals(c("JOHNS HOPKINS HOSPITAL, THE"), best("MD", "heart attack")))
        print(checkEquals(c("GREATER BALTIMORE MEDICAL CENTER"), best("MD", "pneumonia")))
        print(checkException(best("BB", "heart attack")))
        print(checkException(best("NY", "hert attack")))
}

