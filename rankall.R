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
## Ranking hospitals in all states
##
## This function returns a 2-column data frame containing the hospital in each state
## that has the ranking specified in num. When there is a tie, rank by the 
## alphabetical order of the hospital names. The data frame is sorted by its row names,
## which are states.
##
## The outcome can be one of "heart attack", "heart failure", or "pneumonia".
## The num can take values "best", "worst", or an integer indicating the raning (smaller 
## numbers are better).
## 
rankall <- function(outcome, num = "best") {
        ## read outcome data
        outcome.data <- readOutcome()
        
        ## Check that state and outcome are valid
        checkParameters(outcome.data, outcome = outcome)

        ## initialize the data frame to return
        res <- data.frame(hospital = as.character(), state = as.character())

        ## For each state, find the hospital of the given rank
        # get all states, sorted
        allStates = sort(unique(outcome.data[, 7]))
        
        for(state in allStates) {
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
                
                res = rbind(res, data.frame(hospital = hospital, state = state, row.names = state))
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        res
}

##
## Perform unit tests with the given cases in the assignment pdf.
##
library(RUnit)
unitTest_rankall <- function() {
        # rade saved data frame results
        check1 <- dget("rankall_check1.R")
        check2 <- dget("rankall_check2.R")
        check3 <- dget("rankall_check3.R")
        
        # perform unit tests
        print(checkEquals(check1, head(rankall("heart attack", 20), 10)))
        print(checkEquals(check2, tail(rankall("pneumonia", "worst"), 3)))
        print(checkEquals(check3, tail(rankall("heart failure"), 10)))
}