rankhospital <- function(state, outcome, num = "best") {
    library(plyr)
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    #read in the desired data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #check if the state and outcomes are valid
    states <- data[ , 7]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ((state %in% states) == FALSE) {
        stop(print("invalid state"))
    }
    else if ((outcome %in% outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    
    #get the subset of the data with the desired state
    new_data <- subset(data, State == state)
    
    #get the desired outcome column from the data file
    if (outcome == "heart attack") {
        outcome_column <- 11
    }
    else if (outcome == "heart failure") {
        outcome_column <- 17
    }
    else {
        outcome_column <- 23
    }
    
    #get rid of the NA's in the desired outcome column
    required_columns <- as.numeric(new_data[,outcome_column])
    bad <- is.na(required_columns)
    desired_data <- new_data[!bad, ]
    
    columns_considered <- as.numeric(desired_data[, outcome_column])
    order.outcome <- order(columns_considered)
    ordered_rows <- desired_data[order.outcome,]
    ordered_rows$rank <- seq.int(nrow(ordered_rows))
    
    
    
    
    if (num == "best") {
        toPrint <- head(ordered_rows[,c(2, outcome_column, 47)], n = 1L)
        colnames(toPrint) <- c("Hospital.Name", "Rate", "Rank")
        print(toPrint)
    }
    else if (num == "worse" ){
        toPrint <- tail(ordered_rows[,c(2, outcome_column, 47)], n = 1L)
        colnames(toPrint) <- c("Hospital.Name", "Rate", "Rank")
        print(toPrint)
    }
    else if (num %in% 1:nrow(ordered_rows)) {
        toPrint <- head(ordered_rows[,c(2, outcome_column, 47)], n = num)
        colnames(toPrint) <- c("Hospital.Name", "Rate", "Rank")
        print(toPrint)
    }
    else {
        stop(print("Invalid num"))
    }
}