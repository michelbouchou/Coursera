rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    #check if the state and outcomes are valid
    states <- unique(data[ , 7])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ((outcome %in% outcomes) == FALSE) {
        stop(print("invalid outcome"))
    }
    
    #instantiate wanted
    wanted <- data.frame(hospital = character(), state = character())
    
    for (state in states) {
        
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

        
        if (num == "best") {
            wanted <- rbind(wanted, ordered_rows[1 ,c(2, 7)])
        }
        else if (num == "worst" ){
            wanted <- rbind(wanted, ordered_rows[nrow(ordered_rows) ,c(2, 7)])
        }
        else if (num %in% 1:nrow(ordered_rows)) {
            wanted <- rbind(wanted, ordered_rows[num ,c(2, 7)])
        }
        else {
            wanted <- rbind(wanted, c(NA, state))
        }
    }
    wanted.order <- order(wanted[, 2])
    ordered.wanted <- wanted[wanted.order,]
    ordered.wanted
    
    
    
}
