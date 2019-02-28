best <- function (state, outcome){
    ## Read outcome data
    evaluate_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available") )
    
    ## Check that state and outcome are valid
    if(!any(state == evaluate_data$State)){
        stop("invalid state")
    }
    if( !any(outcome == c("heart attack", "heart failure", "pneumonia")) ){
        stop("invalid outcome")
    }
    
    ## Select the data
    evaluate_data <- evaluate_data[ evaluate_data$State == state, ] 
    
    if(outcome == "heart attack"){
        evaluate_data <- evaluate_data[ , c(2,11) ]
    }
    else if(outcome == "heart failure"){
        evaluate_data <- evaluate_data[ , c(2,17) ]
    }
    else if(outcome == "pneumonia"){
        evaluate_data <- evaluate_data[ , c(2,23) ]
    }
    
    ## Adjust the data
    evaluate_data <- evaluate_data[complete.cases(evaluate_data),]
    evaluate_data[, 2] <- as.numeric(evaluate_data[,2])
    
    ## Return hospital name in that state with lowest 30-day death rate
    print( evaluate_data[ order(evaluate_data[,2], evaluate_data[,1]), 1 ][1] )
}

