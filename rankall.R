rankall <- function(outcome, num = "best") {
    
    library(plyr)
    
    ## Read outcome data
    evaluate_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = c("NA", "Not Available") )
    
    ## Check that the outcome are valid
    if( !any(outcome == c("heart attack", "heart failure", "pneumonia")) ){
        stop("invalid outcome")
    }
    
    ## Select the data
    if(outcome == "heart attack"){
        i <- 11
    }
    else if(outcome == "heart failure"){
        i <- 17
    }
    else if(outcome == "pneumonia"){
        i <- 23
    }
    
    evaluate_data <- evaluate_data[ , c(2,7,i) ]
    evaluate_data <- evaluate_data[complete.cases(evaluate_data),]
    
    
    ## Adjust the data in accordance to the states
    evaluate_data[,3] <- as.numeric(evaluate_data[,3])
    
    frag <- split(evaluate_data[,c(1,3)], evaluate_data$State)
    
    new <- lapply(frag, function(df) {
        arrange(df, df[[2]], df[[1]])
    } )
    
    ## For each state, find the hospital of the given rank
    if(num == "best"){
        simplify <- lapply(new, function(df){
            df[c(1,2)][1,1]
        })
    } else if(num =="worst"){
        simplify <- lapply(new, function(df){
            df[c(1,2)][dim(df)[1],1]
        })
    } else {
        num <- as.numeric(num)
        simplify <- lapply(new, function(df){
            df[c(1,2)][num,1]
        })
    }

    ## Return a data frame with the hospital names and the (abbreviated) state name
    data_fr <- ldply(simplify, data.frame) 
    colnames(data_fr) <- c("state", "hospital")
    row.names(data_fr) <- data_fr$state
    data_fr <- data_fr[,c(2,1)]
}