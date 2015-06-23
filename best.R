best <- function(state, outcome) {
  ## Read outcome data
  OOCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  OOCM_state <- OOCM[OOCM$State == state,]
  ## Check that state and outcome are valid
  if(nrow(OOCM_state)==0) stop('invalid state')
  if(tolower(outcome) == 'heart attack') col = 11
  else if(tolower(outcome) == 'heart failure') col = 17
  else if(tolower(outcome) == 'pneumonia') col = 23
  else stop('invalid outcome')
  ## Return hospital name in that state with lowest 30-day death rate
  OOCM_state <- OOCM_state[!(OOCM_state[,col] == "Not Available"),]
  OOCM_state[,col] <- as.numeric(OOCM_state[,col])
  ##OOCM_state[,2][OOCM_state[,11]==14.6]
  OOCM_state[,2][order(OOCM_state[,col],OOCM_state[,2])][1]
}

##   __..-- TESTS --..__
## -----------------------

## best("TX", "heart attack")
## "CYPRESS FAIRBANKS MEDICAL CENTER"

## best("TX", "heart failure")
## "FORT DUNCAN MEDICAL CENTER"

## best("MD", "heart attack")
## "JOHNS HOPKINS HOSPITAL, THE"

## best("MD", "pneumonia")
## "GREATER BALTIMORE MEDICAL CENTER"

## best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state

## best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome

