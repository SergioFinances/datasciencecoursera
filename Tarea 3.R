
Hospital <- read.csv("hospital-data.csv", header = T, sep = ",")
View(Hospital)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

best <- function(state, outcome) {
  df_outcome <- read.csv("outcome-of-care-measures.csv", header = T, sep = ",", stringsAsFactors = F, dec = ",")
  val_states <- unique(df_outcome[,7])  
  val_outcome <- c("heart attack", "heart failure", "pneumonia")
  if (state %in% val_states & outcome %in% val_outcome){
    if (outcome == "heart attack"){
      res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      res[,2] <- as.numeric(res[,2])
      Minimo <- min(res[,2])
      Hosp <- res[res[,2] == Minimo,]
      return(Hosp[,1])
    } else if (outcome == "heart failure"){
      res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      res[,2] <- as.numeric(res[,2])
      Minimo <- min(res[,2])
      Hosp <- res[res[,2] == Minimo,]
      print(Hosp[,1])   
    } else {
      res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      res[,2] <- as.numeric(res[,2])
      Minimo <- min(res[,2])
      Hosp <- res[res[,2] == Minimo,]
      print(Hosp[,1])        
    }
  } else {
    if (state %in% val_states) {
      print("invalid state")
    } else {
      print("invalid outcome")
    }
  }
}

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

#rankhospital <- function (state, outcome, num){
  df_outcome <- read.csv("outcome-of-care-measures.csv", header = T, sep = ",", stringsAsFactors = F, dec = ",")
  val_states <- unique(df_outcome[,7])  
  val_outcome <- c("heart attack", "heart failure", "pneumonia")
  if (state %in% val_states & outcome %in% val_outcome){
    if (num  == "best"){
      if (outcome == "heart attack"){
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        res[,2] <- as.numeric(res[,2])
        Minimo <- min(res[,2])
        Hosp <- res[res[,2] == Minimo,]
        return(Hosp[,1])

      } else if (outcome == "heart failure"){
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        res[,2] <- as.numeric(res[,2])
        Minimo <- min(res[,2])
        Hosp <- res[res[,2] == Minimo,]
        return(Hosp[,1]) 
      } else {
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        res[,2] <- as.numeric(res[,2])
        Minimo <- min(res[,2])
        Hosp <- res[res[,2] == Minimo,]
        return(Hosp[,1])        
      }
    } else if (num  == "worst") {
      if (outcome == "heart attack"){
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        res[,2] <- as.numeric(res[,2])
        Maximo <- max(res[,2])
        Hosp <- res[res[,2] == Maximo,]
        return(Hosp[,1])
      } else if (outcome == "heart failure"){
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        res[,2] <- as.numeric(res[,2])
        Maximo <- max(res[,2])
        Hosp <- res[res[,2] == Maximo,]
        return(Hosp[,1]) 
      } else {
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        res[,2] <- as.numeric(res[,2])
        Maximo <- max(res[,2])
        Hosp <- res[res[,2] == Maximo,]
        return(Hosp[,1])      
      }
    } else {
      if (outcome == "heart attack"){
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        res[,2] <- as.numeric(res[,2])
        res <- res[order(res[,2], res[,1]),]
        return(res[num,1]) 
      } else if (outcome == "heart failure"){
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        res[,2] <- as.numeric(res[,2])
        res <- res[order(res[,2], res[,1]),]
        return(res[num,1])  
      } else {
        res <- subset(df_outcome, State == state & Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available", c(Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        res[,2] <- as.numeric(res[,2])
        res <- res[order(res[,2], res[,1]),]
        return(res[num,1])     
      }
    }

  } else {
    if (state %in% val_states) {
      print("invalid state")
    } else {
      print("invalid outcome")
    }
  }
}

rankhospital <- function(state, outcome, num = "best") {
  # read outcome
  data <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character')
  
  if(!any(state == data$State)) {
    stop('invalid state')
  }
  
  if(outcome == 'heart attack') {
    i <- 11
  }
  else if(outcome == 'heart failure') {
    i <- 17
  }
  else if(outcome == 'pneumonia') {
    i <- 23
  }
  else {
    stop('invalid outcome')
  }
  
  data.state <- data[data$State == state, ]
  data.state[, i] <- as.numeric(x=data.state[, i])
  
  
  data.state <- data.state[complete.cases(data.state), ]
  
  # print(data.state[, c(2, i)])
  
  if(num == "best") {
    num = 1
  }
  else if(num == "worst") {
    num = nrow(data.state)
  }
  else if(is.numeric(x=num)) {
    # print(num)
    if(num<1 || num > nrow(data.state)) {
      return(NA)
    }
  }
  else {
    stop('invalid num')
  }
  
  
  # print(sort(data.state[, i]))
  
  data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
  
  return.names <- data.state[num, ]$Hospital.Name
  
  # print(return.names)
  
  return.names[1]
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

rankall <- function(outcome, num = "best") {
  data <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character')
  
  if(outcome == 'heart attack') {
    i <- 11
  }
  else if(outcome == 'heart failure') {
    i <- 17
  }
  else if(outcome == 'pneumonia') {
    i <- 23
  }
  else {
    stop('invalid outcome')
  }
  
  unique.states <- sort(unique(data$State))
  # print(unique.states)
  
  result.df <- list()
  
  for(state in unique.states) {
    data.state <- data[data$State == state, ]
    data.state[, i] <- as.numeric(x=data.state[, i])
    data.state <- data.state[complete.cases(data.state), ]
    
    # print(num)
    
    if(num == "best") {
      numrank = 1
    }
    else if(num == "worst") {
      numrank = nrow(data.state)
      # if(state == 'WI') {
      #   print(num)
      #   print('WI num')
      # }
    }
    else if(is.numeric(x=num)) {
      # print(num)
      if(num < 1 || num > nrow(data.state)) {
        result.df <- rbind(result.df, list(NA, state))
        print(state)
        next
      }
      else numrank <- num
      # print(num)
    }
    else {
      stop('invalid num')
    }
    
    # print(num)
    data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
    
    # if(state == 'WI') {
    #  print(data.state[, c(2,i)])
    #  print(numrank)
    #  print(nrow(data.state))
    # }
    
    return.names <- data.state[numrank, ]$Hospital.Name
    
    # print(return.names[1])
    
    result.df <- rbind(result.df, list(return.names[1], state))
    # print(result.df)
  }
  
  result.df <- as.data.frame(x=result.df)
  colnames(x=result.df) <- c('hospital', 'state')
  
  result.df
}

r <- rankall("heart attack", 4)

as.character(subset(r, state == "HI")$hospital)


r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)


r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
