outcome <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
head(outcome)

levels(outcome[,7])
contains("AZ",levels(outcome[,7]))
print(("XY" %in% outcome[,7]))
print(colnames(outcome))
         
outcome=outcome[order(outcome[, 17]),]

      
best <- function(state, outcome)  {
  
  data=read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
  

  if (!state %in% data[,7]) {stop("The state not found")} 
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))  { stop("invalid outcome") }
  
  col<- if(outcome == "heart attack") {
    11
  }else if(outcome=="heart failure") {
    17
  }else {
    23
  }
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  
  datafilt=subset(data, State==state)
  ordered=datafilt[order(datafilt[, col],datafilt[ , 2] ,na.last = NA, decreasing = FALSE),]
  ordered[1,2]
}

best("TX", "heart failure")

rankhospital <- function(state, outcome, num) {

  
data=read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")


if (!state %in% data[,7]) {stop("The state not found")} 

if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))  { stop("invalid outcome") }

col<- if(outcome == "heart attack") {
  11
}else if(outcome=="heart failure") {
  17
}else {
  23
}
data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
data[, 2] <- as.character(data[, 2])
datafilt=subset(data, State==state)
ordered=datafilt[order(datafilt[, col],datafilt[ , 2] ,na.last = NA, decreasing = FALSE),]
if((!is.numeric(num))&(!num=="worst")&(!num=="best")) {stop("invalid entry for num")}

if (num=="best") {
 return(ordered[1,2])
}
else if (num=="worst") {
return(tail(ordered[,2],1))
}

else if (is.numeric(num)){
return(ordered[num,2])}
}


rankall <- function(outcome, num="best") {
  
  data=read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv")
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))  { stop("invalid outcome") }
  
  col<- if(outcome == "heart attack") {
    11
  }else if(outcome=="heart failure") {
    17
  }else {
    23
  }
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  
  output <- vector()
  states <- levels(data[,7])
  for(i in 1:length(states)) {
    datafilt=data[grep(states[i], data$State), ]
    ordered=datafilt[order(datafilt[, col],datafilt[ , 2] ,na.last = NA, decreasing = FALSE),]
    hospital <- if(num=="best"){
      ordered[1,2]
    }else if(num=="worst"){
      tail(ordered[,2],1)
    }else{
      ordered[num,2]
    }
    output<-append(output, c(hospital,states[i]))
    }
  output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
  colnames(output) <- c("hospital", "state")
  rownames(output) <- states
  return(output)
}

