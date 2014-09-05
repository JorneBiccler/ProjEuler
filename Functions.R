fibboGenerator <- function(max){
  #generate all fibonacci numbers strict less than max 
  #caution definition of the sequence used: start with 1,1,2,...
  x <- c(1,1)
  i <- 3
  temp <- x[1] + x[2] 
  while(temp < max){
    x[i] <- temp   
    i <- i+1
    temp <- x[i-2] + x[i-1] 
  } 
  x
}

isPrime <- function(n){
  if(n==2){
    return(TRUE)
  }
  return(!any(n%%2:sqrt(n) == 0))
}

numberOfDigits <- function(n){
  digits <- 0
  while(floor(n/(10^digits)) > 0){
    digits <- digits +1
  }
  digits
}

isPalidromic <- function(n){
  digits <- numberOfDigits(n)
    for(i in 1:(digits/2)){
      temp1 <- floor((n%%(10^i))/10^(i-1))
      temp2 <-floor(n/(10^(digits-i)))%%10
      if(!temp1==temp2){
        return(FALSE)
      }  
  }
  return(TRUE)
}

nthPrimeLowerBound <- function(n) {
  n*(log(n) -log(log(n))-1)
}

nthPrimeUpperBound <- function(n) {
  n*(log(n) + log(log(n)))
} 