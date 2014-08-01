#Problem 1
prob1 <- function(max){
  sum <- 0
  count <- 0
  while( count < 1000){
    sum <- sum + count
    count <- count + 3
  }
  count <- 0
  while(count < 1000){
    if(count %% 3 != 0){
      sum <- sum + count
    }
    count <- count + 5 
  }
  sum
}
prob1(1000)

#Problem 2
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

prob2 <- function(max){
  x <- fibboGenerator(max)
  count <- 0
  for(i in x){
    if(i%%2 == 0){
      count <- count + i
    }
  }
  count
}
prob2(4000000)



