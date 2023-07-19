## demand list ##
b1 <- list()
b2 <- list()
b3 <- list()
b4 <- list()
b5 <- list()
b6 <- list()

source("logic/database.R")

## user input seed ##
setSeed <- function(playerName, seed = sample(1:100000, 1)){
  ## set seed ##
  set.seed(seed)
  
  ## generate days ##
  days <- seq(1, 31)
  
  ## bouquet 1 linear demand ##
  b1_demand <- function(day){
    if (day == 5 || day == 6){
      5
    } else if (day == 12 || day == 13){
      10  
    } else if (day == 18){
      16
    } else if (day == 19){
      24
    } else if (day == 20){
      35
    } else if (day == 21){
      21
    } else if (day == 22 || day == 26){
      14
    } else if (day == 27){
      13
    } else {
      y <- 0.4302*day + 1
      y
    }
  }
  
  ## bouquet 2 linear demand ##
  b2_demand <- function(day){
    if (day == 12 || day == 16){
      5
    } else if (day == 12 || day == 13){
      10  
    } else if (day == 13){
      7
    } else if (day == 14){
      10
    } else if (day == 15){
      8
    } else {
      y <- 0.2839*day + 0.5
      y
    }
  }
  
  ## bouquet 3 linear demand ##
  b3_demand <- function(day){
    if (day == 5 || day == 6){
      6
    } else if (day == 12){
      7
    } else if (day == 13){
      8
    } else if (day == 19){
      10
    } else if (day == 20){
      12
    } else if (day == 26 || day == 27){
      15
    } else {
      y <- 0.4379*day + 1
      y
    }
  }
  
  ## bouquet 4 linear demand ##
  b4_demand <- function(day){
    if (day == 5 || day == 6){
      6
    } else if (day == 12 || day == 13){
      11
    } else if (day == 18){
      16
    } else if (day == 19){
      29
    } else if (day == 20){
      40
    } else if (day == 21){
      26
    } else if (day == 22){
      18
    } else if (day == 26){
      15
    } else if (day == 27){
      16
    } else {
      y <- 0.3875*day + 1.8323
      y
    }
  }
  
  ## bouquet 5 linear demand ##
  b5_demand <- function(day){
    if (day == 12){
      7
    } else if (day == 13 || day == 15){
      9
    } else if (day == 14){
      15
    } else if (day == 16){
      6
    } else {
      y <- 0.2173*day + 1.6839
      y
    }
  }
  
  ## bouquet 6 linear demand ##
  b6_demand <- function(day){
    if (day == 5 || day == 6){
      5
    } else if (day == 12 || day == 22){
      13
    } else if (day == 13){
      18
    } else if (day == 14){
      20
    } else if (day == 15){
      16
    } else if (day == 16){
      14
    } else if (day == 18||day == 21){
      15
    } else if (day == 19){
      19
    } else if (day == 20){
      24
    } else if (day == 26){
      17
    } else if (day == 27){
      16
    } else {
      y <- 0.5117*day + 1.3613
      y
    }
  }
  
  ## function to return a random number from a norm dist ##
  normal <- function(mean, sd=1){
    x <- round(rnorm(1, mean=mean, sd=sd), 0)
    x
  }
  
  for (x in days) {
    b1_val <- normal(b1_demand(x))
    b1 <- append(b1, b1_val)
    
    b2_val <- normal(b2_demand(x))
    b2 <- append(b2, b2_val)
    
    b3_val <- normal(b3_demand(x))
    b3 <- append(b3, b3_val)
    
    b4_val <- normal(b4_demand(x))
    b4 <- append(b4, b4_val)
    
    b5_val <- normal(b5_demand(x))
    b5 <- append(b5, b5_val)
    
    b6_val <- normal(b6_demand(x))
    b6 <- append(b6, b6_val)
  }
  
  uploadDemand(b1,b2,b3,b4,b5,b6, playerName, seed)
}



