#Jeanelle and Joash created the random demand generations based on the seed

## demand list ##
b1 <- list()
b2 <- list()
b3 <- list()
b4 <- list()
b5 <- list()
b6 <- list()

source("logic/database.R")

## user input seed ##
setSeed <- function(playerName, userid, seed = sample(1:100000, 1)){
  ## set seed ##
  set.seed(seed)
  
  ## generate days ##
  days <- seq(1, 31)
  
  ## 3R3C linear demand ##
  b1_demand <- function(day){
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
  
  ## 3R5B linear demand ##
  b2_demand <- function(day){
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
  
  ## 3C5B linear demand ##
  b3_demand <- function(day){
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
  
  ## 5R linear demand ##
  b4_demand <- function(day){
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
  
  ## 5C  linear demand ##
  b5_demand <- function(day){
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
  
  ## 10B linear demand ##
  b6_demand <- function(day){
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
  
  ## function to return a random number from a norm dist ##
  normal <- function(mean, sd=1){
    x <- round(rnorm(1, mean=mean, sd=sd), 0)
    x
  }
  
  for (x in days) {
    b1_val <- normal(b1_demand(x))
    if (b1_val < 0){
      b1_val <- 0
    }
    b1 <- append(b1, b1_val)
    
    b2_val <- normal(b2_demand(x))
    if (b2_val < 0){
      b2_val <- 0
    }
    b2 <- append(b2, b2_val)
    
    b3_val <- normal(b3_demand(x))
    if (b3_val < 0){
      b3_val <- 0
    }
    b3 <- append(b3, b3_val)
    
    b4_val <- normal(b4_demand(x))
    if (b4_val < 0){
      b4_val <- 0
    }
    b4 <- append(b4, b4_val)
    
    b5_val <- normal(b5_demand(x))
    if (b5_val < 0){
      b5_val <- 0
    }
    b5 <- append(b5, b5_val)
    
    b6_val <- normal(b6_demand(x))
    if (b6_val < 0){
      b6_val <- 0
    }
    b6 <- append(b6, b6_val)
  }
  
  uploadDemand(b1,b2,b3,b4,b5,b6, playerName, seed, userid)
}


# demand forecast
forecastday <- list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
forecastB1 <- list(2, 3, 4, 2, 3, 4, 5, 4, 6, 5,	7, 8, 9, 10, 11, 10, 11, 12, 11, 12, 11, 12, 12, 10, 14, 15, 16, 16, 17, 18, 16)
forecastB2 <- list(2, 3, 3, 3, 3, 4, 4, 6, 5, 6, 7, 7, 8, 8, 8, 9, 8, 8, 8, 9, 9, 10, 11, 10, 11, 11, 12,	12, 15, 14, 15)
forecastB3 <- list(2, 1, 2, 3, 2, 2, 5, 3, 5, 3,	4, 5, 6, 5, 4, 5, 5, 7, 6, 8,	5, 4, 8, 7, 7, 5, 6, 9, 8, 10, 8)
forecastB4 <- list(2, 3, 3, 2, 2, 3, 3, 5, 4, 5,	6, 7, 6, 7, 6, 8, 9, 9, 10, 11, 10, 10, 10, 11, 10, 14,	13, 12, 14, 13, 15)
forecastB5 <- list(1, 2, 1, 2, 1, 3, 2, 3, 2, 4,	3, 4, 3, 4, 5, 5, 5, 6, 5, 7,	6, 5, 8, 7, 10, 8, 6, 9, 7, 9, 11)
forecastB6 <- list(2, 3, 3, 4, 4, 4, 4, 5, 4, 7, 5, 5, 6, 6, 6, 5, 8, 8, 8, 9, 9, 10, 11, 12, 14, 14, 13, 13, 15, 16, 14)

total_demand_calc <- function(day, playerid){
  if (day == 0){
    result <- 0
  } else {
    demand <- getDemandEachDay(day, playerid)
    result <- as.integer(sum(demand[1,]))
  }
  result
}