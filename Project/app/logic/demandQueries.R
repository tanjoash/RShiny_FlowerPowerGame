## Demand SQL Queries ##
## Upload Bouquet 1 query ##
createNewB1 <- function(conn,demand){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO B1 (day1, day2, day3, day4, day5, day6, day7, day8,
  day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20,
  day21, day22, day23, day24, day25, day26, day27, day28, day29, day30, day31) VALUES 
  (?day1,?day2,?day3,?day4,?day5,?day6,?day7,?day8,?day9,?day10,?day11,?day12,?day13,
  ?day14,?day15,?day16,?day17,?day18,?day19,?day20,?day21,?day22,?day23,?day24,?day25,
  ?day26,?day27,?day28,?day29,?day30,?day31);"
  query <- sqlInterpolate(conn, querytemplate,day1=demand[[1]],day2=demand[[2]],
                          day3=demand[[3]],day4=demand[[4]],day5=demand[[5]],day6=demand[[6]],
                          day7=demand[[7]],day8=demand[[8]],day9=demand[[9]],day10=demand[[10]],
                          day11=demand[[11]],day12=demand[[12]],day13=demand[[13]],day14=demand[[14]],
                          day15=demand[[15]],day16=demand[[16]],day17=demand[[17]],day18=demand[[18]],
                          day19=demand[[19]],day20=demand[[20]],day21=demand[[21]],day22=demand[[22]],
                          day23=demand[[23]],day24=demand[[24]],day25=demand[[25]],day26=demand[[26]],
                          day27=demand[[27]],day28=demand[[28]],day29=demand[[29]],day30=demand[[30]],
                          day31=demand[[31]])
}

## Upload Bouquet 2 query ##
createNewB2 <- function(conn,demand){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO B2 (day1, day2, day3, day4, day5, day6, day7, day8,
  day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20,
  day21, day22, day23, day24, day25, day26, day27, day28, day29, day30, day31) VALUES 
  (?day1,?day2,?day3,?day4,?day5,?day6,?day7,?day8,?day9,?day10,?day11,?day12,?day13,
  ?day14,?day15,?day16,?day17,?day18,?day19,?day20,?day21,?day22,?day23,?day24,?day25,
  ?day26,?day27,?day28,?day29,?day30,?day31);"
  query <- sqlInterpolate(conn, querytemplate,day1=demand[[1]],day2=demand[[2]],
                          day3=demand[[3]],day4=demand[[4]],day5=demand[[5]],day6=demand[[6]],
                          day7=demand[[7]],day8=demand[[8]],day9=demand[[9]],day10=demand[[10]],
                          day11=demand[[11]],day12=demand[[12]],day13=demand[[13]],day14=demand[[14]],
                          day15=demand[[15]],day16=demand[[16]],day17=demand[[17]],day18=demand[[18]],
                          day19=demand[[19]],day20=demand[[20]],day21=demand[[21]],day22=demand[[22]],
                          day23=demand[[23]],day24=demand[[24]],day25=demand[[25]],day26=demand[[26]],
                          day27=demand[[27]],day28=demand[[28]],day29=demand[[29]],day30=demand[[30]],
                          day31=demand[[31]])
}

## Upload Bouquet 3 query ##
createNewB3 <- function(conn,demand){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO B3 (day1, day2, day3, day4, day5, day6, day7, day8,
  day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20,
  day21, day22, day23, day24, day25, day26, day27, day28, day29, day30, day31) VALUES 
  (?day1,?day2,?day3,?day4,?day5,?day6,?day7,?day8,?day9,?day10,?day11,?day12,?day13,
  ?day14,?day15,?day16,?day17,?day18,?day19,?day20,?day21,?day22,?day23,?day24,?day25,
  ?day26,?day27,?day28,?day29,?day30,?day31);"
  query <- sqlInterpolate(conn, querytemplate,day1=demand[[1]],day2=demand[[2]],
                          day3=demand[[3]],day4=demand[[4]],day5=demand[[5]],day6=demand[[6]],
                          day7=demand[[7]],day8=demand[[8]],day9=demand[[9]],day10=demand[[10]],
                          day11=demand[[11]],day12=demand[[12]],day13=demand[[13]],day14=demand[[14]],
                          day15=demand[[15]],day16=demand[[16]],day17=demand[[17]],day18=demand[[18]],
                          day19=demand[[19]],day20=demand[[20]],day21=demand[[21]],day22=demand[[22]],
                          day23=demand[[23]],day24=demand[[24]],day25=demand[[25]],day26=demand[[26]],
                          day27=demand[[27]],day28=demand[[28]],day29=demand[[29]],day30=demand[[30]],
                          day31=demand[[31]])
}

## Upload Bouquet 4 query ##
createNewB4 <- function(conn,demand){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO B4 (day1, day2, day3, day4, day5, day6, day7, day8,
  day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20,
  day21, day22, day23, day24, day25, day26, day27, day28, day29, day30, day31) VALUES 
  (?day1,?day2,?day3,?day4,?day5,?day6,?day7,?day8,?day9,?day10,?day11,?day12,?day13,
  ?day14,?day15,?day16,?day17,?day18,?day19,?day20,?day21,?day22,?day23,?day24,?day25,
  ?day26,?day27,?day28,?day29,?day30,?day31);"
  query <- sqlInterpolate(conn, querytemplate,day1=demand[[1]],day2=demand[[2]],
                          day3=demand[[3]],day4=demand[[4]],day5=demand[[5]],day6=demand[[6]],
                          day7=demand[[7]],day8=demand[[8]],day9=demand[[9]],day10=demand[[10]],
                          day11=demand[[11]],day12=demand[[12]],day13=demand[[13]],day14=demand[[14]],
                          day15=demand[[15]],day16=demand[[16]],day17=demand[[17]],day18=demand[[18]],
                          day19=demand[[19]],day20=demand[[20]],day21=demand[[21]],day22=demand[[22]],
                          day23=demand[[23]],day24=demand[[24]],day25=demand[[25]],day26=demand[[26]],
                          day27=demand[[27]],day28=demand[[28]],day29=demand[[29]],day30=demand[[30]],
                          day31=demand[[31]])
}

## Upload Bouquet 5 query ##
createNewB5 <- function(conn,demand){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO B5 (day1, day2, day3, day4, day5, day6, day7, day8,
  day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20,
  day21, day22, day23, day24, day25, day26, day27, day28, day29, day30, day31) VALUES 
  (?day1,?day2,?day3,?day4,?day5,?day6,?day7,?day8,?day9,?day10,?day11,?day12,?day13,
  ?day14,?day15,?day16,?day17,?day18,?day19,?day20,?day21,?day22,?day23,?day24,?day25,
  ?day26,?day27,?day28,?day29,?day30,?day31);"
  query <- sqlInterpolate(conn, querytemplate,day1=demand[[1]],day2=demand[[2]],
                          day3=demand[[3]],day4=demand[[4]],day5=demand[[5]],day6=demand[[6]],
                          day7=demand[[7]],day8=demand[[8]],day9=demand[[9]],day10=demand[[10]],
                          day11=demand[[11]],day12=demand[[12]],day13=demand[[13]],day14=demand[[14]],
                          day15=demand[[15]],day16=demand[[16]],day17=demand[[17]],day18=demand[[18]],
                          day19=demand[[19]],day20=demand[[20]],day21=demand[[21]],day22=demand[[22]],
                          day23=demand[[23]],day24=demand[[24]],day25=demand[[25]],day26=demand[[26]],
                          day27=demand[[27]],day28=demand[[28]],day29=demand[[29]],day30=demand[[30]],
                          day31=demand[[31]])
}

## Upload Bouquet 6 query ##
createNewB6 <- function(conn,demand){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO B6 (day1, day2, day3, day4, day5, day6, day7, day8,
  day9, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20,
  day21, day22, day23, day24, day25, day26, day27, day28, day29, day30, day31) VALUES 
  (?day1,?day2,?day3,?day4,?day5,?day6,?day7,?day8,?day9,?day10,?day11,?day12,?day13,
  ?day14,?day15,?day16,?day17,?day18,?day19,?day20,?day21,?day22,?day23,?day24,?day25,
  ?day26,?day27,?day28,?day29,?day30,?day31);"
  query <- sqlInterpolate(conn, querytemplate,day1=demand[[1]],day2=demand[[2]],
                          day3=demand[[3]],day4=demand[[4]],day5=demand[[5]],day6=demand[[6]],
                          day7=demand[[7]],day8=demand[[8]],day9=demand[[9]],day10=demand[[10]],
                          day11=demand[[11]],day12=demand[[12]],day13=demand[[13]],day14=demand[[14]],
                          day15=demand[[15]],day16=demand[[16]],day17=demand[[17]],day18=demand[[18]],
                          day19=demand[[19]],day20=demand[[20]],day21=demand[[21]],day22=demand[[22]],
                          day23=demand[[23]],day24=demand[[24]],day25=demand[[25]],day26=demand[[26]],
                          day27=demand[[27]],day28=demand[[28]],day29=demand[[29]],day30=demand[[30]],
                          day31=demand[[31]])
}

uploadB1 <- function(conn, demand){
  query <- createNewB1(conn, demand)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("uploadDemand1: ERROR")
        print(cond)
      }
    )
  } # end while loop
}

uploadB2 <- function(conn, demand){
  query <- createNewB2(conn, demand)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("uploadDemand1: ERROR")
        print(cond)
      }
    )
  } # end while loop
}

uploadB3 <- function(conn, demand){
  query <- createNewB3(conn, demand)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("uploadDemand1: ERROR")
        print(cond)
      }
    )
  } # end while loop
}

uploadB4 <- function(conn, demand){
  query <- createNewB4(conn, demand)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("uploadDemand1: ERROR")
        print(cond)
      }
    )
  } # end while loop
}

uploadB5 <- function(conn, demand){
  query <- createNewB5(conn, demand)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("uploadDemand1: ERROR")
        print(cond)
      }
    )
  } # end while loop
}

uploadB6 <- function(conn, demand){
  query <- createNewB6(conn, demand)
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        success <- TRUE
      }, error=function(cond){print("uploadDemand1: ERROR")
        print(cond)
      }
    )
  } # end while loop
}