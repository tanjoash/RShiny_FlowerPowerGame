## Execute demand generation and upload demand to DBeaver ##
#source("logic/setAWSPassword.R")
source("logic/demandQueries.R")
source("logic/nameSeedQueries.R")
source("logic/initialiseDay0.R")
source("logic/uploadValues.R")

## connect to database ##
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student096",
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student096",
    password = "ReGenbDVMnhY")
  conn
}

uploadDemand <- function(d1,d2,d3,d4,d5,d6, playerName, seed){
  conn <- getAWSConnection()
  initialiseTable(conn)
  uploadB1(conn,d1)
  uploadB2(conn,d2)
  uploadB3(conn,d3)
  uploadB4(conn,d4)
  uploadB5(conn,d5)
  uploadB6(conn,d6)
  initialiseNameSeed(conn, playerName, seed)
  dbDisconnect(conn)
}


# To get demand, call (getDemandEachDay(day, playerid))
getDemandEachDay <- function(day, playerid){
  conn <- getAWSConnection()
  result <- retrieve(conn, day, playerid)
  dbDisconnect(conn)
  return(result) #old was just result
}

getOldCashBal <- function(day){
  conn <- getAWSConnection()
  cashbal <- getCashBal(conn, day)
  dbDisconnect(conn)
  return(cashbal)
}

uploadValues <- function(day, cashBal, cost, revenue, playerid){
  conn <- getAWSConnection()
  uploadCashBal(conn, day, cashBal, playerid)
  uploadCost(conn, day, cost, playerid)
  uploadRevenue(conn, day, revenue, playerid)
  dbDisconnect(conn)
}

playerID <- function(){
  conn <- getAWSConnection()
  playerid <- getPlayerID(conn)
  dbDisconnect(conn)
  playerid
}

getLeaderBoard <- function(){
  conn <- getAWSConnection()
  # DO SQL FN TO GET TOP 10
  dbDisconnect(conn)
}
