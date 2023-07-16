## Execute demand generation and upload demand to DBeaver ##
#source("logic/setAWSPassword.R")
source("logic/demandQueries.R")

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

uploadDemand <- function(d1,d2,d3,d4,d5,d6){
  conn <- getAWSConnection()
  uploadB1(conn,d1)
  uploadB2(conn,d2)
  uploadB3(conn,d3)
  uploadB4(conn,d4)
  uploadB5(conn,d5)
  uploadB6(conn,d6)
  dbDisconnect(conn)
}