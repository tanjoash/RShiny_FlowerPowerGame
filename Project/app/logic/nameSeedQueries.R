#Emily and Joash did these

initialiseNameSeed <- function(conn, playerName, userid, seedNumber){
  querytemplate <- "INSERT INTO FlowerPower (name, seed) VALUES (?playerName, ?seedNumber)"
  query <- sqlInterpolate(conn, querytemplate, playerName=playerName, seedNumber=seedNumber)
  dbExecute(conn, query)
  
  currentID <- dbGetQuery(conn, "SELECT MAX(playerID) AS newID FROM FlowerPower")$newID
  
  querytemplate <- "INSERT INTO Demand (demandid, b1id, b2id, b3id, b4id, b5id, b6id)
  VALUES (?currentID,?currentID,?currentID,?currentID,?currentID,?currentID,?currentID)"
  query <- sqlInterpolate(conn, querytemplate, currentID=currentID)
  dbExecute(conn, query)
  
  querytemplate <- "UPDATE FlowerPower SET finalcashbalid=?currentID, demandid=?currentID, revenueid=?currentID, costid=?currentID, userid=?userid WHERE playerid=?currentID"
  query <- sqlInterpolate(conn, querytemplate, currentID=currentID, userid=userid)
  dbExecute(conn, query)
}

getPlayerID <- function(conn){
  result <- dbGetQuery(conn, "SELECT MAX(playerID) AS playerid FROM FlowerPower")$playerid
  result
}

getUniqueUsers <- function(conn){
  result <- dbGetQuery(conn, "SELECT DISTINCT username FROM User")
  result
}

getuseridfromdb <- function(conn, username, password){
  querytemplate <- "SELECT * FROM User WHERE username=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=username,id2=password)
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    userid <- result$userid[1]
  } else {
    userid <- 0
  }
  userid
}

register <- function(conn, userName, password){
  querytemplate <- "INSERT INTO User (username, password) VALUES (?userName, ?password)"
  query <- sqlInterpolate(conn,querytemplate, userName = userName, password = password)
  dbExecute(conn, query)
}

getLeaderboard <- function(conn){
  query <- "SELECT u.username, cb.day31 AS Score
    FROM User AS u
    INNER JOIN FlowerPower AS fp ON u.userid = fp.userid
    INNER JOIN CashBal AS cb ON fp.finalcashbalid = cb.finalcashbalid
    ORDER BY Score DESC
    LIMIT 5;"
  result <- dbGetQuery(conn, query)
  result
}

getRevCostCash <- function(conn, playerid){
  querytemplate <- "SELECT * FROM Revenue INNER JOIN Cost
    ON Revenue.revenueid = Cost.costid INNER JOIN CashBal
    ON Cost.costid = CashBal.finalcashbalid WHERE CashBal.finalcashbalid = ?playerid"
  query <- sqlInterpolate(conn, querytemplate, playerid=playerid)
  result <- dbGetQuery(conn, query)
  result
}