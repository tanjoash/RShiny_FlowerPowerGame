initialiseNameSeed <- function(conn, playerName, seedNumber){
  querytemplate <- "INSERT INTO FlowerPower (name, seed) VALUES (?playerName, ?seedNumber)"
  query <- sqlInterpolate(conn, querytemplate, playerName=playerName, seedNumber=seedNumber)
  dbExecute(conn, query)
  
  currentID <- dbGetQuery(conn, "SELECT MAX(playerID) AS newID FROM FlowerPower")$newID
  
  querytemplate <- "INSERT INTO Demand (demandid, b1id, b2id, b3id, b4id, b5id, b6id)
  VALUES (?currentID,?currentID,?currentID,?currentID,?currentID,?currentID,?currentID)"
  query <- sqlInterpolate(conn, querytemplate, currentID=currentID)
  dbExecute(conn, query)
  
  querytemplate <- "UPDATE FlowerPower SET finalcashbalid=?currentID, demandid=?currentID, revenueid=?currentID, costid=?currentID WHERE playerid=?currentID"
  query <- sqlInterpolate(conn, querytemplate, currentID=currentID)
  dbExecute(conn, query)
}

getPlayerID <- function(conn){
  result <- dbGetQuery(conn, "SELECT MAX(playerID) AS playerid FROM FlowerPower")$playerid
  result
}