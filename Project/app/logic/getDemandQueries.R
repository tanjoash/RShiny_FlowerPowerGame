retrieve <- function(conn, day, playerid){
  querytemplate <- "SELECT B1.day?day, B2.day?day, B3.day?day, B4.day?day, B5.day?day, B6.day?day FROM Demand
  INNER JOIN B1 ON Demand.b1id = B1.b1id
  INNER JOIN B2 ON Demand.b2id = B2.b2id
  INNER JOIN B3 ON Demand.b3id = B3.b3id
  INNER JOIN B4 ON Demand.b4id = B4.b4id
  INNER JOIN B5 ON Demand.b5id = B5.b5id
  INNER JOIN B6 ON Demand.b6id = B6.b6id
  WHERE Demand.demandid = ?playerid"
  query <- sqlInterpolate(conn, querytemplate, day=day, playerid=playerid)
  result <- dbGetQuery(conn,query)
  result
}