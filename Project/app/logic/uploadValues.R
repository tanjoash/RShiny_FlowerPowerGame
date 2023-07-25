uploadCashBal <- function(conn, day, balance, playerid){
  querytemplate <- "UPDATE CashBal SET day?day = ?balance WHERE finalcashbalid = ?playerid"
  query <- sqlInterpolate(conn, querytemplate, day=day, balance=balance, playerid=playerid)
  dbExecute(conn, query)
}

uploadCost <- function(conn, day, cost, playerid){
  querytemplate <- "UPDATE Cost SET day?day = ?cost WHERE costid = ?playerid"
  query <- sqlInterpolate(conn, querytemplate, day=day, cost=cost, playerid=playerid)
  dbExecute(conn, query)
}

uploadRevenue <- function(conn, day, revenue, playerid){
  querytemplate <- "UPDATE Revenue SET day?day = ?revenue WHERE revenueid = ?playerid"
  query <- sqlInterpolate(conn, querytemplate, day=day, revenue=revenue, playerid=playerid)
  dbExecute(conn, query)
}