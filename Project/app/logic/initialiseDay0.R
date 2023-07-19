initialiseTable <- function(conn){
  querytemplate <- "INSERT INTO CashBal (day0) VALUES (?vals);"
  query <- sqlInterpolate(conn, querytemplate, vals=500)
  dbExecute(conn, query)
  
  querytemplate <- "INSERT INTO Cost (day0) VALUES (?vals);"
  query <- sqlInterpolate(conn, querytemplate, vals=0)
  dbExecute(conn, query)
  
  querytemplate <- "INSERT INTO Revenue (day0) VALUES (?vals);"
  query <- sqlInterpolate(conn, querytemplate, vals=0)
  dbExecute(conn, query)
}