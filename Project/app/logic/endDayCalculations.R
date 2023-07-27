# cost = cost of flowers + cost of manpower
# revenue = sale of bouquets
# cashbal = cashbal - cost + revenue

# databases: 
# eodOrdered
# flowersInventory
# bouquetsInventory
# ordersFulfilled
# ordersUnfulfilled 

currentDemand <- getDemandEachDay(day, playerid)

getCashBal <- function(conn, day, playerid){
  querytemplate <- "SELECT day?number AS cashbal FROM CashBal WHERE finalcashbalid = ?playerid"
  query <- sqlInterpolate(conn, querytemplate, number=day, playerid=playerid)
  cashbal <- dbGetQuery(conn, query)$cashbal
}

# the code below assumes that the ordered flowers and manpower is stored as F1, F2, F3, manpower. update as needed
updateEodOrdered <- function(day, eodOrdered, F1, F2, F3, manpower){
  row_index <- which(eodOrdered$day == day+1)
  eodOrdered[row_index, "F1"] <- F1
  eodOrdered[row_index, "F2"] <- F2
  eodOrdered[row_index, "F3"] <- F3
  eodOrdered[row_index, "manpower"] <- manpower
}

updateFlowersInvStart <- function(day, flowersInventory, eodOrdered){
  row_index <- which(eodOrdered$day == day+1)
  flowersInventory[row_index, "F1"] <- flowersInventory[row_index, "F1"]
  flowersInventory[row_index, "F1"] <- eodOrdered[row_index, "F1"]
  flowersInventory[row_index, "F2"] <- eodOrdered[row_index, "F2"]
  flowersInventory[row_index, "F3"] <- eodOrdered[row_index, "F3"]
}

calculateCost(day, eodOrdered){
  costFlower <- eodOrdered$F1[eodOrdered$day == day]*1 + eodOrdered$F2[eodOrdered$day == day]*0.5 + eodOrdered$F3[eodOrdered$day == day]*0.1
  costManpower <- eodOrdered$quantity[eodOrdered$day == day]
  costTotal <- costFlower + costManpower
  return(costTotal)
}

updateOrdersFulfilled <- function(){
  
}

calculateRevenue(day, ordersFulfilled){
  revenue <- ordersFulfilled$B1[ordersFulfilled$day == day]*10 +  ordersFulfilled$B2[ordersFulfilled$day == day]*5 + ordersFulfilled$B3[ordersFulfilled$day == day]*2 + ordersFulfilled$B4[ordersFulfilled$day == day]*7 + ordersFulfilled$B5[ordersFulfilled$day == day]*4 + ordersFulfilled$B6[ordersFulfilled$day == day]*7
  return(revenue)
}

calculateCashBal(conn, day, cashBal, cost, revenue){
  oldcashBal <- getOldCashBal(day)
  revenue <- calculateRevenue(day, bouquetsSold)
  cost <- calculateCost(day, flowersBought, manpower)
  newCashBal <- oldcashBal - cost + revenue
  return(newCashBal)
}
