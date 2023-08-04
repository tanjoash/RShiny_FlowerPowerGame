# Jeanelle did all the mathematics and dataframe updating here

# cost = cost of flowers + cost of manpower
# revenue = sale of bouquets
# cashbal = cashbal - cost + revenue

# databases: 
# eodOrdered
# flowersInventory
# bouquetsInventory
# ordersFulfilled
# ordersUnfulfilled 

getCashBal <- function(conn, day, playerid){
  querytemplate <- "SELECT day?number AS cashbal FROM CashBal WHERE finalcashbalid = ?playerid"
  query <- sqlInterpolate(conn, querytemplate, number=day, playerid=playerid)
  cashbal <- dbGetQuery(conn, query)$cashbal
}

# the code below assumes that the ordered flowers and manpower is stored as F1, F2, F3, manpower. update as needed
# this is to be stored right after player orders
# name of flowers is F1, F2, F3
updateEodOrdered <- function(day, eodOrdered, F1, F2, F3, manpower){
  row_index <- day+1 #1 day 0
  eodOrdered[row_index, "F1"] <- F1
  eodOrdered[row_index, "F2"] <- F2
  eodOrdered[row_index, "F3"] <- F3
  eodOrdered[row_index, "manpower"] <- manpower
  return(eodOrdered)
}

# At start of day
updateFlowersInvStart <- function(day, flowersInventory, eodOrdered){
  row_index <- day+1 #1 day 0
  flowersInventory[row_index+1, "F1e"] <- flowersInventory[row_index, "F1"] #2 day 1e <- #1 day 0
  flowersInventory[row_index+1, "F2e"] <- flowersInventory[row_index, "F2"]
  flowersInventory[row_index+1, "F3e"] <- flowersInventory[row_index, "F3"]
  flowersInventory[row_index+1, "F1"] <- eodOrdered[row_index, "F1"] #2 day 1 <- #1 day0 EODordered
  flowersInventory[row_index+1, "F2"] <- eodOrdered[row_index, "F2"]
  flowersInventory[row_index+1, "F3"] <- eodOrdered[row_index, "F3"]
  flowersInventory
}

# this cost has to be stored in db
calculateCost <- function(day, eodOrdered){
  costFlower <- eodOrdered$F1[eodOrdered$day == day]*1 + eodOrdered$F2[eodOrdered$day == day]*0.5 + eodOrdered$F3[eodOrdered$day == day]*0.1
  costManpower <- eodOrdered$manpower[eodOrdered$day == day]*10
  costTotal <- costFlower + costManpower
  return(costTotal) # for the next day costs
}
# the global day + 1 here
# name of flowers is B1, B2, B3, B4, B5, B6

updateBouquetsMade <- function(day, bouquetsInventory, B1, B2, B3, B4, B5, B6){
  row_index <- day+1
  bouquetsInventory[row_index, "B1"] <- B1
  bouquetsInventory[row_index, "B2"] <- B2
  bouquetsInventory[row_index, "B3"] <- B3
  bouquetsInventory[row_index, "B4"] <- B4
  bouquetsInventory[row_index, "B5"] <- B5
  bouquetsInventory[row_index, "B6"] <- B6
  bouquetsInventory
}

updateFlowersUsed <- function(day, flowersInventory, F1, F2, F3, F1e, F2e, F3e){ #to change
  row_index <- day+1

  # update expiring
  flowersInventory[row_index, "F1e"] <- F1e
  flowersInventory[row_index, "F2e"] <- F2e
  flowersInventory[row_index, "F3e"] <- F3e

  # update fresh
  flowersInventory[row_index, "F1"] <- F1
  flowersInventory[row_index, "F2"] <- F2
  flowersInventory[row_index, "F3"] <- F3
  flowersInventory
}

# When user clicks run simulation or open shop or smth
# At the end of the start of the day
updateOrdersFulfilled <- function(day, currentDemand, B1_make, B2_make, B3_make, B4_make, B5_make, B6_make, ordersFulfilled){
  row_index <- day+1
  
  demand_B1 <- currentDemand[1]
  demand_B2 <- currentDemand[2]
  demand_B3 <- currentDemand[3]
  demand_B4 <- currentDemand[4]
  demand_B5 <- currentDemand[5]
  demand_B6 <- currentDemand[6]
  
  # update the ordersFulfilled dataframe for the given day
  ordersFulfilled[row_index, "B1"] <- B1_make
  ordersFulfilled[row_index, "B2"] <- B2_make
  ordersFulfilled[row_index, "B3"] <- B3_make
  ordersFulfilled[row_index, "B4"] <- B4_make
  ordersFulfilled[row_index, "B5"] <- B5_make
  ordersFulfilled[row_index, "B6"] <- B6_make
  
  ordersFulfilled
}

# this revenue has to be stored in db - not done
calculateRevenue <- function(day, ordersFulfilled){
  revenue <- ordersFulfilled$B1[ordersFulfilled$day == day]*9 +  ordersFulfilled$B2[ordersFulfilled$day == day]*7 + ordersFulfilled$B3[ordersFulfilled$day == day]*4 + ordersFulfilled$B4[ordersFulfilled$day == day]*10 + ordersFulfilled$B5[ordersFulfilled$day == day]*5 + ordersFulfilled$B6[ordersFulfilled$day == day]*2
  return(revenue)
}

# this cashbal has to be stored in db - not done
calculateCashBal <- function(cost, revenue, oldcashBal){
  newCashBal <- oldcashBal - cost + revenue
  return(newCashBal)
}
