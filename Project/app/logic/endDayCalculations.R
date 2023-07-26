# cost = cost of flowers + cost of manpower
# revenue = sale of bouquets
# cashbal = cashbal - cost + revenue

# need to create:
# 1. dataframe called flowersBought with titles day, F1, F2, F3
# 2. dataframe called flowersInventory with titles day, F1, F2, F3
# 2. dataframe called manpower with titles day, quantity, capacity
# 4. dataframe called bouquetsSold with titles day, B1, B2, B3, B4, B5, B6
# 5. dataframe called bouquetsInventory with titles day, B1, B2, B3, B4, B5, B6 (final bouquetsInventory = initial bouquetsInventory - bouquetsSold for that day)

getPreviousDay(conn){
  currentDay <- dbGetQuery(conn, "SELECT MAX(day) FROM FlowerPower")$day
  return(currentDay)
} 

calculateCost(day, flowersBought, manpower){
  costFlower <- flowersBought$F1[flowersBought$day == day]*1 + flowersBought$F2[flowersBought$day == day]*0.5 + flowersBought$F3[flowersBought$day == day]*0.1
  coatManpower <- manpower$quantity[manpower$day == day]
  costTotal <- costFlower + coatManpower
  return(costTotal)
}
calculateRevenue(day, bouquetsSold){
  revenue <- bouquetsSold$B1[bouquetsSold$day == day]*10 +  bouquetsSold$B2[bouquetsSold$day == day]*5 + bouquetsSold$B3[bouquetsSold$day == day]*2 + bouquetsSold$B4[bouquetsSold$day == day]*7 + bouquetsSold$B5[bouquetsSold$day == day]*4 + bouquetsSold$B6[bouquetsSold$day == day]*7
  return(revenue)
}

calculateCashBal(conn, day, cashBal, cost, revenue){
  cashBal <- retrieve(conn, cashBal) #idk how retrieve the cashbal here
  revenue <- calculateRevenue(day, bouquetsSold)
  cost <- calculateCost(day, flowersBought, manpower)
  finalCashBal <- cashBal - cost + revenue
  return(finalCashBal)
}
