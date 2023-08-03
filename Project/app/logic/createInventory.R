# need to add in createInventory() at the start in server.R
# i think we should create a currentDay counter cuz easier to use local than database day

# creates the following dataframes
# eodOrdered: day, F1, F2, F3, manpower
# flowersInventory
# bouquetsInventory
# ordersFulfilled
# ordersUnfulfilled 

createEodOrdered <- function() {
  eodOrdered <- data.frame(
    day = 0:31,
    F1 = rep(0, 32),
    F2 = rep(0, 32),
    F3 = rep(0, 32),
    manpower = rep(0,32)
  )
  eodOrdered
}

createFlowersInventory <- function() {
  flowersInventory <- data.frame(
    day = 0:31,
    F1 = rep(0, 32),
    F2 = rep(0, 32),
    F3 = rep(0, 32),
    F1e = rep(0, 32),
    F2e = rep(0, 32),
    F3e = rep(0, 32)
  )
  flowersInventory
}

createBouquetsInventory <- function() {
  bouquetsInventory <- data.frame(
    day = 0:31,
    B1 = rep(0, 32),
    B2 = rep(0, 32),
    B3 = rep(0, 32),
    B4 = rep(0, 32),
    B5 = rep(0, 32),
    B6 = rep(0, 32)
  )
  bouquetsInventory
}

createOrdersFulfilled <- function() {
  ordersFulfilled <- data.frame(
    day = 0:31,
    B1 = rep(0, 32),
    B2 = rep(0, 32),
    B3 = rep(0, 32),
    B4 = rep(0, 32),
    B5 = rep(0, 32),
    B6 = rep(0, 32)
  )
  ordersFulfilled
}
# # Call the function to initialize the dataframes
# createInventory()
# 
# # Now you can directly access the dataframes by their names
# # Example:
# flowersBought
# flowersInventory
# manpower
# bouquetsSold
# bouquetsInventory