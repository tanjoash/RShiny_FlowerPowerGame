# need to add in createInventory() at the start in server.R
# i think we should create a currentDay counter cuz easier to use local than database day

# creates the following dataframes
# eodOrdered: day, F1, F2, F3, manpower
# flowersInventory
# bouquetsInventory
# ordersFulfilled
# ordersUnfulfilled 
createInventory <- function() {
  # Create the eodOrdered dataframe
  eodOrdered <- data.frame(
    day = 0:31,
    F1 = rep(0, 32),
    F2 = rep(0, 32),
    F3 = rep(0, 32),
    manpower = rep(0,32)
  )
  assign("eodOrdered", eodOrdered, envir = .GlobalEnv)
  
  # Create the flowersInventory dataframe
  flowersInventory <- data.frame(
    day = 0:31,
    F1 = rep(0, 32),
    F2 = rep(0, 32),
    F3 = rep(0, 32),
    F1e = rep(0, 32),
    F2e = rep(0, 32),
    F3e = rep(0, 32)
  )
  assign("flowersInventory", flowersInventory, envir = .GlobalEnv)
  
  # Create the bouquetsInventory dataframe
  bouquetsInventory <- data.frame(
    day = 0:31,
    B1 = rep(0, 32),
    B2 = rep(0, 32),
    B3 = rep(0, 32),
    B4 = rep(0, 32),
    B5 = rep(0, 32),
    B6 = rep(0, 32),
    B1e = rep(0, 32),
    B2e = rep(0, 32),
    B3e = rep(0, 32),
    B4e = rep(0, 32),
    B5e = rep(0, 32),
    B6e = rep(0, 32)
  )
  assign("bouquetsInventory", bouquetsInventory, envir = .GlobalEnv)
  
  # Create the ordersFulfilled dataframe
  ordersFulfilled <- data.frame(
    day = 0:31,
    B1 = rep(0, 32),
    B2 = rep(0, 32),
    B3 = rep(0, 32),
    B4 = rep(0, 32),
    B5 = rep(0, 32),
    B6 = rep(0, 32)
  )
  assign("ordersFulfilled", ordersFulfilled, envir = .GlobalEnv)
  
  # Create the ordersUnfulfilled dataframe
  ordersUnfulfilled <- data.frame(
    day = 0:31,
    B1 = rep(0, 32),
    B2 = rep(0, 32),
    B3 = rep(0, 32),
    B4 = rep(0, 32),
    B5 = rep(0, 32),
    B6 = rep(0, 32)
  )
  assign("ordersUnfulfilled", ordersUnfulfilled, envir = .GlobalEnv)
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