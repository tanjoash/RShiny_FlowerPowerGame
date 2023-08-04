# Jeanelle did all the dataframes updating and mathematics

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