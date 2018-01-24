library(tidyverse)

months <- list("All Year", "January", "February", "March",
  "April", "May", "June", "July", "August", "September",
  "October", "November", "December")

categories <- list("income", "savings", "expenses")

subcategories <- categories %>% lapply(function(x) {
  if (x == "income") list("salary", "other")
  else if (x == "savings") list("savings", "investments")
  else list("rent", "utilities", "transportation", "groceries",
      "daily", "entertainment", "medical", "vacation", "personal")
})

names(subcategories) <- categories
flat_subcategories <- flatten(subcategories)

createCol <- function(x, salary, other, savings, investments, rent, utilities, 
  transportation, groceries, daily, entertainment, medical, vacation, personal) {
    if (x == "salary")              salary
    else if (x == "other")          other
    else if (x == "savings")        savings
    else if (x == "investments")    investments
    else if (x == "rent")           rent
    else if (x == "utilities")      utilities
    else if (x == "transportation") transportation
    else if (x == "groceries")      groceries
    else if (x == "daily")          daily
    else if (x == "entertainment")  entertainment
    else if (x == "medical")        medical
    else if (x == "vacation")       vacation
    else if (x == "personal")       personal
}

origins <- flat_subcategories %>% lapply(createCol,
  salary         = c("work"), 
  other          = c("gift"), 
  savings        = NA_character_,
  investments    = NA_character_, 
  rent           = c("home"),
  utilities      = c("home"), 
  transportation = c("gas", "maintenance", "uber", "public"), 
  groceries      = c("jumbo"), 
  daily          = c("cafe", "eat out", "event"), 
  entertainment  = c("netflix", "spotify", "enconomist", "amazon"), 
  medical        = c("doctor", "dentist", "meds", "tests"), 
  vacation       = c("airbnb", "hotel", "transportation", "other"), 
  personal       = c("aws", "rescuetime")
)

amounts <- flat_subcategories %>% lapply(createCol,
  salary         = c(1500),
  other          = c(50, 100, 200),
  savings        = c(50, 100, 200),
  investments    = c(50, 100, 200),
  rent           = c(315),
  utilities      = c(50, 100, 150),
  transportation = c(50, 100, 150),
  groceries      = c(100, 200, 300),
  daily          = c(10, 20, 50, 100),
  entertainment  = c(6.99, 13.99, 15),
  medical        = c(10, 20, 50, 100),
  vacation       = c(100, 200, 300, 500),
  personal       = c(10, 50, 100, 200, 300)
)

descriptions <- flat_subcategories %>% lapply(createCol,
  salary         = c("foo", "bar"),
  other          = c("foo", "bar"),
  savings        = c("foo", "bar"),
  investments    = c("foo", "bar"),
  rent           = c("foo", "bar"),
  utilities      = c("foo", "bar"),
  transportation = c("foo", "bar"),
  groceries      = c("foo", "bar"),
  daily          = c("foo", "bar"),
  entertainment  = c("foo", "bar"),
  medical        = c("foo", "bar"),
  vacation       = c("foo", "bar"),
  personal       = c("foo", "bar")
)

names(origins) <- names(amounts) <- names(descriptions) <- flat_subcategories

dat_metadata <- tibble(
  #year,month,day,
  amounts,
  origins,
  #categories,
  flat_subcategories,
  descriptions)

dat <- read_csv("budget.csv")
