## code to prepare `corp_data1` dataset goes here

corp_data1 <- data.table::data.table(
  Company = c(
    "Walmart", "Bershire Hataway", "Apple", "Exxon Mobile",
    "McKesson ",
    "UnitedHealth Group",
    "CVS Health",
    "General Motors",
    "AT&T",
    "Ford Motor Company"
  ),
  Country = c(rep("USA", 4), "Germany", rep("USA", 5)),
  State = c("OH", "", "CA", "TX", "MA", "MA", "RI", "MI", "TN", "MI"),
  SIC = c(3300, 2222, 3384, 2222, 222, NA_integer_, 1112, 2222, 4000, NA_integer_),
  Revenue = c(485, 223, 215, 205, 192, 184, 177, 166, 163, 151),
  unique_key_1 = seq(1, 10)
)

usethis::use_data(corp_data1, overwrite = TRUE)
