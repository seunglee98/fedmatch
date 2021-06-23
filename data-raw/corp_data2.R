## code to prepare `corp_data2` dataset goes here

corp_data2 <- data.table::data.table(
  Name = c(
    "Walmart", "Bershire Hathaway", "Apple Computer", "Exxon Mobile Inc. ",
    "McKesson Corp.",
    "UnitedHealth Group",
    "CVS",
    "GM",
    "AT & T",
    "Ford Motor"
  ),
  country = c(rep("USA", 4), "", "USA", "", "", rep("USA", 2)),
  state_code = c("OH", "NE", "CA", "TX", "MA", "MA", "RI", "MI", "TN", "MI"),
  SIC_code = c(3380, 2220, NA, 2222, 2222, 1130, 1122, 2222, 4000, 2222),
  earnings = c(
    "490,000", "220,000", "220,000", "210,000", "190,000",
    "180,000", "180,000", "170,000", "160,000", "150,000"
  ),
  unique_key_2 = seq(1, 10)
)

usethis::use_data(corp_data2, overwrite = TRUE)
