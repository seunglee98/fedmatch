## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fedmatch)
library(data.table)

## -----------------------------------------------------------------------------

data("corp_data1", package = "fedmatch")
data("corp_data2", package = "fedmatch")

## -----------------------------------------------------------------------------
corp_data1_test <- copy(corp_data1)
data.table::setDT(corp_data1_test)
corp_data2_test <- copy(corp_data2)
data.table::setDT(corp_data2_test)

corp_data1_test[, id_1 := seq(1, .N)]
corp_data2_test[, id_2 := seq(1, .N)]

corp_data2_test[, Country := country]
corp_data2_test[, Company := Name]
result <- merge_plus(
  data1 = corp_data1_test,
  data2 = corp_data2_test,
  match_type = "multivar",
  by = c("Country", "Company"),
   suffixes = c("_1", "_2"),
  unique_key_1 = "id_1",
  unique_key_2 = "id_2", 
  multivar_settings = build_multivar_settings(
  compare_type = c("indicator", "stringdist"),
  wgts = c(.5, .5), nthread = 1
 
))
result

## -----------------------------------------------------------------------------
print(result$matches[, .(Company_1, Company_2, Company_compare)])
print(result$matches[, .(Country_1, Country_2, Country_compare)])

## -----------------------------------------------------------------------------

print(result$matches[, .(Company_compare, Country_compare, multivar_score)])

## -----------------------------------------------------------------------------
set.seed(111)
fake_result_table <- data.table::data.table(
    Company_1 = c("ABC Corp", "XYZ Corporation", "Apple Corp", "Banana Corp"),
    Company_2 = c("ABC Corporation", "XYZ Inc", "Apple Incorporated", "Banana Stand"),
    Country_1 = c("USA", "USA", "TUR", "USA"),
    Country_2 = c("MEX", "USA", "TUR", "USA")
  )
calculated_weights <- calculate_weights(fake_result_table, c("Company", "Country"),
                                        compare_type = c("stringdist", "indicator"),
                                        suffixes = c("_1", "_2"))
calculated_weights$w


## -----------------------------------------------------------------------------
set.seed(111)
corp_data1_test <- copy(corp_data1)
data.table::setDT(corp_data1_test)
corp_data2_test <- copy(corp_data2)
data.table::setDT(corp_data2_test)
corp_data1_test[, id_1 := seq(1, .N)]
corp_data2_test[, id_2 := seq(1, .N)]

corp_data2_test[, Country := country]
corp_data2_test[, Company := Name]
set.seed(111)
fake_result_table <- data.table::data.table(
  match = sample(c(1, 0, 1), 1e5, replace = TRUE),
  Company_compare = runif(1e5),
  Country_compare = sample(c(1, 0), 1e5, replace = TRUE)
)

logit_model <- glm(match ~ Company_compare + Country_compare,
  family = "binomial",
  data = fake_result_table
)

summary(logit_model)

result <- merge_plus(corp_data1_test, corp_data2_test,
                     match_type = "multivar",
                     multivar_settings = build_multivar_settings(logit = logit_model, compare_type = c("indicator", "stringdist"),
  wgts = NULL, nthread = 1),
  by = c("Country", "Company"), unique_key_1 = "id_1",
  unique_key_2 = "id_2", 
  suffixes = c("_1", "_2")
)
result

## -----------------------------------------------------------------------------
corp_data1_test <- copy(corp_data1)
data.table::setDT(corp_data1_test)
corp_data2_test <- copy(corp_data2)
data.table::setDT(corp_data2_test)
corp_data1_test[, id_1 := seq(1, .N)]
corp_data2_test[, id_2 := seq(1, .N)]

corp_data2_test[, Country := country]
corp_data2_test[, Company := Name]
result <- merge_plus(
  data1 = corp_data1_test,
  data2 = corp_data2_test,
  match_type = "multivar",
  by = c("Company"),
   suffixes = c("_1", "_2"),
  unique_key_1 = "id_1",
  unique_key_2 = "id_2", 
  multivar_settings = build_multivar_settings(
  compare_type = c( "stringdist"),
  wgts = c(1), nthread = 1, blocks = "Country"
 
))
result$matches

