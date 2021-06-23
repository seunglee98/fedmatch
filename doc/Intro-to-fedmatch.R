## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = F-------------------------------------------------------
devtools::load_all()

## -----------------------------------------------------------------------------
fedmatch::corp_data1
fedmatch::corp_data2


## -----------------------------------------------------------------------------
basic_merge <- merge_plus(data1 = corp_data1, 
                          data2 = corp_data2,
                          by.x = "Company",
                          by.y = "Name", match_type = "exact", 
                          unique_key_1 = "unique_key_1",
                          unique_key_2 = "unique_key_2")

## -----------------------------------------------------------------------------
print(basic_merge$matches)

## -----------------------------------------------------------------------------
print(basic_merge$matches_filter)

## -----------------------------------------------------------------------------
print(basic_merge$data1_nomatch)
print(basic_merge$data2_nomatch)

## -----------------------------------------------------------------------------
print(basic_merge$match_evaluation)

## -----------------------------------------------------------------------------
fuzzy_result <- merge_plus(data1 = corp_data1, 
                          data2 = corp_data2,
                          by.x = "Company",
                          by.y = "Name", match_type = "fuzzy", 
                          unique_key_1 = "unique_key_1",
                          unique_key_2 = "unique_key_2")
print(fuzzy_result$matches)

## -----------------------------------------------------------------------------
fuzzy_result <- merge_plus(data1 = corp_data1, 
                          data2 = corp_data2,
                          by.x = "Company",
                          by.y = "Name", match_type = "fuzzy", 
                          fuzzy_settings = list(maxDist = .5, method = "jw"),
                          unique_key_1 = "unique_key_1",
                          unique_key_2 = "unique_key_2")
print(fuzzy_result$matches)

## -----------------------------------------------------------------------------
# for simplicity's sake, rename columns in corp_data2
setnames(corp_data2, c("Name", "country"), c("Company", "Country"))
multivar_linear_result <- merge_plus(corp_data1, corp_data2, 
                                     match_type = "multivar",
                                     by = c("Country", "Company"), 
                                     unique_key_1 = "unique_key_1",
                                     suffixes = c("_1", "_2"),
                                     unique_key_2 = "unique_key_2",
                                    
                                     multivar_settings = list(compare_type = c("indicator", "stringdist"),
                                                               wgts = c(.5, .5),
                                                              top = 1))
multivar_linear_result$matches                                     

## -----------------------------------------------------------------------------
set.seed(111)
training_table <- data.table(match = c(rep(1, 5e4), sample(c(0,1 ), 5e4, replace = T)),
                                Company_compare = seq(1, 0.00001, -.00001),
                                Country_compare = c(rep(1, 5e4), sample(c(1, 0), 5e4, replace = T)))
# training_table
logit_model <- glm(match ~ Company_compare + Country_compare, family = "binomial",
                   data = training_table)
summary(logit_model)

## -----------------------------------------------------------------------------
result <- merge_plus(corp_data1, corp_data2, by = c("Country", "Company"), unique_key_1 = "unique_key_1",
                        unique_key_2 = "unique_key_2", 
                     match_type = "multivar",
                     multivar_settings = list(logit = logit_model, compare_type = c("indicator", "stringdist"),
                        wgts = NULL),
                        suffixes = c("_1", "_2"))
result$matches

