## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fedmatch)
library(data.table)

## -----------------------------------------------------------------------------
fuzzy_result <- merge_plus(data1 = corp_data1, 
                          data2 = corp_data2,
                          by.x = "Company",
                          by.y = "Name", match_type = "fuzzy", 
                          unique_key_1 = "unique_key_1",
                          unique_key_2 = "unique_key_2")
print(fuzzy_result$matches)

## -----------------------------------------------------------------------------
dummy_data1 <- data.table(id1 = 1:2,
                            name = "abd")
dummy_data2 <- data.table(id2 = 1,
                            name = "abc")
result1 <- fedmatch::merge_plus(
    data1 = dummy_data1,
    match_type = "fuzzy",
    data2 = dummy_data2, by.x = "name", by.y = "name",
    unique_key_1 = "id1", unique_key_2 = "id2",
    suffixes = c("_1", "_2"), fuzzy_settings = build_fuzzy_settings(maxDist = .5))
print(result1$matches)

## -----------------------------------------------------------------------------
result1 <- fedmatch::merge_plus(
    data1 = dummy_data2,
    match_type = "fuzzy",
    data2 = dummy_data1, by.x = "name", by.y = "name",
    unique_key_1 = "id2", unique_key_2 = "id1",
    suffixes = c("_1", "_2"), fuzzy_settings = build_fuzzy_settings(maxDist = .5))
print(result1$matches)

## -----------------------------------------------------------------------------
wgt_jaccard_match <- merge_plus(data1 = corp_data1, 
                          data2 = corp_data2,
                          by.x = "Company",
                          by.y = "Name", match_type = "fuzzy", 
                          fuzzy_settings = build_fuzzy_settings(method = "wgt_jaccard", nthread = 2,
                                                                maxDist = .5),
                          unique_key_1 = "unique_key_1",
                          unique_key_2 = "unique_key_2")
print(wgt_jaccard_match)

