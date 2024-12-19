## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fedmatch)

## -----------------------------------------------------------------------------
name_vec <- corp_data1[, Company]
name_vec

## -----------------------------------------------------------------------------
clean_strings(name_vec)

## -----------------------------------------------------------------------------
print(sp_char_words)

## -----------------------------------------------------------------------------
new_sp_char <- data.table::data.table(character = c("o"), replacement = c("apple"))
clean_strings(name_vec, sp_char_words = new_sp_char)

## -----------------------------------------------------------------------------
print(corporate_words[1:5])

## -----------------------------------------------------------------------------
clean_strings(name_vec, common_words = data.table::data.table(word = c("general", "almart"),
                                                              replacement = c("bananas", "oranges")))


## -----------------------------------------------------------------------------
word_frequency(sample(c("hi", "Hello", "bye    "), 1e4, replace = TRUE))

## -----------------------------------------------------------------------------
clean_strings(name_vec, sp_char_words = new_sp_char, remove_char = c("a", "c"))
clean_strings(name_vec, common_words = data.table::data.table(word = c("general", "company"),
                                                              replacement = c("bananas", "oranges")),
              remove_words = TRUE)

## -----------------------------------------------------------------------------
clean_strings(c( "call", "calling", "called"), stem = TRUE)

