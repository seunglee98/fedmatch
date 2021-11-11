test_that("clean strings removes replacements", {
  names <- c("abc corp", "abc corporation")
  cleaned <- clean_strings(names, common_words = data.table(word = "corp",
                                                            replacement = "corporation"),
                           remove_words = T)
  expect_identical(cleaned, c("abc", "abc"))
})
