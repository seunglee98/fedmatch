test_that("single jaccard works", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2

  corpus <- fedmatch:::build_corpus(clean_strings(corp_data1$Company),
                         clean_strings(corp_data2$Name))
  result <- fedmatch:::wgt_jaccard_single(corpus[["word"]],
                                           corpus$inv_freq,
                                           c("apple"),
                                           c("apple exxon"),
                                           nthreads = 1)
  expect_true(is.numeric(result$similarity))
})
