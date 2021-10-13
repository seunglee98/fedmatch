context("Weighted Jaccard Testing")
test_that("single jaccard works", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2

  corpus <- fedmatch:::build_corpus(clean_strings(corp_data1$Company),
                         clean_strings(corp_data2$Name))
  corpus
  result <- wgt_jaccard_distance(clean_strings(corp_data1$Company),
                                 clean_strings(corp_data2$Name),
                                 corpus = corpus)
  expect_true(is.numeric(result$similarity))
})
test_that("single jaccard breaks when non-corpus item passed", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2

  corpus <- fedmatch:::build_corpus(clean_strings(corp_data1$Company),
                                    clean_strings(corp_data2$Name))
  expect_error(wgt_jaccard_distance("abcd",
                                    "efgh",
                                    corpus = corpus))
})
test_that("single jaccard breaks when strings are different lengths", {
  corp_data1 <- fedmatch::corp_data1
  corp_data2 <- fedmatch::corp_data2

  corpus <- fedmatch:::build_corpus(clean_strings(corp_data1$Company),
                                    clean_strings(corp_data2$Name))
  expect_error(wgt_jaccard_distance(c("walmart", "walmart abc"),
                                    c("walmart"),
                                    corpus = corpus))
})
