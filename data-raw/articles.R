articles <- data.table(
  article = c(
    "the", "of", "and", "to", "in", "a", "is", "that", "for",
    "it", "as", "with", "by", "on", "not", "or", "from", "at", "an",
    "one", "all", "percent", "dollar"
  ),
  replacement = rep("", 23)
)
usethis::use_data(articles, overwrite = TRUE)
