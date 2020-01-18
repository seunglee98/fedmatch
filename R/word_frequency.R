#' word_frequency
#'
#' @description A function to count occurences in a set of strings; function does minimal cleaning (remove punctuation and extra spaces);
#' 
#' @param string character vector
#'
#' @return data frame with word frequency 
#' @export
#'
#' @examples
#'# with simple vector
#'names = c("company name 1", "company name 2", "company 3")
#'word_frequency(names)
#'
#'# with a dataframe
#'col1 = c(1,2,3)
#'df1 = cbind(col1, names)
#'word_frequency(df1[,2])
#'
#'# more complicated example
#'names = c("company name 1", "company &    3 inc.", "co nm 1,, lp")
#'df2 = cbind(col1, names)
#'word_frequency(df2[,2])
#'
#'
word_frequency <- function(string){
  
  # remove multiple spaces and all punctuation
  clean_str = gsub("[[:punct:]]", " ", tolower(string)) 
  clean_str = gsub("[[:space:]]", " ", clean_str)
  clean_str = gsub("\\s+", " ", clean_str)
  
  # create an empty vector
  word_list = c()
  
  # add all the words from each string
  for (i in 1:length(clean_str)){
    words = strsplit(clean_str[i], "\\s+")
    word_list = c(word_list, words, recursive=TRUE)
  }
  
  # count the frequency of the words
  word_freq = data.frame(table(word_list)) # count words
  word_freq = word_freq[order(-word_freq$Freq),] # order by word count
  names(word_freq) = c("word", "count")
  return(word_freq)
}
