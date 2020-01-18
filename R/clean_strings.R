#' clean_strings
#'
#' @description default string cleaning process for "name_match"
#' 
#' @param string character or character vector of strings 
#' @param sp_char_words character vector. Data.frame where first column is special characaters and second column is full words. 
#' @param common_words data.frame. Data.frame where first column is abbreviations and second column is full words. 
#' @param remove_char character vector. string of specific characters (for example, "letters") to be removed
#' @param remove_words logical. If TRUE, removes all abbreviations and replacement words in common_words
#' @param stem logical. If TRUE, words are stemmed 
#'
#' @return cleaned strings 
#' 
#' @export
#'
#' @examples
#'# basic cleaning example
#'sample_str = c("Holding Co. A @ B St, 3 ))", "Company B & C 4, Inc.", "make $, inc.")
#'sample_clean1 = clean_strings(sample_str)
#'
#'# defining common words with a package database
#'sample_clean2 = clean_strings(sample_str, common_words=corporate_words)
#'
#'# dropping common words in a database
#'sample_clean3 = clean_strings(sample_str, common_words=corporate_words, remove_words=TRUE)
#'
#'# sunco example
#'sample_clean4 = clean_strings("co cosuncosunco co co", common_words = cbind(c("co"), c("company")))
#'
#'# changing special characters to words(Note that @ and & are dropped with punctuation)
#'drop_char = cbind(c("\\$", "\\%"), c("dollar", "percent"))
#'sample_clean5 = clean_strings(sample_str, sp_char_words = drop_char)
#'
#'
clean_strings <- function(string, sp_char_words=NULL, common_words=NULL, remove_char=NULL, remove_words=FALSE, stem=FALSE, replace_null=NULL){
  
  # define the default inputs;
  
    ## special character to words default
    if (is.null(sp_char_words)){
      sp_char_words = data.frame('character'=c("\\&", "\\$", "\\%", "\\@"), 'replacement'=c("and", "dollar", "percent", "at"))
    } else {
      sp_char_words = data.frame(sp_char_words)
    }
    
    ## if remove_words is TRUE, set replacement for common words to '' instead of using replacement word
    if(!is.null(common_words)){
      if(class(common_words) %in% c('matrix','data.frame')){
        if(remove_words==TRUE){
          # names(sp_char_words) = names(common_words) = c('name','name')
          # common_words = rbind(common_words[1],common_words[2], sp_char_words[1],sp_char_words[2])
          common_words[,2] = '' }
      } else if(class(words)=='character'){
         common_words = data.frame(cbind(common_words,''))
      }
     }
  
  # basic cleaning 1;
  
    ## lowcase the strings
    clean_str = tolower(string)

    ## replace commonly abbreviated words 
    if (!is.null(common_words)){
      
      ### convert to regex  so that only full words are removed
      common_words[1] = apply(common_words, 1, function (x) paste0(paste0("\\<", x[1], sep=""), "\\>", sep="") )
      
      ### replace words
      for (i in 1:dim(common_words)[1]) {
        clean_str = gsub(common_words[,1][i], common_words[,2][i], clean_str) } }
    
    ## replace special characters with words
    for (i in 1:dim(sp_char_words)[1]){
      clean_str = gsub(sp_char_words[,1][i], sp_char_words[,2][i], clean_str) } 
    
    ## replace remaining special characters no spaces 
    clean_str = gsub("[[:punct:]]", "", clean_str) 
    
    ## drop specific characters
    if (!is.null(remove_char)) {
      for (i in remove_char) {
        clean_str = gsub(i, " ", clean_str) } }
  
  # basic cleaning 2; 
  
    ##reduce all spaces to single spaces;
    clean_str = gsub("[[:space:]]", " ", clean_str)
    clean_str = gsub("\\s+", " ", clean_str)
    
    ## remove leading and trailing blanks
    clean_str = gsub(' +$', '', clean_str)
    
  # stemming
  if(stem==TRUE){
    clean_str = SnowballC::wordStem(clean_str, "english" ) }
  
  # replace NULL strings;
  if (!is.null(replace_null)){
      clean_str[which(clean_str %in% c('',' '))]=replace_null }
  
  return(clean_str)
}
