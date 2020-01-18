#' count_rows
#'
#' @description Takes two datasets and a character vector of merge variables, and returns the number of rows of a hypothetical merged dataset, 
#' without actually performing the merge. Useful in cases where merge variables may not be unique, and a merge could result in an R-crashingly large dataset. 
#' 
#' @param x data.frame. First data to merge
#' @param y data.frame. Second data to merge
#' @param merge_ids character vector. Merge variables  
#'
#' @return number of rows of the merged dataset
#' 
#' @details h/t Joris Meys and his Stack Overflow post 
#' \url{http://stackoverflow.com/questions/7441188/how-to-efficiently-merge-two-datasets}
#' 
#' @export
#'
#' @examples
#' count_rows(data.frame('id'=c(1,1,1,2,2,3)), data.frame('id'=c(1,1,2,2,3,4)), 'id')
#' 
#' 
count_rows <- function(x, y, by, by.x=by, by.y=by){
  
  # collapsing id to a single variable, if necessary 
  if( (length(by.x)>1) | (length(by.y)>1) ){
    for(input in c('x','y')){
      temp = get(input)
      merge_ids = get(paste('by',input,sep='.'))
      temp['temp'] = do.call(paste, c(sep='_', temp[merge_ids]))
      assign(input, temp)
      assign(paste('by',input,sep='.'), 'temp')
    }
  }
  
  # creating tables
  table_x = table(x[[by.x]])
  table_y = table(y[[by.y]])

  value = names(table_x)[match(names(table_x),names(table_y),0L) > 0L]
  counts = rbind(table_x[match(value,names(table_x))],table_y[match(value,names(table_y))])
  
  sum(apply(counts,2,prod,na.rm=FALSE),na.rm=TRUE)
}
