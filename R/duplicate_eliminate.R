#' duplicate_eliminate
#'
#' @description Identifies and eliminates all duplicates including the first instance, if they exist 
#' 
#' @param data data.frame 
#' @param id_vars character vector. 
#' @param dupout logical. If FALSE, returns dataset without duplicates. If TRUE, returns duplicates. 
#'
#' @return see dupout
#' @export
#' 
#' @details h/t Simon O'Hanlon and his Stack Overflow post
#' \url{http://stackoverflow.com/questions/17352657/using-r-how-can-i-flag-sequential-duplicate-values-in-a-single-column-of-a-data}
#'
#' @examples
#' data = data.frame('a'=c(1,1,1,1,2,2,2,2,3), 'b'=c(1,1,2,4,1,2,2,3,1))
#' duplicate_eliminate(data,c('a','b'))
#' duplicate_eliminate(data,c('a'))
#' 
#' 
duplicate_eliminate <- function(data, id_vars, dupout=FALSE, tag=NULL) {
  
  # combine id_vars 
  data['temp'] = do.call(paste, c(sep='_', data[id_vars]))
  id_vars = 'temp'
  
  # flag duplicates 
  data = data[order(data[,'temp']),]
  temp = rle(data[,id_vars])
  flag = rep(temp$lengths != 1 , times = temp$lengths )
  
  # output
  if(!is.null(tag)){
    data[,tag] = ifelse(flag==dupout,1,0)  
  } else {
    data = data[which(flag==dupout),]
  }
  data[,'temp'] = NULL
   
  return(data)
}
