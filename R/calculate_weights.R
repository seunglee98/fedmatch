#' calculate_weights
#'
#' @description calculate weights for comparison variables based on m and u probabilities estimated from a verified dataset. See \url{https://en.wikipedia.org/wiki/Record_linkage}
#'
#' @param data data.frame. Verified data. Should have all of the variables you want to calculate weights for from both datasets, named the same with data-specific suffixes. 
#' @param variables character vector of the variable names of the variables you want to calculate weights for. 
#' @param compare_type character vector. One of 'stringdist' (for string variables) 'ratio','difference' (for numerics) 'indicator' (0-1 dummy indicating if the two are the same),'in' (0-1 dummy indicating if data1 is IN data2), and 'substr' (numeric indicating how many digits are the same.)
#' @param suffixes character vector. Suffixes of of the variables that indicate what data they are from. Default is same as the default for base R merge, c('.x','.y')
#' @param non_negative logical. Do you want to allow negative weights? 
#'
#' @return list with m probabilities, u probabilites, w weights, and settings, the list argument requried as an input for score_settings in merge_plus using the calculate weights. 
#'
#' @export
#'
#' @examples
#' See match_template.R in examples folder -- end of the file. 
#'
calculate_weights <- function(data, variables, compare_types='stringdist', suffixes=c('.x','.y'), non_negative=FALSE){

  # Preliminaries

    ## checking lengths
    if(length(compare_types)==1){
      compare_types=rep(compare_types, length(variables))
    } else if(length(variables)!= length(compare_types)){
      stop('length of variables not equal to length of compare_types')}

    ## checking to make sure variable names in function 
    for(i in 1:length(variables)){
      for(j in 1:2){
        if(!(paste0(variables[i],suffixes[j]) %in% names(data))){
          stop(paste(paste0(variables[i],suffixes[j]),'not in data'))
         }}}
         
    ## intializing vectors
    m = u = rep(NA, length(variables))


  # Calculating m & u probabilities
  for(i in 1:length(variables)){

    ## getting names for convienience
    variable = variables[i]
    compare_type = compare_types[i]

    ## creating comparison variable
    if(compare_type=='in'        ){data[,variable] = as.numeric(as.character(ifelse(stringr::str_detect(data[,paste0(variable,suffixes[2])],data[,paste0(variable,suffixes[1])]), 1,0)))}
    if(compare_type=='indicator' ){data[,variable] = ifelse((data[,paste0(variable,suffixes[1])]==data[,paste0(variable,suffixes[2])]), 1,0)}
    if(compare_type=='substr'    ){data[,variable] = 0; for(k in 1:max(nchar(data[,paste0(variable,suffixes[1])]),na.rm=TRUE)){data[,variable] = ifelse(substr(data[,paste0(variable,suffixes[1])],1,k)==substr(data[,paste0(variable,suffixes[2])],1,k) & nchar(data[,paste0(variable,suffixes[1])])>=k & nchar(data[,paste0(variable,suffixes[2])])>=k ,k,data[,variable])}}
    if(compare_type=='difference'){data[,variable] = as.numeric(pmin(abs(data[,paste0(variable,suffixes[1])]-data[,paste0(variable,suffixes[2])]),abs(data[,paste0(variable,suffixes[2])]-data[,paste0(variable,suffixes[1])])))}
    if(compare_type=='ratio'     ){data[,variable] = pmax(data[,paste0(variable,suffixes[2])]/data[,paste0(variable,suffixes[1])],data[,paste0(variable,suffixes[1])]/data[,paste0(variable,suffixes[2])] )}
    if(compare_type=='stringdist'){data[,variable] = stringdist::stringdist(data[,paste0(variable,suffixes[1])],data[,paste0(variable,suffixes[2])], method="jw",p=.02)}

    ## normalizing distance variables
    if(compare_type=='in'        ){data[,variable] = data[,variable]}
    if(compare_type=='indicator' ){data[,variable] = data[,variable]}
    if(compare_type=='substr'    ){data[,variable] = data[,variable]/pmin(nchar(data[,paste0(variable,suffixes[1])]), nchar(data[,paste0(variable,suffixes[2])]), na.rm=TRUE)}
    if(compare_type=='difference'){data[,variable] = 1/(1+(1/max(data[,variable], na.rm=TRUE))*data[,variable]^2)}
    if(compare_type=='ratio'     ){data[,variable] = 1/data[,variable]}
    if(compare_type=='stringdist'){data[,variable] = 1-data[,variable]}

    ## calculating match probability
    m[i] = sum(data[,variable], na.rm=TRUE)/length(data[,variable][!is.na(data[,variable])])

    ## calculating unmatch probability

      ### calculating by observation unmatch probabilities for each dataset
      for(suffix in suffixes){
        temp = data.frame(table(as.factor(data[,paste0(variable, suffix)]))/length(data[,paste0(variable, suffix)][which(!is.na(data[,paste0(variable, suffix)]))]))
        data[,paste0(variable, suffix)] = as.factor(data[,paste0(variable, suffix)])
        data = merge(data, temp, by.x=paste0(variable, suffix),by.y='Var1', suffixes=suffixes)
      }

      ### averaging over both datasets 
      u[i] = mean(.5*(data[,paste0('Freq',suffixes[1])]+data[,paste0('Freq',suffixes[2])]), na.rm=TRUE) 
      
      ### deleting 
      data[,paste0('Freq',suffixes[1])] =  data[,paste0('Freq',suffixes[2])] = NULL 
  }
    
  # Calculating weights 
  w = log(m/u, base=2)
  if(non_negative==TRUE){
    w[which(w<0)]=0}
  w = w/sum(w, na.rm=TRUE)
  names(w) = names(m) = names(u) = variables
  
  # Creating settings 
  settings = list()
  for(i in 1:length(variables)){
    settings[[variables[i]]] = list(compare_type=compare_types[i], weight=w[i])}
  
  return(list(m=m, u=u, w=w, settings=settings))
}